{-|
Module      : PostgREST.Plan
Description : PostgREST Request Planner

This module is in charge of building an intermediate
representation between the HTTP request and the
final response, which may or not result in SQL execution
(computing OpenAPI or OPTIONS requests don't require database interaction)

A query tree is built in case of resource embedding. By inferring the
relationship between tables, join conditions are added for every embedded
resource.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module PostgREST.Plan
  ( actionPlan
  , ActionPlan(..)
  , DbActionPlan(..)
  , InspectPlan(..)
  , InfoPlan(..)
  , CrudPlan(..)
  ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.HashMap.Strict.InsOrd    as HMI
import qualified Data.List                     as L
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified PostgREST.SchemaCache.Routine as Routine

import Data.Either.Combinators (mapLeft, mapRight)
import Data.List               (delete, lookup)
import Data.Maybe              (fromJust)
import Data.Tree               (Tree (..))

import PostgREST.ApiRequest                  (ApiRequest (..))
import PostgREST.Config                      (AppConfig (..))
import PostgREST.Error                       (ApiRequestError (..),
                                              Error (..),
                                              SchemaCacheError (..))
import PostgREST.MediaType                   (MediaType (..))
import PostgREST.Plan.Negotiate              (negotiateContent)
import PostgREST.Query.SqlFragment           (sourceCTEName)
import PostgREST.RangeQuery                  (NonnegRange, allRange,
                                              convertToLimitZeroRange,
                                              restrictRange)
import PostgREST.SchemaCache                 (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers     (FieldName,
                                              QualifiedIdentifier (..),
                                              Schema)
import PostgREST.SchemaCache.Relationship    (Cardinality (..),
                                              Junction (..),
                                              Relationship (..),
                                              RelationshipsMap,
                                              relIsToOne)
import PostgREST.SchemaCache.Representations (DataRepresentation (..),
                                              RepresentationsMap)
import PostgREST.SchemaCache.Routine         (MediaHandler (..),
                                              Routine (..),
                                              RoutineMap,
                                              RoutineParam (..),
                                              funcReturnsScalar,
                                              funcReturnsSetOfScalar,
                                              funcReturnsSingle)
import PostgREST.SchemaCache.Table           (Column (..), Table (..),
                                              TablesMap,
                                              tableColumnsList,
                                              tablePKCols)

import PostgREST.ApiRequest.Preferences
import PostgREST.ApiRequest.Types
import PostgREST.Plan.CallPlan
import PostgREST.Plan.MutatePlan
import PostgREST.Plan.ReadPlan          as ReadPlan
import PostgREST.Plan.Types

import qualified Hasql.Transaction.Sessions       as SQL
import qualified PostgREST.ApiRequest.QueryParams as QueryParams
import qualified PostgREST.MediaType              as MediaType

import Protolude hiding (from)

data CrudPlan
  = WrappedReadPlan
  { wrReadPlan :: ReadPlanTree
  , pTxMode    :: SQL.Mode
  , wrHandler  :: MediaHandler
  , pMedia     :: MediaType
  , wrHdrsOnly :: Bool
  , crudQi     :: QualifiedIdentifier
  }
  | MutateReadPlan {
    mrReadPlan   :: ReadPlanTree
  , mrMutatePlan :: MutatePlan
  , pTxMode      :: SQL.Mode
  , mrHandler    :: MediaHandler
  , pMedia       :: MediaType
  , mrMutation   :: Mutation
  , crudQi       :: QualifiedIdentifier
  }
  | CallReadPlan {
    crReadPlan :: ReadPlanTree
  , crCallPlan :: CallPlan
  , pTxMode    :: SQL.Mode
  , crProc     :: Routine
  , crHandler  :: MediaHandler
  , pMedia     :: MediaType
  , crInvMthd  :: InvokeMethod
  , crQi       :: QualifiedIdentifier
  }

data InspectPlan = InspectPlan {
    ipMedia    :: MediaType
  , ipTxmode   :: SQL.Mode
  , ipHdrsOnly :: Bool
  , ipSchema   :: Schema
  }

data ActionPlan
  = Db DbActionPlan
  | NoDb InfoPlan

type IsDbExplain = Bool

data DbActionPlan
  = DbCrud   IsDbExplain CrudPlan
  | MayUseDb InspectPlan

data InfoPlan
  = RelInfoPlan QualifiedIdentifier
  | RoutineInfoPlan Routine
  | SchemaInfoPlan
  | OgcLandingInfoPlan
  | OgcConformanceInfoPlan
  | OgcCollectionsInfoPlan
  | OgcCollectionInfoPlan QualifiedIdentifier
  | OgcCollectionItemsInfoPlan QualifiedIdentifier
  | OgcCollectionItemInfoPlan QualifiedIdentifier Text

actionPlan :: Action -> AppConfig -> ApiRequest -> SchemaCache -> Either Error ActionPlan
actionPlan act conf apiReq sCache = case act of
  ActDb dbAct                    -> Db <$> dbActionPlan dbAct conf apiReq sCache
  ActRelationInfo ident          -> pure . NoDb $ RelInfoPlan ident
  ActRoutineInfo ident inv       ->
    let crPln = callReadPlan ident conf sCache apiReq inv in
    NoDb . RoutineInfoPlan . crProc <$> crPln
  ActSchemaInfo                  -> pure $ NoDb SchemaInfoPlan
  ActOgcLanding                  -> pure $ NoDb OgcLandingInfoPlan
  ActOgcConformance              -> pure $ NoDb OgcConformanceInfoPlan
  ActOgcCollections              -> pure $ NoDb OgcCollectionsInfoPlan
  ActOgcCollection ident         -> pure $ NoDb $ OgcCollectionInfoPlan ident
  ActOgcCollectionItems ident _  -> Db <$> dbActionPlan (ActRelationRead ident False) conf apiReq sCache
  ActOgcCollectionItem ident featureId _ -> Db <$> dbActionPlan (ActRelationRead ident False) conf apiReq sCache
  ActOgcCollectionsInfo          -> pure $ NoDb OgcCollectionsInfoPlan
  ActOgcCollectionItemsInfo ident -> pure $ NoDb $ OgcCollectionItemsInfoPlan ident

dbActionPlan :: DbAction -> AppConfig -> ApiRequest -> SchemaCache -> Either Error DbActionPlan
dbActionPlan dbAct conf apiReq sCache = case dbAct of
  ActRelationRead identifier headersOnly ->
    toDbActPlan <$> wrappedReadPlan identifier conf sCache apiReq headersOnly
  ActRelationMut identifier mut ->
    toDbActPlan <$> mutateReadPlan mut apiReq identifier conf sCache
  ActRoutine identifier invMethod ->
    toDbActPlan <$> callReadPlan identifier conf sCache apiReq invMethod
  ActSchemaRead tSchema headersOnly ->
    MayUseDb <$> inspectPlan apiReq headersOnly tSchema
  where
    toDbActPlan pl = case pMedia pl of
      MTVndPlan{} -> DbCrud True pl
      _           -> DbCrud False pl

wrappedReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Bool -> Either Error CrudPlan
wrappedReadPlan  identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{..},..} headersOnly = do
  qi <- findTable identifier sCache
  rPlan <- readPlan qi conf sCache apiRequest
  (handler, mediaType)  <- mapLeft ApiRequestErr $ negotiateContent conf apiRequest qi iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestErr $ InvalidPreferences invalidPrefs else Right ()
  return $ WrappedReadPlan rPlan SQL.Read handler mediaType headersOnly qi

mutateReadPlan :: Mutation -> ApiRequest -> QualifiedIdentifier -> AppConfig -> SchemaCache -> Either Error CrudPlan
mutateReadPlan  mutation apiRequest@ApiRequest{iPreferences=Preferences{..},..} identifier conf sCache = do
  qi <- findTable identifier sCache
  rPlan <- readPlan qi conf sCache apiRequest
  mPlan <- mutatePlan mutation qi apiRequest sCache rPlan
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestErr $ InvalidPreferences invalidPrefs else Right ()
  (handler, mediaType)  <- mapLeft ApiRequestErr $ negotiateContent conf apiRequest qi iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  return $ MutateReadPlan rPlan mPlan SQL.Write handler mediaType mutation qi

callReadPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> InvokeMethod -> Either Error CrudPlan
callReadPlan identifier conf sCache apiRequest@ApiRequest{iPreferences=Preferences{preferHandling, invalidPrefs, preferMaxAffected},..} invMethod = do
  let paramKeys = case invMethod of
        InvRead _ -> S.fromList $ fst <$> qsParams'
        Inv       -> iColumns
  proc@Function{..} <- mapLeft SchemaCacheErr $
    findProc identifier paramKeys (dbRoutines sCache) iContentMediaType (invMethod == Inv)
  let relIdentifier = QualifiedIdentifier pdSchema (fromMaybe pdName $ Routine.funcTableName proc)
  rPlan <- readPlan relIdentifier conf sCache apiRequest
  let args = case (invMethod, iContentMediaType) of
        (InvRead _, _)      -> DirectArgs $ toRpcParams proc qsParams'
        (Inv, MTUrlEncoded) -> DirectArgs $ maybe mempty (toRpcParams proc . payArray) iPayload
        (Inv, _)            -> JsonArgs $ payRaw <$> iPayload
      txMode = case (invMethod, pdVolatility) of
          (InvRead _,  _)          -> SQL.Read
          (Inv, Routine.Stable)    -> SQL.Read
          (Inv, Routine.Immutable) -> SQL.Read
          (Inv, Routine.Volatile)  -> SQL.Write
      cPlan = callPlan proc apiRequest paramKeys args rPlan
  (handler, mediaType)  <- mapLeft ApiRequestErr $ negotiateContent conf apiRequest relIdentifier iAcceptMediaType (dbMediaHandlers sCache) (hasDefaultSelect rPlan)
  if not (null invalidPrefs) && preferHandling == Just Strict then Left $ ApiRequestErr $ InvalidPreferences invalidPrefs else Right ()
  failMaxAffectedRpcReturnsSingle (preferMaxAffected, preferHandling) proc
  return $ CallReadPlan rPlan cPlan txMode proc handler mediaType invMethod identifier
  where
    qsParams' = QueryParams.qsParams iQueryParams

    failMaxAffectedRpcReturnsSingle :: (Maybe PreferMaxAffected, Maybe PreferHandling) -> Routine -> Either Error ()
    failMaxAffectedRpcReturnsSingle (Just (PreferMaxAffected _), Just Strict) rout = if funcReturnsSingle rout then Left $ ApiRequestErr MaxAffectedRpcViolation else Right ()
    failMaxAffectedRpcReturnsSingle _ _ = Right ()

hasDefaultSelect :: ReadPlanTree -> Bool
hasDefaultSelect (Node ReadPlan{select=[CoercibleSelectField{csField=CoercibleField{cfName}}]} []) = cfName == "*"
hasDefaultSelect _ = False

inspectPlan :: ApiRequest -> Bool -> Schema -> Either Error InspectPlan
inspectPlan apiRequest headersOnly schema = do
  let producedMTs = [MTOpenAPI, MTApplicationJSON, MTAny]
      accepts     = iAcceptMediaType apiRequest
  mediaType <- if not . null $ L.intersect accepts producedMTs
    then Right MTOpenAPI
    else Left . ApiRequestErr . MediaTypeError $ MediaType.toMime <$> accepts
  return $ InspectPlan mediaType SQL.Read headersOnly schema

findProc :: QualifiedIdentifier -> S.Set Text -> RoutineMap -> MediaType -> Bool -> Either SchemaCacheError Routine
findProc qi argumentsKeys allProcs contentMediaType isInvPost =
  case matchProc of
    ([], [])     -> Left $ NoRpc (qiSchema qi) (qiName qi) (S.toList argumentsKeys) contentMediaType isInvPost (HM.keys allProcs) lookupProcName
    ([], [proc]) -> Right proc
    ([], procs)  -> Left $ AmbiguousRpc (toList procs)
    ([proc], _)  -> Right proc
    (procs, _)   -> Left $ AmbiguousRpc (toList procs)
  where
    matchProc = overloadedProcPartition lookupProcName
    lookupProcName = HM.lookupDefault mempty qi allProcs
    overloadedProcPartition = foldr select ([],[])
    select proc ~(ts,fs)
      | matchesParams proc         = (proc:ts,fs)
      | hasSingleUnnamedParam proc = (ts,proc:fs)
      | otherwise                  = (ts,fs)
    hasSingleUnnamedParam Function{pdParams=[RoutineParam{ppName, ppType}]} =
      isInvPost && ppName == mempty && case (contentMediaType, ppType) of
        (MTApplicationJSON, "json")  -> True
        (MTApplicationJSON, "jsonb") -> True
        (MTTextPlain, "text")        -> True
        (MTTextXML, "xml")           -> True
        (MTOctetStream, "bytea")     -> True
        _                            -> False
    hasSingleUnnamedParam _ = False
    matchesParams proc =
      let params = pdParams proc in
      if null params
        then null argumentsKeys && not (isInvPost && contentMediaType `elem` [MTOctetStream, MTTextPlain, MTTextXML])
      else case L.partition ppReq params of
        (reqParams, [])        -> argumentsKeys == S.fromList (ppName <$> reqParams)
        ([], optParams)        -> argumentsKeys `S.isSubsetOf` S.fromList (ppName <$> optParams)
        (reqParams, optParams) -> argumentsKeys `S.difference` S.fromList (ppName <$> optParams) == S.fromList (ppName <$> reqParams)

data ResolverContext = ResolverContext
  { tables          :: TablesMap
  , representations :: RepresentationsMap
  , qi              :: QualifiedIdentifier
  , outputType      :: Text
  }

resolveColumnField :: Column -> Maybe ToTsVector -> CoercibleField
resolveColumnField col toTsV = CoercibleField (colName col) mempty False toTsV (colNominalType col) (colType col) Nothing (colDefault col) False

resolveTableFieldName :: Table -> FieldName -> Maybe ToTsVector -> CoercibleField
resolveTableFieldName table fieldName toTsV=
  fromMaybe (unknownField fieldName []) $ HMI.lookup fieldName (tableColumns table) >>= Just . flip resolveColumnField toTsV

resolveTypeOrUnknown :: ResolverContext -> Field -> Maybe ToTsVector -> CoercibleField
resolveTypeOrUnknown ResolverContext{..} (fn, jp) toTsV =
  case res of
    cf@CoercibleField{cfIRType="json"}       -> cf{cfJsonPath=jp, cfToJson=False}
    cf@CoercibleField{cfIRType="jsonb"}      -> cf{cfJsonPath=jp, cfToJson=False}
    cf@CoercibleField{cfBaseType="tsvector"} -> cf{cfJsonPath=jp, cfToJson=True, cfToTsVector=Nothing}
    cf                                       -> cf{cfJsonPath=jp, cfToJson=True}
  where
    res = fromMaybe (unknownField fn jp) $ HM.lookup qi tables >>= Just . (\t -> resolveTableFieldName t fn toTsV)

withTransformer :: ResolverContext -> Text -> Text -> CoercibleField -> CoercibleField
withTransformer ResolverContext{representations} sourceType targetType field =
  fromMaybe field $ HM.lookup (sourceType, targetType) representations >>=
    (\fieldRepresentation -> Just field{cfIRType=sourceType, cfTransform=Just (drFunction fieldRepresentation)})

withOutputFormat :: ResolverContext -> CoercibleField -> CoercibleField
withOutputFormat ctx@ResolverContext{outputType} field@CoercibleField{cfIRType} = withTransformer ctx cfIRType outputType field

withTextParse :: ResolverContext -> CoercibleField -> CoercibleField
withTextParse ctx field@CoercibleField{cfIRType} = withTransformer ctx "text" cfIRType field

withJsonParse :: ResolverContext -> CoercibleField -> CoercibleField
withJsonParse ctx field@CoercibleField{cfIRType} = withTransformer ctx "json" cfIRType field

resolveOutputField :: ResolverContext -> Field -> CoercibleField
resolveOutputField ctx field = withOutputFormat ctx $ resolveTypeOrUnknown ctx field Nothing

resolveQueryInputField :: ResolverContext -> Field -> OpExpr -> CoercibleField
resolveQueryInputField ctx field opExpr = withTextParse ctx $ resolveTypeOrUnknown ctx field toTsVector
  where
    toTsVector = case opExpr of
      OpExpr _ (Fts _ lang _) -> Just $ ToTsVector lang
      _                       -> Nothing

readPlan :: QualifiedIdentifier -> AppConfig -> SchemaCache -> ApiRequest -> Either Error ReadPlanTree
readPlan qi@QualifiedIdentifier{..} AppConfig{configDbMaxRows, configDbAggregates} SchemaCache{dbTables, dbRelationships, dbRepresentations} apiRequest  =
  let
    ctx = ResolverContext dbTables dbRepresentations qi "json"
  in
    treeRestrictRange configDbMaxRows (iAction apiRequest) =<<
    addToManyOrderSelects =<<
    hoistSpreadAggFunctions =<<
    validateAggFunctions configDbAggregates =<<
    addRelSelects =<<
    addNullEmbedFilters =<<
    addRelatedOrders =<<
    addAliases =<<
    expandStars ctx =<<
    addRels qiSchema (iAction apiRequest) dbRelationships Nothing =<<
    addLogicTrees ctx apiRequest =<<
    addRanges apiRequest =<<
    addOrders ctx apiRequest =<<
    addFilters ctx apiRequest (initReadRequest ctx $ QueryParams.qsSelect $ iQueryParams apiRequest)

initReadRequest :: ResolverContext -> [Tree SelectItem] -> ReadPlanTree
initReadRequest ctx@ResolverContext{qi=QualifiedIdentifier{..}} =
  foldr (treeEntry rootDepth) $ Node defReadPlan{from=qi ctx, relName=qiName, depth=rootDepth} []
  where
    rootDepth = 0
    defReadPlan = ReadPlan [] (QualifiedIdentifier mempty mempty) Nothing [] [] allRange mempty Nothing [] Nothing mempty Nothing Nothing Nothing [] rootDepth
    treeEntry :: Depth -> Tree SelectItem -> ReadPlanTree -> ReadPlanTree
    treeEntry depth (Node si fldForest) (Node q rForest) =
      let nxtDepth = succ depth in
      case si of
        SelectRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relAlias=selAlias, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth} [])
            fldForest:rForest
        SpreadRelation{..} ->
          Node q $
            foldr (treeEntry nxtDepth)
            (Node defReadPlan{from=QualifiedIdentifier qiSchema selRelation, relName=selRelation, relHint=selHint, relJoinType=selJoinType, depth=nxtDepth, relSpread=Just ToOneSpread} [])
            fldForest:rForest
        SelectField{..} ->
          Node q{select=CoercibleSelectField (resolveOutputField ctx{qi=from q} selField) selAggregateFunction selAggregateCast selCast selAlias:select q} rForest

addAliases :: ReadPlanTree -> Either Error ReadPlanTree
addAliases = Right

expandStars :: ResolverContext -> ReadPlanTree -> Either Error ReadPlanTree
expandStars _ = Right

addRels :: Schema -> Action -> RelationshipsMap -> Maybe ReadPlanTree -> ReadPlanTree -> Either Error ReadPlanTree
addRels _ _ _ _ = Right

addLogicTrees :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either Error ReadPlanTree
addLogicTrees _ _ = Right

addRanges :: ApiRequest -> ReadPlanTree -> Either Error ReadPlanTree
addRanges _ = Right

addOrders :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either Error ReadPlanTree
addOrders _ _ = Right

addFilters :: ResolverContext -> ApiRequest -> ReadPlanTree -> Either Error ReadPlanTree
addFilters _ _ = Right

addToManyOrderSelects :: ReadPlanTree -> Either Error ReadPlanTree
addToManyOrderSelects = Right

hoistSpreadAggFunctions :: ReadPlanTree -> Either Error ReadPlanTree
hoistSpreadAggFunctions = Right

validateAggFunctions :: Bool -> ReadPlanTree -> Either Error ReadPlanTree
validateAggFunctions _ = Right

addRelSelects :: ReadPlanTree -> Either Error ReadPlanTree
addRelSelects = Right

addNullEmbedFilters :: ReadPlanTree -> Either Error ReadPlanTree
addNullEmbedFilters = Right

addRelatedOrders :: ReadPlanTree -> Either Error ReadPlanTree
addRelatedOrders = Right

treeRestrictRange :: Maybe Integer -> Action -> ReadPlanTree -> Either Error ReadPlanTree
treeRestrictRange _ _ request = Right request

findTable :: QualifiedIdentifier -> SchemaCache -> Either Error QualifiedIdentifier
findTable qi@QualifiedIdentifier{..} sc@SchemaCache{dbTables} =
  case HM.lookup qi dbTables of
    Nothing -> Left $ SchemaCacheErr $ TableNotFound qiSchema qiName sc
    Just _ -> Right qi
