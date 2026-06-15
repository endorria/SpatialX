{-|
Module      : PostgREST.Request.ApiRequest
Description : PostgREST functions to translate HTTP request to a domain type called ApiRequest.
-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module PostgREST.ApiRequest
  ( ApiRequest(..)
  , userApiRequest
  , userPreferences
  , userBearerAuth
  ) where

import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict  as HM
import qualified Data.List.NonEmpty   as NonEmptyList
import qualified Data.Set             as S
import qualified Data.Text.Encoding   as T

import Data.List                       (lookup)
import Data.Ranged.Ranges              (emptyRange, rangeIntersection,
                                        rangeIsEmpty)
import Network.HTTP.Types.Header       (RequestHeaders,
                                        hAuthorization, hCookie)
import Network.Wai                     (Request (..))
import Network.Wai.Middleware.HttpAuth (extractBearerAuth)
import Network.Wai.Parse               (parseHttpAccept)
import Web.Cookie                      (parseCookies)

import PostgREST.ApiRequest.Payload      (getPayload)
import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.ApiRequest.Types        (Action (..), DbAction (..),
                                          InvokeMethod (..),
                                          Mutation (..), Payload (..),
                                          RequestBody, Resource (..))
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.Database         (TimezoneNames)
import PostgREST.Error                   (ApiRequestError (..),
                                          RangeError (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.RangeQuery              (NonnegRange, allRange,
                                          convertToLimitZeroRange,
                                          hasLimitZero,
                                          rangeRequested)
import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)

import qualified PostgREST.ApiRequest.Preferences as Preferences
import qualified PostgREST.ApiRequest.QueryParams as QueryParams
import qualified PostgREST.MediaType              as MediaType

import Protolude

data ApiRequest = ApiRequest {
    iAction              :: Action
  , iRange               :: HM.HashMap Text NonnegRange
  , iTopLevelRange       :: NonnegRange
  , iPayload             :: Maybe Payload
  , iPreferences         :: Preferences.Preferences
  , iQueryParams         :: QueryParams.QueryParams
  , iColumns             :: S.Set FieldName
  , iHeaders             :: [(ByteString, ByteString)]
  , iCookies             :: [(ByteString, ByteString)]
  , iPath                :: ByteString
  , iMethod              :: ByteString
  , iSchema              :: Schema
  , iNegotiatedByProfile :: Bool
  , iAcceptMediaType     :: [MediaType]
  , iContentMediaType    :: MediaType
  }

userApiRequest :: AppConfig -> Preferences.Preferences -> Request -> RequestBody -> Either ApiRequestError ApiRequest
userApiRequest conf prefs req reqBody = do
  resource <- getResource conf $ pathInfo req
  (schema, negotiatedByProfile) <- getSchema conf hdrs method
  act <- getAction resource schema method
  qPrms <- first QueryParamError $ QueryParams.parse (actIsInvokeSafe act) $ rawQueryString req
  (topLevelRange, ranges) <- getRanges method qPrms hdrs
  (payload, columns) <- getPayload reqBody contentMediaType qPrms act
  return $ ApiRequest {
    iAction = act
  , iRange = ranges
  , iTopLevelRange = topLevelRange
  , iPayload = payload
  , iPreferences = prefs
  , iQueryParams = qPrms
  , iColumns = columns
  , iHeaders = iHdrs
  , iCookies = iCkies
  , iPath = rawPathInfo req
  , iMethod = method
  , iSchema = schema
  , iNegotiatedByProfile = negotiatedByProfile
  , iAcceptMediaType = maybe [MTAny] (map MediaType.decodeMediaType . parseHttpAccept) $ lookupHeader "accept"
  , iContentMediaType = contentMediaType
  }
  where
    method = requestMethod req
    hdrs = requestHeaders req
    lookupHeader    = flip lookup hdrs
    iHdrs = [ (CI.foldedCase k, v) | (k,v) <- hdrs, k /= hCookie]
    iCkies = maybe [] parseCookies $ lookupHeader "Cookie"
    contentMediaType = maybe MTApplicationJSON MediaType.decodeMediaType $ lookupHeader "content-type"
    actIsInvokeSafe x = case x of {ActDb (ActRoutine _  (InvRead _)) -> True; _ -> False}

userPreferences :: AppConfig -> Request -> TimezoneNames -> Preferences.Preferences
userPreferences conf req timezones = Preferences.fromHeaders (configDbTxAllowOverride conf) timezones $ requestHeaders req

userBearerAuth :: Request -> Maybe ByteString
userBearerAuth req = extractBearerAuth =<< lookup hAuthorization (requestHeaders req)

getResource :: AppConfig -> [Text] -> Either ApiRequestError Resource
getResource AppConfig{configOpenApiMode, configDbRootSpec, configOgcApiEnabled} = \case
  []
    | configOgcApiEnabled -> Right ResourceOgcLanding
    | otherwise ->
        case (configOpenApiMode,configDbRootSpec) of
          (OADisabled,_) -> Left OpenAPIDisabled
          (_, Just qi)   -> Right $ ResourceRoutine (qiName qi)
          (_, Nothing)   -> Right ResourceSchema
  ["conformance"]
    | configOgcApiEnabled -> Right ResourceOgcConformance
  ["collections"]
    | configOgcApiEnabled -> Right ResourceOgcCollections
  ["collections", collectionId]
    | configOgcApiEnabled -> Right $ ResourceOgcCollection collectionId
  ["collections", collectionId, "items"]
    | configOgcApiEnabled -> Right $ ResourceOgcCollectionItems collectionId
  ["collections", collectionId, "items", featureId]
    | configOgcApiEnabled -> Right $ ResourceOgcCollectionItem collectionId featureId
  [table]        -> Right $ ResourceRelation table
  ["rpc", pName] -> Right $ ResourceRoutine pName
  _              -> Left InvalidResourcePath

getAction :: Resource -> Schema -> ByteString -> Either ApiRequestError Action
getAction resource schema method =
  case (resource, method) of
    (ResourceRoutine rout, "HEAD")    -> Right . ActDb $ ActRoutine (qi rout) $ InvRead True
    (ResourceRoutine rout, "GET")     -> Right . ActDb $ ActRoutine (qi rout) $ InvRead False
    (ResourceRoutine rout, "POST")    -> Right . ActDb $ ActRoutine (qi rout) Inv
    (ResourceRoutine rout, "OPTIONS") -> Right $ ActRoutineInfo (qi rout) $ InvRead True
    (ResourceRoutine _, _)            -> Left $ InvalidRpcMethod method

    (ResourceRelation rel, "HEAD")    -> Right . ActDb $ ActRelationRead (qi rel) True
    (ResourceRelation rel, "GET")     -> Right . ActDb $ ActRelationRead (qi rel) False
    (ResourceRelation rel, "POST")    -> Right . ActDb $ ActRelationMut  (qi rel) MutationCreate
    (ResourceRelation rel, "PUT")     -> Right . ActDb $ ActRelationMut  (qi rel) MutationSingleUpsert
    (ResourceRelation rel, "PATCH")   -> Right . ActDb $ ActRelationMut  (qi rel) MutationUpdate
    (ResourceRelation rel, "DELETE")  -> Right . ActDb $ ActRelationMut  (qi rel) MutationDelete
    (ResourceRelation rel, "OPTIONS") -> Right $ ActRelationInfo (qi rel)

    (ResourceSchema, "HEAD")          -> Right . ActDb $ ActSchemaRead schema True
    (ResourceSchema, "GET")           -> Right . ActDb $ ActSchemaRead schema False
    (ResourceSchema, "OPTIONS")       -> Right ActSchemaInfo

    (ResourceOgcLanding, "HEAD")      -> Right ActOgcLanding
    (ResourceOgcLanding, "GET")       -> Right ActOgcLanding
    (ResourceOgcLanding, "OPTIONS")   -> Right ActOgcCollectionsInfo

    (ResourceOgcConformance, "HEAD")    -> Right ActOgcConformance
    (ResourceOgcConformance, "GET")     -> Right ActOgcConformance
    (ResourceOgcConformance, "OPTIONS") -> Right ActOgcCollectionsInfo

    (ResourceOgcCollections, "HEAD")    -> Right ActOgcCollections
    (ResourceOgcCollections, "GET")     -> Right ActOgcCollections
    (ResourceOgcCollections, "OPTIONS") -> Right ActOgcCollectionsInfo

    (ResourceOgcCollection collectionId, "HEAD")    -> Right $ ActOgcCollection (qi collectionId)
    (ResourceOgcCollection collectionId, "GET")     -> Right $ ActOgcCollection (qi collectionId)
    (ResourceOgcCollection _, "OPTIONS")            -> Right ActOgcCollectionsInfo

    (ResourceOgcCollectionItems collectionId, "HEAD")    -> Right $ ActOgcCollectionItems (qi collectionId) True
    (ResourceOgcCollectionItems collectionId, "GET")     -> Right $ ActOgcCollectionItems (qi collectionId) False
    (ResourceOgcCollectionItems collectionId, "OPTIONS") -> Right $ ActOgcCollectionItemsInfo (qi collectionId)

    (ResourceOgcCollectionItem collectionId featureId, "HEAD") -> Right $ ActOgcCollectionItem (qi collectionId) featureId True
    (ResourceOgcCollectionItem collectionId featureId, "GET")  -> Right $ ActOgcCollectionItem (qi collectionId) featureId False
    (ResourceOgcCollectionItem collectionId _, "OPTIONS")      -> Right $ ActOgcCollectionItemsInfo (qi collectionId)

    _                                 -> Left $ UnsupportedMethod method
  where
    qi = QualifiedIdentifier schema

getSchema :: AppConfig -> RequestHeaders -> ByteString -> Either ApiRequestError (Schema, Bool)
getSchema AppConfig{configDbSchemas} hdrs method = do
  case profile of
    Just p | p `notElem` configDbSchemas -> Left $ UnacceptableSchema p $ toList configDbSchemas
           | otherwise                   -> Right (p, True)
    Nothing -> Right (defaultSchema, length configDbSchemas /= 1)
  where
    defaultSchema = NonEmptyList.head configDbSchemas
    profile = case method of
      "DELETE" -> contentProfile
      "PATCH"  -> contentProfile
      "POST"   -> contentProfile
      "PUT"    -> contentProfile
      _        -> acceptProfile
    contentProfile = T.decodeUtf8 <$> lookupHeader "Content-Profile"
    acceptProfile = T.decodeUtf8 <$> lookupHeader "Accept-Profile"
    lookupHeader    = flip lookup hdrs

getRanges :: ByteString -> QueryParams -> RequestHeaders -> Either ApiRequestError (NonnegRange, HM.HashMap Text NonnegRange)
getRanges method QueryParams{qsRanges} hdrs
  | isInvalidRange = Left $ InvalidRange (if rangeIsEmpty headerRange then LowerGTUpper else NegativeLimit)
  | method == "PUT" && topLevelRange /= allRange = Left PutLimitNotAllowedError
  | otherwise = Right (topLevelRange, ranges)
  where
    headerRange = if method == "GET" then rangeRequested hdrs else allRange
    limitRange = fromMaybe allRange (HM.lookup "limit" qsRanges)
    headerAndLimitRange = rangeIntersection headerRange limitRange
    ranges = HM.insert "limit" (convertToLimitZeroRange limitRange headerAndLimitRange) qsRanges
    isInvalidRange = topLevelRange == emptyRange && not (hasLimitZero limitRange)
    topLevelRange = fromMaybe allRange $ HM.lookup "limit" ranges
