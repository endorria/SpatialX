{-# LANGUAGE DuplicateRecordFields #-}
module PostgREST.ApiRequest.Types
  ( AggregateFunction(..)
  , Alias
  , Cast
  , Depth
  , EmbedParam(..)
  , EmbedPath
  , Field
  , Filter(..)
  , Hint
  , JoinType(..)
  , JsonOperand(..)
  , JsonOperation(..)
  , JsonPath
  , Language
  , ListVal
  , LogicOperator(..)
  , LogicTree(..)
  , NodeName
  , OpExpr(..)
  , Operation (..)
  , OpQuantifier(..)
  , OrderDirection(..)
  , OrderNulls(..)
  , OrderTerm(..)
  , SingleVal
  , IsVal(..)
  , SimpleOperator(..)
  , QuantOperator(..)
  , FtsOperator(..)
  , SelectItem(..)
  , Payload (..)
  , InvokeMethod (..)
  , Mutation (..)
  , Resource (..)
  , DbAction (..)
  , Action (..)
  , RequestBody
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as S

import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema)

import Protolude

data InvokeMethod = Inv | InvRead Bool
  deriving Eq

data Mutation
  = MutationCreate
  | MutationDelete
  | MutationSingleUpsert
  | MutationUpdate
  deriving Eq

data Resource
  = ResourceRelation Text
  | ResourceRoutine Text
  | ResourceSchema
  | ResourceOgcLanding
  | ResourceOgcConformance
  | ResourceOgcCollections
  | ResourceOgcCollection Text
  | ResourceOgcCollectionItems Text
  | ResourceOgcCollectionItem Text Text

data DbAction
  = ActRelationRead {dbActQi :: QualifiedIdentifier, actHeadersOnly :: Bool}
  | ActRelationMut  {dbActQi :: QualifiedIdentifier, actMutation :: Mutation}
  | ActRoutine      {dbActQi :: QualifiedIdentifier, actInvMethod :: InvokeMethod}
  | ActSchemaRead   Schema Bool

data Action
  = ActDb           DbAction
  | ActRelationInfo QualifiedIdentifier
  | ActRoutineInfo  QualifiedIdentifier InvokeMethod
  | ActSchemaInfo
  | ActOgcLanding
  | ActOgcConformance
  | ActOgcCollections
  | ActOgcCollection QualifiedIdentifier
  | ActOgcCollectionItems QualifiedIdentifier Bool
  | ActOgcCollectionItem QualifiedIdentifier Text Bool
  | ActOgcCollectionsInfo
  | ActOgcCollectionItemsInfo QualifiedIdentifier

type RequestBody = LBS.ByteString

data Payload
  = ProcessedJSON
      { payRaw  :: LBS.ByteString
      , payKeys :: S.Set Text
      }
  | ProcessedUrlEncoded { payArray  :: [(Text, Text)], payKeys :: S.Set Text }
  | RawJSON { payRaw  :: LBS.ByteString }
  | RawPay  { payRaw  :: LBS.ByteString }

data SelectItem
  = SelectField
    { selField             :: Field
    , selAggregateFunction :: Maybe AggregateFunction
    , selAggregateCast     :: Maybe Cast
    , selCast              :: Maybe Cast
    , selAlias             :: Maybe Alias
    }
  | SelectRelation
    { selRelation :: FieldName
    , selAlias    :: Maybe Alias
    , selHint     :: Maybe Hint
    , selJoinType :: Maybe JoinType
    }
  | SpreadRelation
    { selRelation :: FieldName
    , selHint     :: Maybe Hint
    , selJoinType :: Maybe JoinType
    }
  deriving (Eq, Show)

type NodeName = Text
type Depth = Integer

data OrderTerm
  = OrderTerm
    { otTerm      :: Field
    , otDirection :: Maybe OrderDirection
    , otNullOrder :: Maybe OrderNulls
    }
  | OrderRelationTerm
    { otRelation  :: FieldName
    , otRelTerm   :: Field
    , otDirection :: Maybe OrderDirection
    , otNullOrder :: Maybe OrderNulls
    }
  deriving (Eq, Show)

data OrderDirection
  = OrderAsc
  | OrderDesc
  deriving (Eq, Show)

data OrderNulls
  = OrderNullsFirst
  | OrderNullsLast
  deriving (Eq, Show)

type Field = (FieldName, JsonPath)
type Cast = Text
type Alias = Text
type Hint = Text

data AggregateFunction = Sum | Avg | Max | Min | Count
  deriving (Show, Eq)

data EmbedParam
  = EPHint Hint
  | EPJoinType JoinType

data JoinType
  = JTInner
  | JTLeft
  deriving (Eq, Show)

type EmbedPath = [Text]
type JsonPath = [JsonOperation]

data JsonOperation
  = JArrow { jOp :: JsonOperand }
  | J2Arrow { jOp :: JsonOperand }
  deriving (Eq, Show, Ord)

data JsonOperand
  = JKey { jVal :: Text }
  | JIdx { jVal :: Text }
  deriving (Eq, Show, Ord)

data LogicTree
  = Expr Bool LogicOperator [LogicTree]
  | Stmnt Filter
  deriving (Eq, Show)

data LogicOperator
  = And
  | Or
  deriving (Eq, Show)

data Filter
  = Filter
  { field  :: Field
  , opExpr :: OpExpr
  }
  deriving (Eq, Show)

data OpExpr
  = OpExpr Bool Operation
  | NoOpExpr Text
  deriving (Eq, Show)

data OpQuantifier = QuantAny | QuantAll
  deriving (Eq, Show)

data Operation
  = Op SimpleOperator SingleVal
  | OpQuant QuantOperator (Maybe OpQuantifier) SingleVal
  | In ListVal
  | Is IsVal
  | IsDistinctFrom SingleVal
  | Fts FtsOperator (Maybe Language) SingleVal
  deriving (Eq, Show)

type Language = Text
type SingleVal = Text
type ListVal = [Text]

data IsVal
  = IsNull
  | IsNotNull
  | IsTriTrue
  | IsTriFalse
  | IsTriUnknown
  deriving (Eq, Show)

data QuantOperator
  = OpEqual
  | OpGreaterThanEqual
  | OpGreaterThan
  | OpLessThanEqual
  | OpLessThan
  | OpLike
  | OpILike
  | OpMatch
  | OpIMatch
  deriving (Eq, Show)

data SimpleOperator
  = OpNotEqual
  | OpContains
  | OpContained
  | OpOverlap
  | OpStrictlyLeft
  | OpStrictlyRight
  | OpNotExtendsRight
  | OpNotExtendsLeft
  | OpAdjacent
  deriving (Eq, Show)

data FtsOperator
  = FilterFts
  | FilterFtsPlain
  | FilterFtsPhrase
  | FilterFtsWebsearch
  deriving (Eq, Show)
