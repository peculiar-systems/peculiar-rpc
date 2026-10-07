module Peculiar.Rpc.OpenApi.Document
  ( Document (..)
  , Info (..)
  , Tag (..)
  , PathItem
  , Operation (..)
  , Parameter (..)
  , RequestBody (..)
  , Response (..)
  , Media (..)
  , Components (..)
  , Schema (..)
  , emptySchema
  ) where

import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, OmitNothingFields, Rename)
import GHC.Generics (Generic)

data Document = Document
  { openapi :: T.Text
  , info :: Info
  , tags :: [Tag]
  , paths :: Map.Map T.Text PathItem
  , components :: Components
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Document

data Info = Info
  { title :: T.Text
  , version :: T.Text
  , description :: Maybe T.Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Info

data Tag = Tag
  { name :: T.Text
  , description :: Maybe T.Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Tag

type PathItem = Map.Map T.Text Operation

data Operation = Operation
  { operationId :: T.Text
  , summary :: Maybe T.Text
  , description :: Maybe T.Text
  , tags :: [T.Text]
  , parameters :: [Parameter]
  , requestBody :: Maybe RequestBody
  , responses :: Map.Map T.Text Response
  , deprecated :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Operation

data Parameter = Parameter
  { name :: T.Text
  , location :: T.Text
  , required :: Bool
  , description :: Maybe T.Text
  , schema :: Schema
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields, FieldLabelModifier '[Rename "location" "in"]] Parameter

data RequestBody = RequestBody
  { required :: Bool
  , content :: Map.Map T.Text Media
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] RequestBody

data Response = Response
  { description :: T.Text
  , content :: Map.Map T.Text Media
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Response

newtype Media = Media
  { schema :: Schema
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Media

newtype Components = Components
  { schemas :: Map.Map T.Text Schema
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Components

data Schema = Schema
  { ref :: Maybe T.Text
  , kind :: Maybe T.Text
  , format :: Maybe T.Text
  , description :: Maybe T.Text
  , properties :: Maybe (Map.Map T.Text Schema)
  , required :: Maybe [T.Text]
  , items :: Maybe Schema
  , additionalProperties :: Maybe Schema
  , enum :: Maybe [T.Text]
  , readOnly :: Maybe Bool
  , writeOnly :: Maybe Bool
  , deprecated :: Maybe Bool
  , minimum :: Maybe Double
  , maximum :: Maybe Double
  , exclusiveMinimum :: Maybe Double
  , exclusiveMaximum :: Maybe Double
  , minLength :: Maybe Int
  , maxLength :: Maybe Int
  , regex :: Maybe T.Text
  , minItems :: Maybe Int
  , maxItems :: Maybe Int
  , uniqueItems :: Maybe Bool
  , minProperties :: Maybe Int
  , maxProperties :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields, FieldLabelModifier '[Rename "ref" "$ref", Rename "kind" "type", Rename "regex" "pattern"]] Schema

emptySchema :: Schema
emptySchema =
  Schema
    { ref = Nothing
    , kind = Nothing
    , format = Nothing
    , description = Nothing
    , properties = Nothing
    , required = Nothing
    , items = Nothing
    , additionalProperties = Nothing
    , enum = Nothing
    , readOnly = Nothing
    , writeOnly = Nothing
    , deprecated = Nothing
    , minimum = Nothing
    , maximum = Nothing
    , exclusiveMinimum = Nothing
    , exclusiveMaximum = Nothing
    , minLength = Nothing
    , maxLength = Nothing
    , regex = Nothing
    , minItems = Nothing
    , maxItems = Nothing
    , uniqueItems = Nothing
    , minProperties = Nothing
    , maxProperties = Nothing
    }
