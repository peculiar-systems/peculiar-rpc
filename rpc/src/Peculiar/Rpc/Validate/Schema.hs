module Peculiar.Rpc.Validate.Schema
  ( Schema (..)
  , MessageType (..)
  , Field (..)
  , Oneof (..)
  , EnumType (..)
  , Kind (..)
  , schemaOf
  , kindNumber
  , isMessageKind
  , fieldNamed
  , fieldNumbered
  , extensionOf
  , extensionsOf
  , isMap
  , mapEntryOf
  , enumNamed
  ) where

import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.ProtoLens (unknownFields)
import Data.ProtoLens.Encoding.Wire (TaggedValue)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import GHC.Generics (Generic, Generically (..))
import Lens.Family2 ((^.))
import Proto.Google.Protobuf.Descriptor
  ( DescriptorProto
  , EnumDescriptorProto
  , FeatureSet
  , FeatureSet'EnumType (..)
  , FeatureSet'FieldPresence (..)
  , FeatureSet'MessageEncoding (..)
  , FeatureSet'Utf8Validation (..)
  , FieldDescriptorProto
  , FieldDescriptorProto'Label (..)
  , FieldDescriptorProto'Type (..)
  , FileDescriptorProto
  )

data Kind
  = DoubleKind
  | FloatKind
  | Int64Kind
  | Uint64Kind
  | Int32Kind
  | Fixed64Kind
  | Fixed32Kind
  | BoolKind
  | StringKind
  | GroupKind
  | MessageKind
  | BytesKind
  | Uint32Kind
  | EnumKind
  | Sfixed32Kind
  | Sfixed64Kind
  | Sint32Kind
  | Sint64Kind
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Field = Field
  { name :: T.Text
  , fullName :: T.Text
  , number :: Int32
  , kind :: Kind
  , typeName :: T.Text
  , repeated :: Bool
  , presence :: Bool
  , oneof :: Maybe T.Text
  , extension :: Bool
  , textName :: T.Text
  , defaultValue :: Maybe T.Text
  , options :: [TaggedValue]
  , verifiesUtf8 :: Bool
  }

data Oneof = Oneof
  { name :: T.Text
  , options :: [TaggedValue]
  }

data MessageType = MessageType
  { fullName :: T.Text
  , fields :: [Field]
  , numbered :: Map.Map Int32 Field
  , named :: Map.Map T.Text Field
  , oneofs :: [Oneof]
  , mapEntry :: Bool
  , options :: [TaggedValue]
  }

data EnumType = EnumType
  { fullName :: T.Text
  , values :: [(T.Text, Int32)]
  , closed :: Bool
  }

data Schema = Schema
  { messages :: Map.Map T.Text MessageType
  , enums :: Map.Map T.Text EnumType
  , extensions :: Map.Map (T.Text, Int32) Field
  , extensionNames :: Map.Map T.Text Field
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Schema

data Syntax = Proto2 | Proto3 | Editions
  deriving stock (Eq)

data Features = Features
  { presence :: FeatureSet'FieldPresence
  , enumType :: FeatureSet'EnumType
  , delimited :: Bool
  , verifiesUtf8 :: Bool
  }

kindNumber :: Kind -> Int
kindNumber kind = length (takeWhile (/= kind) [minBound ..]) + 1

isMessageKind :: Kind -> Bool
isMessageKind kind = kind == MessageKind || kind == GroupKind

fieldNamed :: T.Text -> MessageType -> Maybe Field
fieldNamed name message = Map.lookup name message.named

fieldNumbered :: Int32 -> MessageType -> Maybe Field
fieldNumbered number message = Map.lookup number message.numbered

extensionOf :: Schema -> T.Text -> Int32 -> Maybe Field
extensionOf schema extendee number = Map.lookup (extendee, number) schema.extensions

extensionsOf :: Schema -> T.Text -> [Field]
extensionsOf schema extendee = [field | ((owner, _), field) <- Map.toList schema.extensions, owner == extendee]

enumNamed :: Schema -> T.Text -> Maybe EnumType
enumNamed schema name = Map.lookup name schema.enums

mapEntryOf :: Schema -> Field -> Maybe MessageType
mapEntryOf schema field
  | field.repeated && field.kind == MessageKind = case Map.lookup field.typeName schema.messages of
      Just entry | entry.mapEntry -> Just entry
      _ -> Nothing
  | otherwise = Nothing

isMap :: Schema -> Field -> Bool
isMap schema = isJust . mapEntryOf schema

schemaOf :: [FileDescriptorProto] -> Schema
schemaOf = foldl' (\schema file -> schema <> fileSchema file) mempty

fileSchema :: FileDescriptorProto -> Schema
fileSchema file =
  mconcat
    ( fmap (messageSchema syntax features scope) (file ^. #messageType)
        <> fmap (enumSchema syntax features scope) (file ^. #enumType)
        <> fmap (extensionSchema syntax features scope) (file ^. #extension)
    )
 where
  syntax = case file ^. #syntax of
    "proto3" -> Proto3
    "editions" -> Editions
    _ -> Proto2
  scope = file ^. #package
  features = refined (defaults syntax) (file ^. #options . #maybe'features)

defaults :: Syntax -> Features
defaults = \case
  Proto2 -> Features{presence = FeatureSet'EXPLICIT, enumType = FeatureSet'CLOSED, delimited = False, verifiesUtf8 = False}
  Proto3 -> Features{presence = FeatureSet'IMPLICIT, enumType = FeatureSet'OPEN, delimited = False, verifiesUtf8 = True}
  Editions -> Features{presence = FeatureSet'EXPLICIT, enumType = FeatureSet'OPEN, delimited = False, verifiesUtf8 = True}

refined :: Features -> Maybe FeatureSet -> Features
refined inherited = \case
  Nothing -> inherited
  Just set ->
    Features
      { presence = case set ^. #maybe'fieldPresence of
          Just FeatureSet'FIELD_PRESENCE_UNKNOWN -> inherited.presence
          Just presence -> presence
          Nothing -> inherited.presence
      , enumType = case set ^. #maybe'enumType of
          Just FeatureSet'ENUM_TYPE_UNKNOWN -> inherited.enumType
          Just enumType -> enumType
          Nothing -> inherited.enumType
      , delimited = case set ^. #maybe'messageEncoding of
          Just FeatureSet'DELIMITED -> True
          Just FeatureSet'LENGTH_PREFIXED -> False
          _ -> inherited.delimited
      , verifiesUtf8 = case set ^. #maybe'utf8Validation of
          Just FeatureSet'VERIFY -> True
          Just FeatureSet'NONE -> False
          _ -> inherited.verifiesUtf8
      }

qualified :: T.Text -> T.Text -> T.Text
qualified scope name = if T.null scope then name else scope <> "." <> name

messageSchema :: Syntax -> Features -> T.Text -> DescriptorProto -> Schema
messageSchema syntax inherited scope message =
  mconcat
    ( mempty{messages = Map.singleton fullName described}
        : fmap (messageSchema syntax features fullName) (message ^. #nestedType)
          <> fmap (enumSchema syntax features fullName) (message ^. #enumType)
          <> fmap (extensionSchema syntax features fullName) (message ^. #extension)
    )
 where
  fullName = qualified scope (message ^. #name)
  features = refined inherited (message ^. #options . #maybe'features)
  declaredOneofs = message ^. #oneofDecl
  syntheticOneofs = [index | field <- message ^. #field, field ^. #proto3Optional, Just index <- [field ^. #maybe'oneofIndex]]
  realOneofs = [Oneof{name = oneof ^. #name, options = oneof ^. #options . unknownFields} | (index, oneof) <- zip [0 :: Int32 ..] declaredOneofs, index `notElem` syntheticOneofs]
  oneofName field = case field ^. #maybe'oneofIndex of
    Just index | not (field ^. #proto3Optional) -> case drop (fromIntegral index) declaredOneofs of
      oneof : _ -> Just (oneof ^. #name)
      [] -> Nothing
    _ -> Nothing
  described =
    MessageType
      { fullName
      , fields
      , numbered = Map.fromList [(field.number, field) | field <- fields]
      , named = Map.fromList [(field.name, field) | field <- fields]
      , oneofs = realOneofs
      , mapEntry = message ^. #options . #mapEntry
      , options = message ^. #options . unknownFields
      }
  entries = [qualified fullName (nested ^. #name) | nested <- message ^. #nestedType, nested ^. #options . #mapEntry]
  fields = [fieldOf Scope{syntax, features, name = fullName, entries} (oneofName field) False field | field <- message ^. #field]

extensionSchema :: Syntax -> Features -> T.Text -> FieldDescriptorProto -> Schema
extensionSchema syntax features scope declaration =
  mempty
    { extensions = Map.singleton (extendee, field.number) field
    , extensionNames = Map.singleton field.fullName field
    }
 where
  extendee = T.dropWhile (== '.') (declaration ^. #extendee)
  field = fieldOf Scope{syntax, features, name = scope, entries = []} Nothing True declaration

data Scope = Scope
  { syntax :: Syntax
  , features :: Features
  , name :: T.Text
  , entries :: [T.Text]
  }

fieldOf :: Scope -> Maybe T.Text -> Bool -> FieldDescriptorProto -> Field
fieldOf scope oneof isExtension declaration =
  Field
    { name = declaration ^. #name
    , fullName = qualified scope.name (declaration ^. #name)
    , number = declaration ^. #number
    , kind
    , typeName = T.dropWhile (== '.') (declaration ^. #typeName)
    , repeated
    , presence =
        not repeated
          && ( isMessageKind kind
                 || isJust oneof
                 || declaration ^. #proto3Optional
                 || required
                 || scope.syntax /= Proto3 && features.presence /= FeatureSet'IMPLICIT
             )
    , oneof
    , extension = isExtension
    , textName = if isExtension then "[" <> qualified scope.name (declaration ^. #name) <> "]" else declaration ^. #name
    , defaultValue = declaration ^. #maybe'defaultValue
    , options = declaration ^. #options . unknownFields
    , verifiesUtf8 = features.verifiesUtf8
    }
 where
  features = refined scope.features (declaration ^. #options . #maybe'features)
  repeated = declaration ^. #label == FieldDescriptorProto'LABEL_REPEATED
  required = declaration ^. #label == FieldDescriptorProto'LABEL_REQUIRED || features.presence == FeatureSet'LEGACY_REQUIRED
  kind = case declaration ^. #type' of
    FieldDescriptorProto'TYPE_DOUBLE -> DoubleKind
    FieldDescriptorProto'TYPE_FLOAT -> FloatKind
    FieldDescriptorProto'TYPE_INT64 -> Int64Kind
    FieldDescriptorProto'TYPE_UINT64 -> Uint64Kind
    FieldDescriptorProto'TYPE_INT32 -> Int32Kind
    FieldDescriptorProto'TYPE_FIXED64 -> Fixed64Kind
    FieldDescriptorProto'TYPE_FIXED32 -> Fixed32Kind
    FieldDescriptorProto'TYPE_BOOL -> BoolKind
    FieldDescriptorProto'TYPE_STRING -> StringKind
    FieldDescriptorProto'TYPE_GROUP -> GroupKind
    FieldDescriptorProto'TYPE_MESSAGE
      | features.delimited && T.dropWhile (== '.') (declaration ^. #typeName) `notElem` scope.entries -> GroupKind
      | otherwise -> MessageKind
    FieldDescriptorProto'TYPE_BYTES -> BytesKind
    FieldDescriptorProto'TYPE_UINT32 -> Uint32Kind
    FieldDescriptorProto'TYPE_ENUM -> EnumKind
    FieldDescriptorProto'TYPE_SFIXED32 -> Sfixed32Kind
    FieldDescriptorProto'TYPE_SFIXED64 -> Sfixed64Kind
    FieldDescriptorProto'TYPE_SINT32 -> Sint32Kind
    FieldDescriptorProto'TYPE_SINT64 -> Sint64Kind

enumSchema :: Syntax -> Features -> T.Text -> EnumDescriptorProto -> Schema
enumSchema _ inherited scope declaration =
  mempty
    { enums =
        Map.singleton
          fullName
          EnumType
            { fullName
            , values = [(value ^. #name, value ^. #number) | value <- declaration ^. #value]
            , closed = (refined inherited (declaration ^. #options . #maybe'features)).enumType == FeatureSet'CLOSED
            }
    }
 where
  fullName = qualified scope (declaration ^. #name)
