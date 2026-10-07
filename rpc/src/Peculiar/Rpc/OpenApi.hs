module Peculiar.Rpc.OpenApi
  ( Info (..)
  , Document
  , openApi
  , fieldBehaviours
  , reachable
  , shortName
  , Behaviour (..)
  ) where

import Data.ByteString qualified as BS
import Data.Foldable (asum)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.ProtoLens (unknownFields)
import Data.ProtoLens.Encoding.Wire (TaggedValue (..), WireValue (..))
import Data.ProtoLens.Encoding.Wire qualified as Wire
import Data.ProtoLens.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Family2 ((^.))
import Peculiar.Rpc.Catalog (Catalog (..), Described (..), commentOf, fieldNamed)
import Peculiar.Rpc.OpenApi.Document
import Peculiar.Rpc.Rest (Binding (..))
import Peculiar.Rpc.Rest.Template (openApiPath, variables)
import Peculiar.Rpc.Validate.Rules (Bound (..), Format (..), Kinded (..), Numbers (..), Rules (..), Sizes (..), Strings (..), rulesOf)
import Proto.Google.Protobuf.Descriptor (DescriptorProto, FieldDescriptorProto, FieldDescriptorProto'Label (..), FieldDescriptorProto'Type (..))

data Behaviour = Required | OutputOnly | InputOnly
  deriving stock (Eq, Show)

openApi :: Info -> Catalog -> [Binding] -> Document
openApi info catalog bindings =
  Document
    { openapi = "3.1.0"
    , info
    , tags = [Tag{name = shortName service, description = commentOf described} | (service, described) <- services]
    , paths = Map.fromListWith (<>) [(openApiPath binding.template, Map.singleton (T.toLower (TE.decodeUtf8 binding.verb)) (operation catalog binding named)) | (binding, named) <- numbered]
    , components = Components{schemas = builtins <> Map.fromList (mapMaybe (schemaFor catalog) (Set.toList (reachable catalog roots)))}
    }
 where
  served = filter (not . (.streaming)) bindings
  numbered = zip served (operationIds served)
  services = Map.toList (Map.fromList [(binding.service, described) | binding <- served, Just (_, described) <- [Map.lookup ("." <> binding.service) catalog.services]])
  roots = concat [[binding.input, binding.output] | binding <- served]

operationIds :: [Binding] -> [T.Text]
operationIds = go Map.empty
 where
  go _ [] = []
  go seen (binding : rest) =
    let base = shortName binding.service <> "_" <> binding.method
        count = Map.findWithDefault (0 :: Int) base seen
        named = if count == 0 then base else base <> "_" <> T.pack (show count)
     in named : go (Map.insert base (count + 1) seen) rest

shortName :: T.Text -> T.Text
shortName = last' . T.splitOn "."
 where
  last' parts = fromMaybe "" (listToMaybe (reverse parts))

operation :: Catalog -> Binding -> T.Text -> Operation
operation catalog binding operationId =
  Operation
    { operationId
    , summary = T.strip . T.takeWhile (/= '\n') <$> comment
    , description = comment
    , tags = [shortName binding.service]
    , parameters = pathParameters <> queryParameters
    , requestBody = requestBodyOf
    , responses =
        Map.fromList
          [ ("200", Response{description = "OK", content = json responseSchema})
          , ("default", Response{description = "An error, as a google.rpc.Status", content = json (reference ".google.rpc.Status")})
          ]
    , deprecated = Nothing
    }
 where
  comment = commentOf binding.described
  input = fst <$> Map.lookup binding.input catalog.messages
  pathFields = [field | (field, _) <- variables binding.template]
  pathParameters =
    [ Parameter{name = T.intercalate "." field, location = "path", required = True, description = descriptionAt field, schema = schemaAt field}
    | field <- pathFields
    ]
  queryParameters = case binding.body of
    Just "*" -> []
    body ->
      [ Parameter{name = field ^. #name, location = "query", required = False, description = Nothing, schema = fieldSchema catalog Nothing field}
      | Just message <- [input]
      , field <- message ^. #field
      , [field ^. #name] `notElem` pathFields
      , Just (field ^. #name) /= body
      , queryable field
      ]
  requestBodyOf = case binding.body of
    Nothing -> Nothing
    Just "*" -> Just RequestBody{required = True, content = json (reference binding.input)}
    Just field -> Just RequestBody{required = True, content = json (schemaAt [field])}
  responseSchema = case binding.responseBody of
    Just field -> maybe emptySchema (fieldSchema catalog Nothing) (fieldNamed field . fst =<< Map.lookup binding.output catalog.messages)
    Nothing -> reference binding.output
  schemaAt path = maybe emptySchema (fieldSchema catalog Nothing) (resolve binding.input path)
  descriptionAt _ = Nothing
  resolve typeName = \case
    [] -> Nothing
    [name] -> fieldNamed name . fst =<< Map.lookup typeName catalog.messages
    name : rest -> resolve typeName [name] >>= \field -> resolve (field ^. #typeName) rest
  queryable field = case field ^. #type' of
    FieldDescriptorProto'TYPE_MESSAGE -> field ^. #typeName `elem` stringLike
    _ -> True
  json schema = Map.singleton "application/json" Media{schema}
  stringLike =
    [ ".google.protobuf.Timestamp"
    , ".google.protobuf.Duration"
    , ".google.protobuf.FieldMask"
    , ".google.protobuf.BoolValue"
    , ".google.protobuf.StringValue"
    , ".google.protobuf.BytesValue"
    , ".google.protobuf.Int32Value"
    , ".google.protobuf.Int64Value"
    , ".google.protobuf.UInt32Value"
    , ".google.protobuf.UInt64Value"
    , ".google.protobuf.FloatValue"
    , ".google.protobuf.DoubleValue"
    ]

reference :: T.Text -> Schema
reference typeName = emptySchema{ref = Just ("#/components/schemas/" <> T.dropWhile (== '.') typeName)}

reachable :: Catalog -> [T.Text] -> Set.Set T.Text
reachable catalog = go Set.empty
 where
  go seen = \case
    [] -> seen
    name : rest
      | Set.member name seen || Map.member name wellKnownSchemas -> go seen rest
      | otherwise -> case Map.lookup name catalog.messages of
          Just (message, _) -> go (Set.insert name seen) (rest <> referenced message)
          Nothing
            | Map.member name catalog.enums -> go (Set.insert name seen) rest
            | otherwise -> go seen rest
  referenced message =
    concat
      [ case mapEntry catalog field of
          Just entry -> [value ^. #typeName | value <- entry ^. #field, value ^. #number == 2, not (T.null (value ^. #typeName))]
          Nothing -> [field ^. #typeName]
      | field <- message ^. #field
      , field ^. #type' `elem` [FieldDescriptorProto'TYPE_MESSAGE, FieldDescriptorProto'TYPE_ENUM]
      ]

schemaFor :: Catalog -> T.Text -> Maybe (T.Text, Schema)
schemaFor catalog name = case (Map.lookup name catalog.messages, Map.lookup name catalog.enums) of
  (Just (message, described), _) -> Just (T.dropWhile (== '.') name, messageSchema catalog message described)
  (_, Just (enum, described)) ->
    Just
      ( T.dropWhile (== '.') name
      , emptySchema{kind = Just "string", enum = Just [value ^. #name | value <- enum ^. #value], description = commentOf described}
      )
  _ -> Nothing

messageSchema :: Catalog -> DescriptorProto -> Described -> Schema
messageSchema catalog message described =
  emptySchema
    { kind = Just "object"
    , description = commentOf described
    , properties = Just (Map.fromList [(field ^. #jsonName, fieldSchema catalog (Just (described{path = described.path <> [2, index]})) field) | (index, field) <- zip [0 ..] (message ^. #field)])
    , required = case [field ^. #jsonName | field <- message ^. #field, Required `elem` fieldBehaviours field || (rulesOf field).required] of
        [] -> Nothing
        names -> Just names
    }

fieldSchema :: Catalog -> Maybe Described -> FieldDescriptorProto -> Schema
fieldSchema catalog described field =
  constrained (rulesOf field) . annotated $ case (field ^. #label, mapEntry catalog field) of
    (FieldDescriptorProto'LABEL_REPEATED, Just entry) ->
      emptySchema{kind = Just "object", additionalProperties = Just (maybe emptySchema (fieldSchema catalog Nothing) (listToMaybe [value | value <- entry ^. #field, value ^. #number == 2]))}
    (FieldDescriptorProto'LABEL_REPEATED, Nothing) -> emptySchema{kind = Just "array", items = Just (single field)}
    _ -> single field
 where
  behaviours = fieldBehaviours field
  annotated schema =
    schema
      { description = described >>= commentOf
      , readOnly = if OutputOnly `elem` behaviours then Just True else Nothing
      , writeOnly = if InputOnly `elem` behaviours then Just True else Nothing
      , deprecated = if field ^. #options . #deprecated then Just True else Nothing
      }
  single this = case this ^. #type' of
    FieldDescriptorProto'TYPE_DOUBLE -> typed "number" "double"
    FieldDescriptorProto'TYPE_FLOAT -> typed "number" "float"
    FieldDescriptorProto'TYPE_INT64 -> typed "string" "int64"
    FieldDescriptorProto'TYPE_SINT64 -> typed "string" "int64"
    FieldDescriptorProto'TYPE_SFIXED64 -> typed "string" "int64"
    FieldDescriptorProto'TYPE_UINT64 -> typed "string" "uint64"
    FieldDescriptorProto'TYPE_FIXED64 -> typed "string" "uint64"
    FieldDescriptorProto'TYPE_INT32 -> typed "integer" "int32"
    FieldDescriptorProto'TYPE_SINT32 -> typed "integer" "int32"
    FieldDescriptorProto'TYPE_SFIXED32 -> typed "integer" "int32"
    FieldDescriptorProto'TYPE_UINT32 -> (typed "integer" "uint32"){minimum = Just 0}
    FieldDescriptorProto'TYPE_FIXED32 -> (typed "integer" "uint32"){minimum = Just 0}
    FieldDescriptorProto'TYPE_BOOL -> emptySchema{kind = Just "boolean"}
    FieldDescriptorProto'TYPE_STRING -> emptySchema{kind = Just "string"}
    FieldDescriptorProto'TYPE_BYTES -> typed "string" "byte"
    _ -> fromMaybe (reference (this ^. #typeName)) (Map.lookup (this ^. #typeName) wellKnownSchemas)
  typed kind format = emptySchema{kind = Just kind, format = Just format}

constrained :: Rules -> Schema -> Schema
constrained rules schema = case rules.kinded of
  Just (Text strings) ->
    schema
      { minLength = fromIntegral <$> asum [strings.lengths.exactly, strings.lengths.atLeast]
      , maxLength = fromIntegral <$> asum [strings.lengths.exactly, strings.lengths.atMost]
      , enum = if null strings.among then schema.enum else Just strings.among
      , format = maybe schema.format openApiFormat strings.format
      }
  Just (Numeric numbers) -> bounds numbers schema
  Just (Repeated sizes unique items) ->
    schema
      { minItems = fromIntegral <$> sizes.atLeast
      , maxItems = fromIntegral <$> sizes.atMost
      , uniqueItems = if unique then Just True else Nothing
      , items = (\item -> maybe item (`constrained` item) items) <$> schema.items
      }
  Just (Keyed sizes _ _) -> schema{minProperties = fromIntegral <$> sizes.atLeast, maxProperties = fromIntegral <$> sizes.atMost}
  _ -> schema
 where
  bounds numbers target =
    target
      { minimum = case numbers.above of
          Just (Inclusive limit) -> Just (fromRational limit)
          _ -> target.minimum
      , exclusiveMinimum = case numbers.above of
          Just (Exclusive limit) -> Just (fromRational limit)
          _ -> Nothing
      , maximum = case numbers.below of
          Just (Inclusive limit) -> Just (fromRational limit)
          _ -> Nothing
      , exclusiveMaximum = case numbers.below of
          Just (Exclusive limit) -> Just (fromRational limit)
          _ -> Nothing
      }
  openApiFormat = \case
    Email -> Just "email"
    Uuid -> Just "uuid"
    Uri -> Just "uri"
    UriReference -> Just "uri-reference"
    Hostname -> Just "hostname"
    Ipv4 -> Just "ipv4"
    Ipv6 -> Just "ipv6"
    Ip -> Nothing

mapEntry :: Catalog -> FieldDescriptorProto -> Maybe DescriptorProto
mapEntry catalog field = case Map.lookup (field ^. #typeName) catalog.messages of
  Just (message, _) | message ^. #options . #mapEntry -> Just message
  _ -> Nothing

wellKnownSchemas :: Map.Map T.Text Schema
wellKnownSchemas =
  Map.fromList
    [ (".google.protobuf.Timestamp", emptySchema{kind = Just "string", format = Just "date-time"})
    , (".google.protobuf.Duration", emptySchema{kind = Just "string", regex = Just "^-?[0-9]+(\\.[0-9]{1,9})?s$"})
    , (".google.protobuf.FieldMask", emptySchema{kind = Just "string"})
    , (".google.protobuf.Struct", emptySchema{kind = Just "object"})
    , (".google.protobuf.Value", emptySchema)
    , (".google.protobuf.ListValue", emptySchema{kind = Just "array", items = Just emptySchema})
    , (".google.protobuf.Empty", emptySchema{kind = Just "object"})
    , (".google.protobuf.Any", reference ".google.protobuf.Any")
    , (".google.protobuf.BoolValue", emptySchema{kind = Just "boolean"})
    , (".google.protobuf.StringValue", emptySchema{kind = Just "string"})
    , (".google.protobuf.BytesValue", emptySchema{kind = Just "string", format = Just "byte"})
    , (".google.protobuf.Int32Value", emptySchema{kind = Just "integer", format = Just "int32"})
    , (".google.protobuf.UInt32Value", emptySchema{kind = Just "integer", format = Just "uint32"})
    , (".google.protobuf.Int64Value", emptySchema{kind = Just "string", format = Just "int64"})
    , (".google.protobuf.UInt64Value", emptySchema{kind = Just "string", format = Just "uint64"})
    , (".google.protobuf.FloatValue", emptySchema{kind = Just "number", format = Just "float"})
    , (".google.protobuf.DoubleValue", emptySchema{kind = Just "number", format = Just "double"})
    ]

builtins :: Map.Map T.Text Schema
builtins =
  Map.fromList
    [
      ( "google.rpc.Status"
      , emptySchema
          { kind = Just "object"
          , description = Just "The error model: a code from google.rpc.Code, a message for developers, and typed details."
          , properties =
              Just
                ( Map.fromList
                    [ ("code", emptySchema{kind = Just "integer", format = Just "int32"})
                    , ("message", emptySchema{kind = Just "string"})
                    , ("details", emptySchema{kind = Just "array", items = Just (reference ".google.protobuf.Any")})
                    ]
                )
          }
      )
    ,
      ( "google.protobuf.Any"
      , emptySchema
          { kind = Just "object"
          , description = Just "A message of the type named by @type, with its fields alongside."
          , properties = Just (Map.singleton "@type" emptySchema{kind = Just "string"})
          , additionalProperties = Just emptySchema
          }
      )
    ]

fieldBehaviours :: FieldDescriptorProto -> [Behaviour]
fieldBehaviours field = concatMap (mapMaybe behaviour . numbers) [value | TaggedValue (Wire.Tag 1052) value <- field ^. #options . unknownFields]
 where
  numbers = \case
    VarInt number -> [toInteger number]
    Lengthy bytes -> varints bytes
    _ -> []
  behaviour = \case
    2 -> Just Required
    3 -> Just OutputOnly
    4 -> Just InputOnly
    _ -> Nothing

varints :: BS.ByteString -> [Integer]
varints bytes
  | BS.null bytes = []
  | otherwise =
      let (number, rest) = one 0 1 bytes
       in number : varints rest
 where
  one :: Integer -> Integer -> BS.ByteString -> (Integer, BS.ByteString)
  one acc scale input = case BS.uncons input of
    Nothing -> (acc, BS.empty)
    Just (byte, rest)
      | byte >= 128 -> one (acc + toInteger (byte - 128) * scale) (scale * 128) rest
      | otherwise -> (acc + toInteger byte * scale, rest)
