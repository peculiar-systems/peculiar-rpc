module Peculiar.Rpc.Rest
  ( Binding (..)
  , Lookup (..)
  , Routes (..)
  , noRoutes
  , bindingsOf
  , lookupBinding
  , requestValue
  , responseValue
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (unknownFields)
import Data.ProtoLens.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vector
import Lens.Family2 ((^.))
import Network.HTTP.Types.URI (decodePathSegments, parseQueryText)
import Peculiar.Rpc.Catalog (Catalog (..), Described (..), extension, fieldNamed)
import Peculiar.Rpc.Json (Registry)
import Peculiar.Rpc.Rest.Template (Template, matchTemplate, parseTemplate)
import Proto.Google.Api.Http (HttpRule, HttpRule'Pattern (..))
import Proto.Google.Protobuf.Descriptor (FieldDescriptorProto, FieldDescriptorProto'Label (..), FieldDescriptorProto'Type (..), MethodDescriptorProto)
import Text.Read (readMaybe)

data Binding = Binding
  { verb :: BS.ByteString
  , template :: Template
  , raw :: T.Text
  , body :: Maybe T.Text
  , responseBody :: Maybe T.Text
  , rpc :: BS.ByteString
  , service :: T.Text
  , method :: T.Text
  , input :: T.Text
  , output :: T.Text
  , streaming :: Bool
  , described :: Described
  }

data Routes = Routes
  { bindings :: [Binding]
  , catalog :: Catalog
  , registry :: Registry
  , document :: Maybe BS.ByteString
  }

noRoutes :: Routes
noRoutes = Routes{bindings = [], catalog = mempty, registry = mempty, document = Nothing}

data Lookup
  = NoRoute
  | WrongVerb [BS.ByteString]
  | Routed Binding [([T.Text], T.Text)] [(T.Text, T.Text)]

bindingsOf :: Catalog -> [T.Text] -> [Binding]
bindingsOf catalog names =
  [ binding
  | name <- names
  , Just (service, described) <- [Map.lookup ("." <> name) catalog.services]
  , (index, method) <- zip [0 ..] (service ^. #method)
  , Just rule <- [extension @HttpRule 72295728 (method ^. #options . unknownFields)]
  , each <- rule : rule ^. #additionalBindings
  , Just binding <- [bindingFor name described{path = described.path <> [2, index]} method each]
  ]

bindingFor :: T.Text -> Described -> MethodDescriptorProto -> HttpRule -> Maybe Binding
bindingFor name described method rule = do
  (verb, path) <- case rule ^. #maybe'pattern' of
    Just (HttpRule'Get path) -> Just ("GET", path)
    Just (HttpRule'Put path) -> Just ("PUT", path)
    Just (HttpRule'Post path) -> Just ("POST", path)
    Just (HttpRule'Delete path) -> Just ("DELETE", path)
    Just (HttpRule'Patch path) -> Just ("PATCH", path)
    Just (HttpRule'Custom custom) -> Just (TE.encodeUtf8 (custom ^. #kind), custom ^. #path)
    Nothing -> Nothing
  template <- parseTemplate path
  pure
    Binding
      { verb
      , template
      , raw = path
      , body = nonEmpty (rule ^. #body)
      , responseBody = nonEmpty (rule ^. #responseBody)
      , rpc = TE.encodeUtf8 ("/" <> name <> "/" <> method ^. #name)
      , service = name
      , method = method ^. #name
      , input = method ^. #inputType
      , output = method ^. #outputType
      , streaming = method ^. #serverStreaming || method ^. #clientStreaming
      , described
      }
 where
  nonEmpty text = if T.null text then Nothing else Just text

lookupBinding :: [Binding] -> BS.ByteString -> BS.ByteString -> Lookup
lookupBinding bindings verb target = case [(binding, bound) | binding <- bindings, Just bound <- [matchTemplate binding.template pieces]] of
  [] -> NoRoute
  matches -> case [(binding, bound) | (binding, bound) <- matches, binding.verb == verb] of
    (binding, bound) : _ -> Routed binding bound [(key, value) | (key, Just value) <- parseQueryText query]
    [] -> WrongVerb (distinct [binding.verb | (binding, _) <- matches])
 where
  (path, query) = BC.break (== '?') target
  pieces = decodePathSegments path

requestValue :: Catalog -> Binding -> [([T.Text], T.Text)] -> [(T.Text, T.Text)] -> Maybe Value -> Either T.Text Value
requestValue catalog binding bound query body = do
  base <- case (binding.body, body) of
    (Just "*", Just (Object fields)) -> Right fields
    (Just "*", Just _) -> Left "the request body must be a JSON object"
    (Just field, Just value) -> Right (KeyMap.singleton (Key.fromText field) value)
    (_, _) -> Right KeyMap.empty
  withPath <- foldM (\fields (path, raw) -> assign path [raw] fields) base bound
  let boundPaths = fmap fst bound
      queried = [(T.splitOn "." key, value) | binding.body /= Just "*", (key, value) <- query, T.splitOn "." key `notElem` boundPaths]
      grouped = Map.toList (Map.fromListWith (flip (<>)) [(path, [value]) | (path, value) <- queried])
  Object <$> foldM (\fields (path, values) -> assign path values fields) withPath grouped
 where
  assign path values fields = do
    field <- resolve binding.input path
    value <- converted field values
    Right (setAt path value fields)
  resolve typeName = \case
    [] -> Left "an empty field path"
    [name] -> do
      (message, _) <- maybe (Left ("unknown message " <> typeName)) Right (Map.lookup typeName catalog.messages)
      maybe (Left ("no field named " <> name)) Right (fieldNamed name message)
    name : rest -> do
      field <- resolve typeName [name]
      if field ^. #type' == FieldDescriptorProto'TYPE_MESSAGE then resolve (field ^. #typeName) rest else Left (name <> " is not a message")

converted :: FieldDescriptorProto -> [T.Text] -> Either T.Text Value
converted field values
  | field ^. #label == FieldDescriptorProto'LABEL_REPEATED = Array . Vector.fromList <$> traverse single values
  | otherwise = case values of
      [value] -> single value
      _ -> Left (field ^. #name <> " is given more than once")
 where
  single value = case field ^. #type' of
    FieldDescriptorProto'TYPE_BOOL -> boolean value
    FieldDescriptorProto'TYPE_ENUM | Just number <- readMaybe @Integer (T.unpack value), T.all isDigit value -> Right (Number (fromInteger number))
    FieldDescriptorProto'TYPE_MESSAGE -> case field ^. #typeName of
      ".google.protobuf.BoolValue" -> boolean value
      named
        | named `elem` stringy -> Right (String value)
        | otherwise -> Left (field ^. #name <> " is a message and cannot be given in the path or query")
    _ -> Right (String value)
  boolean = \case
    "true" -> Right (Bool True)
    "false" -> Right (Bool False)
    other -> Left (other <> " is not true or false")
  stringy =
    [ ".google.protobuf.Timestamp"
    , ".google.protobuf.Duration"
    , ".google.protobuf.FieldMask"
    , ".google.protobuf.StringValue"
    , ".google.protobuf.BytesValue"
    , ".google.protobuf.Int32Value"
    , ".google.protobuf.Int64Value"
    , ".google.protobuf.UInt32Value"
    , ".google.protobuf.UInt64Value"
    , ".google.protobuf.FloatValue"
    , ".google.protobuf.DoubleValue"
    ]

distinct :: (Ord a) => [a] -> [a]
distinct = go Set.empty
 where
  go _ [] = []
  go seen (item : rest)
    | Set.member item seen = go seen rest
    | otherwise = item : go (Set.insert item seen) rest

setAt :: [T.Text] -> Value -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
setAt path value fields = case path of
  [] -> fields
  [name] -> KeyMap.insert (Key.fromText name) value fields
  name : rest ->
    let inner = case KeyMap.lookup (Key.fromText name) fields of
          Just (Object existing) -> existing
          _ -> KeyMap.empty
     in KeyMap.insert (Key.fromText name) (Object (setAt rest value inner)) fields

responseValue :: Catalog -> Binding -> Value -> Value
responseValue catalog binding value = case (binding.responseBody, value) of
  (Just field, Object fields) ->
    let jsonName = maybe field (^. #jsonName) (fieldNamed field . fst =<< Map.lookup binding.output catalog.messages)
     in fromMaybe Null (KeyMap.lookup (Key.fromText jsonName) fields)
  _ -> value
