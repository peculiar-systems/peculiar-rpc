module Peculiar.Rpc.Json.Decode
  ( Parsing (..)
  , decodeWith
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.ProtoLens (Message (..), ScalarField (..), encodeMessage)
import Data.ProtoLens.Message (FieldAccessor (..), Registry, SomeMessageType (..), lookupRegistered)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Json.Access (Field (..), Many (..), repeatedAt, scalarAt, snakePath)
import Peculiar.Rpc.Json.Encode (special)
import Peculiar.Rpc.Json.Error (JsonError (..), Problem (..), failure, within)
import Peculiar.Rpc.Json.Plan (Kind (..), Names (..), Plan (..), Slot (..), planOf, slotAt)
import Peculiar.Rpc.Json.Scalar (parseKey, parseScalar)
import Peculiar.Rpc.Json.Time (readDuration, readTimestamp)

data Parsing = Parsing
  { registry :: Registry
  , ignoreUnknown :: Bool
  }

decodeWith :: (Message msg) => Parsing -> Plan msg -> Value -> Either JsonError msg
decodeWith parsing plan json = case plan.name of
  "google.protobuf.Any" -> anyFrom parsing plan json
  "google.protobuf.Timestamp" -> seconds readTimestamp
  "google.protobuf.Duration" -> seconds readDuration
  "google.protobuf.FieldMask" -> fieldMask plan json
  "google.protobuf.Value" -> valueFrom parsing plan json
  "google.protobuf.Struct" -> expecting "an object" (\case Object _ -> True; _ -> False) (first json)
  "google.protobuf.ListValue" -> expecting "an array" (\case Array _ -> True; _ -> False) (first json)
  name | special name -> first json
  _ -> objectFrom parsing plan json
 where
  first value = maybe (failure (Malformed ("a malformed " <> plan.name))) (\slot -> assign parsing slot value defMessage) (slotAt 1 plan)
  expecting what matches decoded = if matches json then decoded else failure (Expected what)
  seconds reading = case (json, scalarAt Int64Field 1 plan, scalarAt Int32Field 2 plan) of
    (String text, Just (Field whole), Just (Field part)) -> do
      (secs, nanos) <- reading text
      pure (defMessage & whole .~ secs & part .~ nanos)
    (String _, _, _) -> failure (Malformed ("a malformed " <> plan.name))
    _ -> failure (Expected "a string")

objectFrom :: (Message msg) => Parsing -> Plan msg -> Value -> Either JsonError msg
objectFrom parsing plan = \case
  Object fields -> fst <$> foldM step (defMessage, (Set.empty, Set.empty)) (KeyMap.toList fields)
  _ -> failure (Expected "an object")
 where
  step (message, (tags, oneofs)) (key, value) = case Map.lookup (Key.toText key) plan.byName of
    Nothing
      | parsing.ignoreUnknown -> Right (message, (tags, oneofs))
      | otherwise -> failure (UnknownField (Key.toText key))
    Just slot@(Slot names kind _)
      | Set.member names.tag tags -> failure (NamedTwice names.json)
      | value == Null && not (nullable names kind) -> Right (message, (Set.insert names.tag tags, oneofs))
      | Just group <- names.oneof, Set.member group oneofs -> failure (SeveralOfOneof group)
      | otherwise -> do
          assigned <- within names.json (assign parsing slot value message)
          pure (assigned, (Set.insert names.tag tags, maybe oneofs (`Set.insert` oneofs) names.oneof))

nullable :: Names -> Kind v -> Bool
nullable names = \case
  Scalar _ -> names.nullValue
  Nested plan -> plan.name == "google.protobuf.Value"

assign :: Parsing -> Slot msg -> Value -> msg -> Either JsonError msg
assign parsing (Slot names kind accessor) json message = case accessor of
  PlainField _ lens -> maybe message (\value -> message & lens .~ value) <$> lenient parsing (single parsing names kind json)
  OptionalField lens -> maybe message (\value -> message & lens .~ Just value) <$> lenient parsing (single parsing names kind json)
  RepeatedField _ lens -> case json of
    Array items -> (\values -> message & lens .~ values) . catMaybes <$> traverse item (Vector.toList (Vector.indexed items))
    _ -> failure (Expected "an array")
  MapField keyLens valueLens lens -> case (kind, json) of
    (Nested entryPlan, Object pairs) ->
      (\entries -> message & lens .~ Map.fromList [(built ^. keyLens, built ^. valueLens) | built <- catMaybes entries])
        <$> traverse (entry (Nested entryPlan)) (KeyMap.toList pairs)
    (_, Object _) -> failure (Malformed "a map without an entry message")
    _ -> failure (Expected "an object")
 where
  item (index, value)
    | value == Null && not (nullable names kind) = within (T.pack (show index)) (failure (Expected "a value, not null"))
    | otherwise = within (T.pack (show index)) (lenient parsing (single parsing names kind value))
  entry :: Kind entry -> (Key.Key, Value) -> Either JsonError (Maybe entry)
  entry (Scalar _) _ = failure (Malformed "a map without an entry message")
  entry (Nested entryPlan) (key, value) = within (Key.toText key) do
    keyed <- case slotAt 1 entryPlan of
      Just (Slot _ (Scalar keyKind) (PlainField _ keyField)) -> (\parsed -> defMessage & keyField .~ parsed) <$> parseKey keyKind (Key.toText key)
      Just (Slot _ (Scalar keyKind) (OptionalField keyField)) -> (\parsed -> defMessage & keyField .~ Just parsed) <$> parseKey keyKind (Key.toText key)
      _ -> failure (Malformed "a map entry without a key")
    case slotAt 2 entryPlan of
      Just (Slot valueNames valueKind valueAccessor)
        | value == Null && not (nullable valueNames valueKind) -> failure (Expected "a value, not null")
        | otherwise ->
            lenient parsing (single parsing valueNames valueKind value) >>= \case
              Nothing -> Right Nothing
              Just parsed -> do
                built <- case valueAccessor of
                  PlainField _ valueField -> Right (keyed & valueField .~ parsed)
                  OptionalField valueField -> Right (keyed & valueField .~ Just parsed)
                  _ -> failure (Malformed "a map entry whose value is not singular")
                pure (Just built)
      Nothing -> failure (Malformed "a map entry without a value")

lenient :: Parsing -> Either JsonError a -> Either JsonError (Maybe a)
lenient parsing = \case
  Left JsonError{path = [], problem = UnknownEnum _} | parsing.ignoreUnknown -> Right Nothing
  other -> Just <$> other

single :: Parsing -> Names -> Kind v -> Value -> Either JsonError v
single parsing names = \case
  Scalar kind -> parseScalar names.nullValue kind
  Nested plan -> decodeWith parsing plan

fieldMask :: (Message msg) => Plan msg -> Value -> Either JsonError msg
fieldMask plan = \case
  String text -> case repeatedAt StringField 1 plan of
    Just (Many paths) ->
      maybe
        (failure (Malformed "a field mask path in snake_case"))
        (\snake -> Right (defMessage & paths .~ snake))
        (traverse snakePath (if T.null text then [] else T.splitOn "," text))
    Nothing -> failure (Malformed "a malformed google.protobuf.FieldMask")
  _ -> failure (Expected "a string")

valueFrom :: (Message msg) => Parsing -> Plan msg -> Value -> Either JsonError msg
valueFrom parsing plan json = maybe (failure (Malformed "a malformed google.protobuf.Value")) (\slot -> assign parsing slot json defMessage) (slotAt kindTag plan)
 where
  kindTag = case json of
    Null -> 1
    Number _ -> 2
    String _ -> 3
    Bool _ -> 4
    Object _ -> 5
    Array _ -> 6

anyFrom :: (Message msg) => Parsing -> Plan msg -> Value -> Either JsonError msg
anyFrom parsing plan = \case
  Object fields
    | KeyMap.null fields -> Right defMessage
    | otherwise -> case (KeyMap.lookup "@type" fields, scalarAt StringField 1 plan, scalarAt BytesField 2 plan) of
        (Just (String url), Just (Field urlField), Just (Field bytesField)) -> case lookupRegistered url parsing.registry of
          Nothing -> failure (UnknownType url)
          Just (SomeMessageType (Proxy :: Proxy inner)) -> do
            let body
                  | special (messageName (Proxy @inner)) = maybe (failure (Malformed "an Any of a well-known type needs a value")) Right (KeyMap.lookup "value" fields)
                  | otherwise = Right (Object (KeyMap.delete "@type" fields))
            inner <- decodeWith parsing (planOf @inner) =<< body
            pure (defMessage & urlField .~ url & bytesField .~ encodeMessage inner)
        (Just _, _, _) -> failure (Expected "@type to be a string")
        (Nothing, _, _) -> failure (Malformed "an Any needs @type")
  _ -> failure (Expected "an object")
