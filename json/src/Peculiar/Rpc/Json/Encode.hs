module Peculiar.Rpc.Json.Encode
  ( encodeWith
  , special
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.ProtoLens (Message (..), ScalarField (..), decodeMessage)
import Data.ProtoLens.Message (FieldAccessor (..), FieldDefault (..), Registry, SomeMessageType (..), WireDefault (..), lookupRegistered)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Json.Access (Field (..), Many (..), camelPath, repeatedAt, scalarAt)
import Peculiar.Rpc.Json.Error (JsonError, Problem (..), failure, within)
import Peculiar.Rpc.Json.Plan (Kind (..), Names (..), Plan (..), Slot (..), planOf, slotAt)
import Peculiar.Rpc.Json.Scalar (encodeKey, encodeScalar)
import Peculiar.Rpc.Json.Time (showDuration, showTimestamp)

special :: T.Text -> Bool
special = (`elem` specials)
 where
  specials =
    [ "google.protobuf.Any"
    , "google.protobuf.Timestamp"
    , "google.protobuf.Duration"
    , "google.protobuf.FieldMask"
    , "google.protobuf.Struct"
    , "google.protobuf.Value"
    , "google.protobuf.ListValue"
    , "google.protobuf.DoubleValue"
    , "google.protobuf.FloatValue"
    , "google.protobuf.Int64Value"
    , "google.protobuf.UInt64Value"
    , "google.protobuf.Int32Value"
    , "google.protobuf.UInt32Value"
    , "google.protobuf.BoolValue"
    , "google.protobuf.StringValue"
    , "google.protobuf.BytesValue"
    ]

encodeWith :: Registry -> Plan msg -> msg -> Either JsonError Value
encodeWith registry plan message = case plan.name of
  "google.protobuf.Any" -> anyJson registry plan message
  "google.protobuf.Timestamp" -> seconds showTimestamp
  "google.protobuf.Duration" -> seconds showDuration
  "google.protobuf.FieldMask" -> fieldMask plan message
  "google.protobuf.Value" -> valueJson registry plan message
  name | special name -> maybe (failure (Malformed ("a malformed " <> name))) (payload registry message) (slotAt 1 plan)
  _ -> Object . KeyMap.fromList . concat <$> traverse (field registry message) plan.slots
 where
  seconds shown = case (scalarAt Int64Field 1 plan, scalarAt Int32Field 2 plan) of
    (Just (Field whole), Just (Field part)) -> String <$> shown (message ^. whole) (message ^. part)
    _ -> failure (Malformed ("a malformed " <> plan.name))

field :: Registry -> msg -> Slot msg -> Either JsonError [(Key.Key, Value)]
field registry message slot@(Slot names kind accessor) = within names.json case accessor of
  PlainField Optional lens -> if message ^. lens == fieldDefault then Right [] else one <$> single registry names kind (message ^. lens)
  PlainField Required lens -> one <$> single registry names kind (message ^. lens)
  OptionalField lens -> maybe (Right []) (fmap one . single registry names kind) (message ^. lens)
  RepeatedField _ lens -> if null (message ^. lens) then Right [] else one <$> payload registry message slot
  MapField _ _ lens -> if Map.null (message ^. lens) then Right [] else one <$> payload registry message slot
 where
  one value = [(Key.fromText names.json, value)]

payload :: Registry -> msg -> Slot msg -> Either JsonError Value
payload registry message (Slot names kind accessor) = case accessor of
  PlainField _ lens -> single registry names kind (message ^. lens)
  OptionalField lens -> maybe (Right Null) (single registry names kind) (message ^. lens)
  RepeatedField _ lens -> Array . Vector.fromList <$> traverse (single registry names kind) (message ^. lens)
  MapField keyLens valueLens lens -> case kind of
    Nested entryPlan -> Object . KeyMap.fromList <$> traverse (\(key, value) -> entry entryPlan (defMessage & keyLens .~ key & valueLens .~ value)) (Map.toList (message ^. lens))
    Scalar _ -> failure (Malformed "a map without an entry message")
 where
  entry entryPlan built = do
    keyText <- case slotAt 1 entryPlan of
      Just (Slot _ (Scalar keyKind) (PlainField _ lens)) -> Right (encodeKey keyKind (built ^. lens))
      Just (Slot _ (Scalar keyKind) (OptionalField lens)) -> maybe (failure (Malformed "a map entry without a key")) (Right . encodeKey keyKind) (built ^. lens)
      _ -> failure (Malformed "a map entry without a key")
    rendered <- maybe (failure (Malformed "a map entry without a value")) (payload registry built) (slotAt 2 entryPlan)
    pure (Key.fromText keyText, rendered)

single :: Registry -> Names -> Kind v -> v -> Either JsonError Value
single registry names = \case
  Scalar kind -> Right . encodeScalar names.nullValue kind
  Nested plan -> encodeWith registry plan

fieldMask :: Plan msg -> msg -> Either JsonError Value
fieldMask plan message = case repeatedAt StringField 1 plan of
  Just (Many paths) -> maybe (failure (Malformed "a field mask path that has no JSON form")) (Right . String . T.intercalate ",") (traverse camelPath (message ^. paths))
  Nothing -> failure (Malformed "a malformed google.protobuf.FieldMask")

valueJson :: Registry -> Plan msg -> msg -> Either JsonError Value
valueJson registry plan message = case [slot | slot@(Slot _ _ (OptionalField lens)) <- plan.slots, isJust (message ^. lens)] of
  [] -> failure (Malformed "a google.protobuf.Value with no kind set")
  slot@(Slot _ kind (OptionalField lens)) : _ -> case (kind, message ^. lens) of
    (Scalar DoubleField, Just number)
      | isNaN number || isInfinite number -> failure (Malformed "a google.protobuf.Value number must be finite")
    _ -> payload registry message slot
  slot : _ -> payload registry message slot

anyJson :: Registry -> Plan msg -> msg -> Either JsonError Value
anyJson registry plan message = case (scalarAt StringField 1 plan, scalarAt BytesField 2 plan) of
  (Just (Field url), Just (Field bytes))
    | T.null (message ^. url) -> Right (Object KeyMap.empty)
    | otherwise -> case lookupRegistered (message ^. url) registry of
        Nothing -> failure (UnknownType (message ^. url))
        Just (SomeMessageType (Proxy :: Proxy inner)) -> do
          decoded <- either (const (failure (Malformed "an Any whose value does not parse"))) Right (decodeMessage @inner (message ^. bytes))
          inner <- encodeWith registry (planOf @inner) decoded
          let typed = ("@type", String (message ^. url))
          pure case inner of
            Object fields | not (special (messageName (Proxy @inner))) -> Object (KeyMap.insert "@type" (String (message ^. url)) fields)
            other -> Object (KeyMap.fromList [typed, ("value", other)])
  _ -> failure (Malformed "a malformed google.protobuf.Any")
