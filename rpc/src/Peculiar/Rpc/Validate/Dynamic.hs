module Peculiar.Rpc.Validate.Dynamic
  ( DecodeError (..)
  , decodeMessage
  , decodeFields
  , emptyMessage
  , has
  , get
  , whichOneof
  , zeroOf
  , defaultOf
  , isZero
  , equal
  , numericOrdering
  , lookupKey
  , celValue
  , fieldValue
  , typeUrlName
  ) where

import Data.Bits (xor, (.&.))
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.ProtoLens.Encoding.Bytes (Parser, atEnd, getFixed32, getFixed64, getVarInt, runParser)
import Data.ProtoLens.Encoding.Wire (Tag (..), TaggedValue (..), WireValue (..), parseFieldSet)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Read qualified as TR
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble, float2Double)
import Peculiar.Rpc.Validate.Arithmetic (shiftedRight)
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Schema

data DecodeError
  = MalformedWire T.Text
  | UnterminatedGroup Int32
  | InvalidUtf8 T.Text
  | UnknownMessage T.Text
  deriving stock (Eq, Show)

decodeMessage :: Schema -> MessageType -> BS.ByteString -> Either DecodeError Message
decodeMessage schema described bytes = either (Left . MalformedWire . T.pack) (decodeFields schema described) (runParser parseFieldSet bytes)

emptyMessage :: Schema -> T.Text -> Message
emptyMessage schema name =
  Message
    { described = fromMaybe placeholder (Map.lookup name schema.messages)
    , slots = Map.empty
    }
 where
  placeholder = MessageType{fullName = name, fields = [], numbered = Map.empty, named = Map.empty, oneofs = [], mapEntry = False, options = []}

decodeFields :: Schema -> MessageType -> [TaggedValue] -> Either DecodeError Message
decodeFields schema described = go Message{described, slots = Map.empty}
 where
  go message = \case
    [] -> Right message
    TaggedValue (Tag tag) wire : rest -> case lookupField (fromIntegral tag) of
      Nothing -> case wire of
        StartGroup -> grouped (fromIntegral tag) rest >>= go message . snd
        _ -> go message rest
      Just field -> case wire of
        StartGroup
          | field.kind == GroupKind -> do
              (body, rest') <- grouped (fromIntegral tag) rest
              nested <- decodeFields schema (messageTypeOf field) body
              go (store field (MessageV nested) message) rest'
          | otherwise -> grouped (fromIntegral tag) rest >>= go message . snd
        _ -> do
          decoded <- decodeWire schema field wire
          go (foldl' (flip (store field)) message decoded) rest
  lookupField number = case fieldNumbered number described of
    Just field -> Just field
    Nothing -> extensionOf schema described.fullName number
  messageTypeOf field = (emptyMessage schema field.typeName).described
  store field value message = case mapEntryOf schema field of
    Just entry -> case value of
      MessageV pair -> insertPair schema field entry pair message
      _ -> message
    Nothing
      | field.repeated -> message{slots = Map.insertWith appended field.number (Many [value]) message.slots}
      | otherwise -> (clearOneof field message){slots = Map.insertWith (merged field) field.number (Single value) message.slots}
  appended new old = case (old, new) of
    (Many olds, Many news) -> Many (olds <> news)
    _ -> new
  merged field new old = case (old, new) of
    (Single (MessageV previous), Single (MessageV next)) | isMessageKind field.kind -> Single (MessageV (mergeMessages previous next))
    _ -> new
  clearOneof field message = case field.oneof of
    Nothing -> message
    Just group -> message{slots = Map.filterWithKey (\number _ -> all (\other -> other.oneof /= Just group || other.number == field.number) (fieldNumbered number described)) message.slots}

insertPair :: Schema -> Field -> MessageType -> Message -> Message -> Message
insertPair schema field entry pair message = case (fieldNumbered 1 entry, fieldNumbered 2 entry) of
  (Just keyField, Just valueField) ->
    let key = get schema keyField pair
        value = get schema valueField pair
     in case valueKey key of
          Just mapKey -> message{slots = Map.insertWith joined field.number (Pairs (Map.singleton mapKey value)) message.slots}
          Nothing -> message
  _ -> message
 where
  joined new old = case (old, new) of
    (Pairs olds, Pairs news) -> Pairs (Map.union news olds)
    _ -> new

mergeMessages :: Message -> Message -> Message
mergeMessages previous next = previous{slots = Map.unionWith combine previous.slots next.slots}
 where
  combine old new = case (old, new) of
    (Single (MessageV a), Single (MessageV b)) -> Single (MessageV (mergeMessages a b))
    (Many olds, Many news) -> Many (olds <> news)
    (Pairs olds, Pairs news) -> Pairs (Map.union news olds)
    _ -> new

grouped :: Int32 -> [TaggedValue] -> Either DecodeError ([TaggedValue], [TaggedValue])
grouped tag = go (0 :: Int) []
 where
  go depth taken = \case
    [] -> Left (UnterminatedGroup tag)
    value@(TaggedValue (Tag current) wire) : rest -> case wire of
      EndGroup
        | depth == 0 && fromIntegral current == tag -> Right (reverse taken, rest)
        | otherwise -> go (depth - 1) (value : taken) rest
      StartGroup -> go (depth + 1) (value : taken) rest
      _ -> go depth (value : taken) rest

decodeWire :: Schema -> Field -> WireValue -> Either DecodeError [Value]
decodeWire schema field wire = case (field.kind, wire) of
  (StringKind, Lengthy bytes)
    | field.verifiesUtf8 -> either (const (Left (InvalidUtf8 field.fullName))) (Right . pure . StringV) (TE.decodeUtf8' bytes)
    | otherwise -> Right [StringV (TE.decodeUtf8With lenientDecode bytes)]
  (BytesKind, Lengthy bytes) -> Right [BytesV bytes]
  (MessageKind, Lengthy bytes) -> pure . MessageV <$> decodeMessage schema (emptyMessage schema field.typeName).described bytes
  (kind, Lengthy bytes)
    | field.repeated && packable kind -> either (Left . MalformedWire . T.pack) (Right . mapMaybe (scalar kind)) (runParser (packed kind) bytes)
  (kind, VarInt word) | varint kind -> Right (maybeToList (scalar kind (VarInt word)))
  (kind, Fixed32 word) | fixed32 kind -> Right (maybeToList (scalar kind (Fixed32 word)))
  (kind, Fixed64 word) | fixed64 kind -> Right (maybeToList (scalar kind (Fixed64 word)))
  _ -> Right []
 where
  packable kind = varint kind || fixed32 kind || fixed64 kind
  varint kind = kind `elem` [Int32Kind, Int64Kind, Uint32Kind, Uint64Kind, Sint32Kind, Sint64Kind, BoolKind, EnumKind]
  fixed32 kind = kind `elem` [Fixed32Kind, Sfixed32Kind, FloatKind]
  fixed64 kind = kind `elem` [Fixed64Kind, Sfixed64Kind, DoubleKind]
  packed :: Kind -> Parser [WireValue]
  packed kind = loop []
   where
    loop acc = do
      end <- atEnd
      if end
        then pure (reverse acc)
        else do
          next <-
            if
              | varint kind -> VarInt <$> getVarInt
              | fixed32 kind -> Fixed32 <$> getFixed32
              | otherwise -> Fixed64 <$> getFixed64
          loop (next : acc)
  scalar kind value = case (kind, value) of
    (Int32Kind, VarInt word) -> Just (IntV (fromIntegral (fromIntegral word :: Int32)))
    (Int64Kind, VarInt word) -> Just (IntV (fromIntegral word))
    (Uint32Kind, VarInt word) -> Just (UintV (fromIntegral (fromIntegral word :: Word32)))
    (Uint64Kind, VarInt word) -> Just (UintV word)
    (Sint32Kind, VarInt word) -> Just (IntV (fromIntegral (zigzag32 (fromIntegral word))))
    (Sint64Kind, VarInt word) -> Just (IntV (zigzag64 word))
    (BoolKind, VarInt word) -> Just (BoolV (word /= 0))
    (EnumKind, VarInt word) ->
      let number = fromIntegral word :: Int32
       in if knownEnum number then Just (IntV (fromIntegral number)) else Nothing
    (Fixed32Kind, Fixed32 word) -> Just (UintV (fromIntegral word))
    (Sfixed32Kind, Fixed32 word) -> Just (IntV (fromIntegral (fromIntegral word :: Int32)))
    (FloatKind, Fixed32 word) -> Just (DoubleV (float2Double (castWord32ToFloat word)))
    (Fixed64Kind, Fixed64 word) -> Just (UintV word)
    (Sfixed64Kind, Fixed64 word) -> Just (IntV (fromIntegral word))
    (DoubleKind, Fixed64 word) -> Just (DoubleV (castWord64ToDouble word))
    _ -> Nothing
  knownEnum number = case enumNamed schema field.typeName of
    Just enum | enum.closed -> number `elem` fmap snd enum.values
    _ -> True

zigzag32 :: Word32 -> Int32
zigzag32 word = fromIntegral (shiftedRight word 1) `xor` negate (fromIntegral (word .&. 1))

zigzag64 :: Word64 -> Int64
zigzag64 word = fromIntegral (shiftedRight word 1) `xor` negate (fromIntegral (word .&. 1))

has :: Field -> Message -> Bool
has field message = case Map.lookup field.number message.slots of
  Nothing -> False
  Just (Many values) -> not (null values)
  Just (Pairs pairs) -> not (Map.null pairs)
  Just (Single value) -> field.presence || not (isZero value)

whichOneof :: T.Text -> Message -> Maybe Field
whichOneof group message = case [field | field <- message.described.fields, field.oneof == Just group, has field message] of
  field : _ -> Just field
  [] -> Nothing

get :: Schema -> Field -> Message -> Value
get schema field message = case Map.lookup field.number message.slots of
  Just (Single value) -> value
  Just (Many values) -> ListV values
  Just (Pairs pairs) -> MapV pairs
  Nothing
    | isMap schema field -> MapV Map.empty
    | field.repeated -> ListV []
    | otherwise -> defaultOf schema field

defaultOf :: Schema -> Field -> Value
defaultOf schema field = case field.defaultValue of
  Just written | not field.repeated -> fromMaybe zero (parsedDefault schema field written)
  _ -> zero
 where
  zero = zeroOf schema field

zeroOf :: Schema -> Field -> Value
zeroOf schema field = case field.kind of
  DoubleKind -> DoubleV 0
  FloatKind -> DoubleV 0
  Int64Kind -> IntV 0
  Uint64Kind -> UintV 0
  Int32Kind -> IntV 0
  Fixed64Kind -> UintV 0
  Fixed32Kind -> UintV 0
  BoolKind -> BoolV False
  StringKind -> StringV ""
  GroupKind -> MessageV (emptyMessage schema field.typeName)
  MessageKind -> MessageV (emptyMessage schema field.typeName)
  BytesKind -> BytesV BS.empty
  Uint32Kind -> UintV 0
  EnumKind -> IntV (maybe 0 (fromIntegral . snd) (firstValue =<< enumNamed schema field.typeName))
  Sfixed32Kind -> IntV 0
  Sfixed64Kind -> IntV 0
  Sint32Kind -> IntV 0
  Sint64Kind -> IntV 0
 where
  firstValue enum = case enum.values of
    first : _ | enum.closed -> Just first
    _ -> Nothing

parsedDefault :: Schema -> Field -> T.Text -> Maybe Value
parsedDefault schema field written = case field.kind of
  DoubleKind -> DoubleV <$> floating
  FloatKind -> DoubleV . float2Double . realToFrac <$> floating
  Int64Kind -> IntV <$> signed
  Int32Kind -> IntV <$> signed
  Sint64Kind -> IntV <$> signed
  Sint32Kind -> IntV <$> signed
  Sfixed64Kind -> IntV <$> signed
  Sfixed32Kind -> IntV <$> signed
  Uint64Kind -> UintV <$> unsigned
  Uint32Kind -> UintV <$> unsigned
  Fixed64Kind -> UintV <$> unsigned
  Fixed32Kind -> UintV <$> unsigned
  BoolKind -> Just (BoolV (written == "true"))
  StringKind -> Just (StringV written)
  BytesKind -> BytesV <$> unescaped written
  EnumKind -> IntV . fromIntegral <$> (lookup written . (.values) =<< enumNamed schema field.typeName)
  _ -> Nothing
 where
  whole :: TR.Reader a -> Maybe a
  whole parser = case parser written of
    Right (value, rest) | T.null rest -> Just value
    _ -> Nothing
  signed :: (Integral a) => Maybe a
  signed = whole (TR.signed TR.decimal)
  unsigned :: (Integral a) => Maybe a
  unsigned = whole TR.decimal
  floating = case written of
    "inf" -> Just (1 / 0)
    "-inf" -> Just (negate (1 / 0))
    "nan" -> Just (0 / 0)
    _ -> whole TR.double

unescaped :: T.Text -> Maybe BS.ByteString
unescaped = fmap BS.pack . go . BS.unpack . TE.encodeUtf8
 where
  go = \case
    [] -> Just []
    92 : rest -> case rest of
      110 : more -> (10 :) <$> go more
      114 : more -> (13 :) <$> go more
      116 : more -> (9 :) <$> go more
      34 : more -> (34 :) <$> go more
      39 : more -> (39 :) <$> go more
      92 : more -> (92 :) <$> go more
      120 : more ->
        let (digits, others) = span isHex (take 2 more)
         in if null digits then Nothing else (fromIntegral (foldl' (\acc digit -> acc * 16 + hexValue digit) (0 :: Int) digits) :) <$> go (others <> drop 2 more)
      _ ->
        let (digits, others) = span isOctal (take 3 rest)
         in if null digits then Nothing else (fromIntegral (foldl' (\acc digit -> acc * 8 + fromIntegral (digit - 48)) (0 :: Int) digits) :) <$> go (others <> drop 3 rest)
    byte : rest -> (byte :) <$> go rest
  isOctal byte = byte >= 48 && byte <= 55
  isHex byte = byte >= 48 && byte <= 57 || byte >= 97 && byte <= 102 || byte >= 65 && byte <= 70
  hexValue byte
    | byte >= 97 = fromIntegral byte - 87
    | byte >= 65 = fromIntegral byte - 55
    | otherwise = fromIntegral byte - 48

isZero :: Value -> Bool
isZero = \case
  NullV -> True
  BoolV flag -> not flag
  IntV number -> number == 0
  UintV number -> number == 0
  DoubleV number -> number == 0
  StringV text -> T.null text
  BytesV bytes -> BS.null bytes
  ListV values -> null values
  MapV pairs -> Map.null pairs
  MessageV message -> all (\field -> not (has field message)) message.described.fields && Map.null (Map.filterWithKey (\number _ -> number `notElem` Map.keys message.described.numbered) message.slots)
  DurationV nanos -> nanos == 0
  TimestampV nanos -> nanos == 0
  TypeV _ -> False

equal :: Value -> Value -> Bool
equal left right = case (left, right) of
  (NullV, NullV) -> True
  (BoolV a, BoolV b) -> a == b
  (StringV a, StringV b) -> a == b
  (BytesV a, BytesV b) -> a == b
  (DurationV a, DurationV b) -> a == b
  (TimestampV a, TimestampV b) -> a == b
  (TypeV a, TypeV b) -> a == b
  (ListV as, ListV bs) -> length as == length bs && and (zipWith equal as bs)
  (MapV as, MapV bs) -> Map.size as == Map.size bs && all (\(key, value) -> any (equal value) (lookupKey key bs)) (Map.toList as)
  (MessageV a, MessageV b) -> messagesEqual a b
  _ -> case numericOrdering left right of
    Just (Right EQ) -> True
    _ -> False

numericOrdering :: Value -> Value -> Maybe (Either CelError Ordering)
numericOrdering left right = case (left, right) of
  (IntV a, IntV b) -> ordered (compare a b)
  (UintV a, UintV b) -> ordered (compare a b)
  (DoubleV a, DoubleV b)
    | isNaN a || isNaN b -> unordered
    | otherwise -> ordered (doubles a b)
  (IntV a, UintV b) -> ordered (intUint a b)
  (UintV a, IntV b) -> ordered (inverted (intUint b a))
  (DoubleV a, IntV b)
    | isNaN a -> unordered
    | otherwise -> ordered (doubleInt a b)
  (IntV a, DoubleV b)
    | isNaN b -> unordered
    | otherwise -> ordered (inverted (doubleInt b a))
  (DoubleV a, UintV b)
    | isNaN a -> unordered
    | otherwise -> ordered (doubleUint a b)
  (UintV a, DoubleV b)
    | isNaN b -> unordered
    | otherwise -> ordered (inverted (doubleUint b a))
  _ -> Nothing
 where
  ordered = Just . Right
  unordered = Just (Left (InvalidArgument "NaN values cannot be ordered"))
  doubles a b
    | a < b = LT
    | a > b = GT
    | otherwise = EQ
  inverted = \case
    LT -> GT
    GT -> LT
    EQ -> EQ
  intUint a b
    | a < 0 || b > fromIntegral (maxBound :: Int64) = LT
    | otherwise = compare a (fromIntegral b)
  doubleInt a b
    | a < -9.223372036854775808e18 = LT
    | a > 9.223372036854775807e18 = GT
    | otherwise = doubles a (fromIntegral b)
  doubleUint a b
    | a < 0 = LT
    | a > 1.8446744073709551615e19 = GT
    | otherwise = doubles a (fromIntegral b)

lookupKey :: Key -> Map.Map Key Value -> Maybe Value
lookupKey key pairs = case Map.lookup key pairs of
  Just value -> Just value
  Nothing -> case key of
    IntK number | number >= 0 -> Map.lookup (UintK (fromIntegral number)) pairs
    UintK number | number <= fromIntegral (maxBound :: Int64) -> Map.lookup (IntK (fromIntegral number)) pairs
    _ -> Nothing

messagesEqual :: Message -> Message -> Bool
messagesEqual a b =
  a.described.fullName == b.described.fullName
    && all sameField a.described.fields
    && extensionsEqual
 where
  sameField field = case (has field a, has field b) of
    (False, False) -> True
    (True, True) -> equal (slotValue field a) (slotValue field b)
    _ -> False
  slotValue field message = case Map.lookup field.number message.slots of
    Just (Single value) -> value
    Just (Many values) -> ListV values
    Just (Pairs pairs) -> MapV pairs
    Nothing -> NullV
  declared message = Map.filterWithKey (\number _ -> number `notElem` Map.keys message.described.numbered) message.slots
  extensionsEqual = Map.keys (declared a) == Map.keys (declared b)

typeUrlName :: T.Text -> T.Text
typeUrlName = snd . T.breakOnEnd "/"

fieldValue :: Schema -> Field -> Message -> Either CelError Value
fieldValue schema field message
  | field.kind == EnumKind && field.typeName == "google.protobuf.NullValue" && not field.repeated = Right NullV
  | isMessageKind field.kind && not field.repeated && not (has field message) && wrapper field.typeName = Right NullV
  | otherwise = case get schema field message of
      ListV values -> ListV <$> traverse (celValue schema) values
      MapV pairs -> MapV <$> traverse (celValue schema) pairs
      value -> celValue schema value

wrapper :: T.Text -> Bool
wrapper name =
  name
    `elem` [ "google.protobuf.BoolValue"
           , "google.protobuf.BytesValue"
           , "google.protobuf.DoubleValue"
           , "google.protobuf.FloatValue"
           , "google.protobuf.Int32Value"
           , "google.protobuf.Int64Value"
           , "google.protobuf.StringValue"
           , "google.protobuf.UInt32Value"
           , "google.protobuf.UInt64Value"
           ]

celValue :: Schema -> Value -> Either CelError Value
celValue schema = \case
  MessageV message -> case message.described.fullName of
    "google.protobuf.Timestamp" -> Right (TimestampV (seconds message * 1000000000 + nanos message))
    "google.protobuf.Duration" -> Right (DurationV (clamped (seconds message * 1000000000 + nanos message)))
    "google.protobuf.Any" -> unpacked message
    "google.protobuf.Struct" -> structOf message
    "google.protobuf.Value" -> dynamicOf message
    "google.protobuf.ListValue" -> listOf message
    name
      | wrapper name -> Right (inner message)
      | otherwise -> Right (MessageV message)
  value -> Right value
 where
  numbered number message = case fieldNumbered number message.described of
    Just field -> get schema field message
    Nothing -> NullV
  seconds message = case numbered 1 message of
    IntV number -> toInteger number
    _ -> 0
  nanos message = case numbered 2 message of
    IntV number -> toInteger number
    _ -> 0
  clamped total = fromInteger (max (toInteger (minBound :: Int64)) (min (toInteger (maxBound :: Int64)) total))
  inner = numbered 1
  unpacked message = case (numbered 1 message, numbered 2 message) of
    (StringV url, BytesV bytes) -> case Map.lookup (typeUrlName url) schema.messages of
      Just described -> either (Left . InvalidArgument . T.pack . show) (celValue schema . MessageV) (decodeMessage schema described bytes)
      Nothing -> Left (UnknownType url)
    _ -> Left (InvalidArgument "malformed google.protobuf.Any")
  structOf message = case numbered 1 message of
    MapV pairs -> MapV <$> traverse (celValue schema) pairs
    _ -> Right (MapV Map.empty)
  listOf message = case numbered 1 message of
    ListV values -> ListV <$> traverse (celValue schema) values
    _ -> Right (ListV [])
  dynamicOf message = case [(field.number, get schema field message) | field <- message.described.fields, has field message] of
    (1, _) : _ -> Right NullV
    (2, value) : _ -> Right value
    (3, value) : _ -> Right value
    (4, value) : _ -> Right value
    (5, value) : _ -> celValue schema value
    (6, value) : _ -> celValue schema value
    _ -> Right NullV
