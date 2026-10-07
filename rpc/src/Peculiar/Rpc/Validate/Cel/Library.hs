module Peculiar.Rpc.Validate.Cel.Library
  ( call
  , arithmetic
  , ordering
  , negated
  , indexed
  , member
  , selected
  , present
  , compilePattern
  , knownFunction
  ) where

import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace, toLower, toUpper)
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Peculiar.Rpc.Validate.Address (isEmail, isHostAndPort, isHostname, isIp, isIpPrefix)
import Peculiar.Rpc.Validate.Arithmetic (digitValue, floorQuotRem, quotient, remainder)
import Peculiar.Rpc.Validate.Cel.Format (formatString, generalDouble)
import Peculiar.Rpc.Validate.Cel.Syntax (Operator (..))
import Peculiar.Rpc.Validate.Cel.Time
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Dynamic (equal, fieldValue, has, lookupKey, numericOrdering)
import Peculiar.Rpc.Validate.Schema (Schema, fieldNamed)
import Peculiar.Rpc.Validate.Uri (isUri, isUriReference)
import Regex.RE2 qualified as RE2

knownFunction :: T.Text -> Bool
knownFunction name = name `Set.member` functions
 where
  functions =
    Set.fromList
      [ "int"
      , "uint"
      , "double"
      , "string"
      , "bytes"
      , "bool"
      , "dyn"
      , "type"
      , "duration"
      , "timestamp"
      , "size"
      , "contains"
      , "startsWith"
      , "endsWith"
      , "matches"
      , "getFullYear"
      , "getMonth"
      , "getDayOfYear"
      , "getDayOfMonth"
      , "getDate"
      , "getDayOfWeek"
      , "getHours"
      , "getMinutes"
      , "getSeconds"
      , "getMilliseconds"
      , "getField"
      , "isNan"
      , "isInf"
      , "isHostname"
      , "isEmail"
      , "isIp"
      , "isIpPrefix"
      , "isUri"
      , "isUriRef"
      , "isHostAndPort"
      , "unique"
      , "charAt"
      , "indexOf"
      , "lastIndexOf"
      , "lowerAscii"
      , "upperAscii"
      , "replace"
      , "split"
      , "substring"
      , "trim"
      , "join"
      , "reverse"
      , "format"
      , "strings.quote"
      ]

noOverload :: T.Text -> [Value] -> Either CelError a
noOverload name arguments = Left (NoSuchOverload (name <> "(" <> T.intercalate ", " (fmap (typeName . typeOf) arguments) <> ")"))

compilePattern :: T.Text -> Either CelError RE2.Pattern
compilePattern source = case RE2.compile (TE.encodeUtf8 source) of
  Right compiled -> Right compiled
  Left failure -> Left (InvalidArgument ("invalid regular expression: " <> T.pack (RE2.errorMessage failure)))

matchesPattern :: RE2.Pattern -> T.Text -> Bool
matchesPattern compiled text = isJust (RE2.find compiled (TE.encodeUtf8 text))

call :: Schema -> T.Text -> Maybe Value -> [Value] -> Either CelError Value
call schema name receiver arguments =
  fromMaybe (noOverload name (maybe arguments (: arguments) receiver)) case receiver of
    Nothing -> globalCall schema name arguments
    Just target -> asum [textCall name target arguments, validationCall name target arguments, timeCall name target arguments]

globalCall :: Schema -> T.Text -> [Value] -> Maybe (Either CelError Value)
globalCall schema name arguments = case (name, arguments) of
  ("int", [value]) -> Just (toInt value)
  ("uint", [value]) -> Just (toUint value)
  ("double", [value]) -> Just (toDouble value)
  ("string", [value]) -> Just (toText value)
  ("bytes", [StringV text]) -> Just (Right (BytesV (TE.encodeUtf8 text)))
  ("bytes", [BytesV bytes]) -> Just (Right (BytesV bytes))
  ("bool", [BoolV flag]) -> Just (Right (BoolV flag))
  ("bool", [StringV text]) -> Just (toBool text)
  ("dyn", [value]) -> Just (Right value)
  ("type", [value]) -> Just (Right (TypeV (typeOf value)))
  ("duration", [StringV text]) -> Just (maybe (Left (InvalidArgument ("invalid duration: " <> text))) (Right . DurationV) (parseDuration text))
  ("duration", [DurationV nanos]) -> Just (Right (DurationV nanos))
  ("timestamp", [StringV text]) -> Just (maybe (Left (InvalidArgument ("invalid timestamp: " <> text))) inRange (parseTimestamp text))
  ("timestamp", [IntV seconds]) -> Just (inRange (toInteger seconds * 1000000000))
  ("timestamp", [TimestampV nanos]) -> Just (Right (TimestampV nanos))
  ("size", [value]) -> Just (sized value)
  ("matches", [StringV text, StringV source]) -> Just (matching text source)
  ("getField", [target, StringV field]) -> Just (selected schema target field)
  ("strings.quote", [StringV text]) -> Just (Right (StringV (quoted text)))
  _ -> Nothing

toBool :: T.Text -> Either CelError Value
toBool text
  | text `elem` ["1", "t", "T", "TRUE", "true", "True"] = Right (BoolV True)
  | text `elem` ["0", "f", "F", "FALSE", "false", "False"] = Right (BoolV False)
  | otherwise = Left (InvalidArgument ("type conversion error from 'string' to 'bool': " <> text))

inRange :: Integer -> Either CelError Value
inRange nanos
  | seconds < minimumSeconds || seconds > maximumSeconds = Left TimestampOverflow
  | otherwise = Right (TimestampV nanos)
 where
  (seconds, _) = floorQuotRem nanos 1000000000

sized :: Value -> Either CelError Value
sized = \case
  StringV text -> Right (IntV (fromIntegral (T.length text)))
  BytesV bytes -> Right (IntV (fromIntegral (BS.length bytes)))
  ListV values -> Right (IntV (fromIntegral (length values)))
  MapV pairs -> Right (IntV (fromIntegral (Map.size pairs)))
  other -> noOverload "size" [other]

matching :: T.Text -> T.Text -> Either CelError Value
matching text source = BoolV . (`matchesPattern` text) <$> compilePattern source

textCall :: T.Text -> Value -> [Value] -> Maybe (Either CelError Value)
textCall name receiver arguments = case (name, receiver, arguments) of
  ("size", value, []) -> Just (sized value)
  ("matches", StringV text, [StringV source]) -> Just (matching text source)
  ("contains", StringV text, [StringV part]) -> Just (Right (BoolV (part `T.isInfixOf` text)))
  ("contains", BytesV bytes, [BytesV part]) -> Just (Right (BoolV (part `BS.isInfixOf` bytes)))
  ("startsWith", StringV text, [StringV part]) -> Just (Right (BoolV (part `T.isPrefixOf` text)))
  ("startsWith", BytesV bytes, [BytesV part]) -> Just (Right (BoolV (part `BS.isPrefixOf` bytes)))
  ("endsWith", StringV text, [StringV part]) -> Just (Right (BoolV (part `T.isSuffixOf` text)))
  ("endsWith", BytesV bytes, [BytesV part]) -> Just (Right (BoolV (part `BS.isSuffixOf` bytes)))
  ("charAt", StringV text, [IntV position])
    | position < 0 || position > fromIntegral (T.length text) -> Just (Left IndexOutOfRange)
    | otherwise -> Just (Right (StringV (T.take 1 (T.drop (fromIntegral position) text))))
  ("indexOf", StringV text, [StringV part]) -> Just (indexOf text part 0)
  ("indexOf", StringV text, [StringV part, IntV start]) -> Just (indexOf text part start)
  ("lastIndexOf", StringV text, [StringV part]) -> Just (lastIndexOf text part (fromIntegral (T.length text)))
  ("lastIndexOf", StringV text, [StringV part, IntV start]) -> Just (lastIndexOf text part start)
  ("lowerAscii", StringV text, []) -> Just (Right (StringV (T.map (\c -> if isAsciiUpper c then toLower c else c) text)))
  ("upperAscii", StringV text, []) -> Just (Right (StringV (T.map (\c -> if isAsciiLower c then toUpper c else c) text)))
  ("replace", StringV text, [StringV old, StringV new]) -> Just (Right (StringV (replaced text old new (-1))))
  ("replace", StringV text, [StringV old, StringV new, IntV limit]) -> Just (Right (StringV (replaced text old new limit)))
  ("split", StringV text, [StringV separator]) -> Just (Right (ListV (fmap StringV (splitText text separator (-1)))))
  ("split", StringV text, [StringV separator, IntV limit]) -> Just (Right (ListV (fmap StringV (splitText text separator limit))))
  ("substring", StringV text, [IntV start]) -> Just (substring text start (fromIntegral (T.length text)))
  ("substring", StringV text, [IntV start, IntV end]) -> Just (substring text start end)
  ("trim", StringV text, []) -> Just (Right (StringV (T.dropAround isSpace text)))
  ("join", ListV values, rest) | Just separator <- separatorOf rest -> Just (joined values separator)
  ("reverse", StringV text, []) -> Just (Right (StringV (T.reverse text)))
  ("format", StringV template, [ListV values]) -> Just (StringV <$> formatString template values)
  ("unique", ListV values, []) -> Just (BoolV <$> unique values)
  _ -> Nothing
 where
  separatorOf = \case
    [] -> Just ""
    [StringV separator] -> Just separator
    _ -> Nothing
  joined values separator = case traverse textOf values of
    Just pieces -> Right (StringV (T.intercalate separator pieces))
    Nothing -> noOverload "join" values
  textOf = \case
    StringV text -> Just text
    _ -> Nothing

validationCall :: T.Text -> Value -> [Value] -> Maybe (Either CelError Value)
validationCall name receiver arguments =
  Right . BoolV <$> case (name, receiver, arguments) of
    ("isNan", DoubleV number, []) -> Just (isNaN number)
    ("isInf", DoubleV number, []) -> Just (isInfinite number)
    ("isInf", DoubleV number, [IntV sign]) -> Just (isInfinite number && (sign == 0 || signum number == fromIntegral (signum sign)))
    ("isHostname", StringV text, []) -> Just (isHostname text)
    ("isEmail", StringV text, []) -> Just (isEmail text)
    ("isIp", StringV text, []) -> Just (isIp text 0)
    ("isIp", StringV text, [IntV version]) -> Just (isIp text version)
    ("isIpPrefix", StringV text, []) -> Just (isIpPrefix text 0 False)
    ("isIpPrefix", StringV text, [IntV version]) -> Just (isIpPrefix text version False)
    ("isIpPrefix", StringV text, [BoolV strict]) -> Just (isIpPrefix text 0 strict)
    ("isIpPrefix", StringV text, [IntV version, BoolV strict]) -> Just (isIpPrefix text version strict)
    ("isUri", StringV text, []) -> Just (isUri text)
    ("isUriRef", StringV text, []) -> Just (isUriReference text)
    ("isHostAndPort", StringV text, [BoolV portRequired]) -> Just (isHostAndPort text portRequired)
    _ -> Nothing

timeCall :: T.Text -> Value -> [Value] -> Maybe (Either CelError Value)
timeCall name receiver arguments = do
  component <- componentNamed name
  case (receiver, arguments) of
    (TimestampV nanos, []) -> Just (fromComponent (timestampComponent component Nothing nanos))
    (TimestampV nanos, [StringV zone]) -> Just (fromComponent (timestampComponent component (Just zone) nanos))
    (DurationV nanos, []) -> Right . IntV <$> durationComponent component nanos
    _ -> Nothing
 where
  fromComponent = either (Left . InvalidArgument) (Right . IntV)

componentNamed :: T.Text -> Maybe Component
componentNamed = \case
  "getFullYear" -> Just FullYear
  "getMonth" -> Just Month
  "getDayOfYear" -> Just DayOfYear
  "getDayOfMonth" -> Just DayOfMonth
  "getDate" -> Just Date
  "getDayOfWeek" -> Just DayOfWeek
  "getHours" -> Just Hours
  "getMinutes" -> Just Minutes
  "getSeconds" -> Just Seconds
  "getMilliseconds" -> Just Milliseconds
  _ -> Nothing

toInt :: Value -> Either CelError Value
toInt = \case
  IntV number -> Right (IntV number)
  UintV number
    | number > fromIntegral (maxBound :: Int64) -> Left IntOverflow
    | otherwise -> Right (IntV (fromIntegral number))
  DoubleV number
    | isNaN number || isInfinite number || number <= -9.223372036854775808e18 || number >= 9.223372036854775807e18 -> Left IntOverflow
    | otherwise -> Right (IntV (truncate number))
  StringV text -> case signedDecimal text of
    Just number | number >= toInteger (minBound :: Int64) && number <= toInteger (maxBound :: Int64) -> Right (IntV (fromInteger number))
    _ -> Left (InvalidArgument ("cannot convert '" <> text <> "' to int"))
  TimestampV nanos -> Right (IntV (fromInteger (fst (floorQuotRem nanos 1000000000))))
  other -> noOverload "int" [other]

toUint :: Value -> Either CelError Value
toUint = \case
  UintV number -> Right (UintV number)
  IntV number
    | number < 0 -> Left UintOverflow
    | otherwise -> Right (UintV (fromIntegral number))
  DoubleV number
    | isNaN number || isInfinite number || number < 0 || number >= 1.8446744073709551616e19 -> Left UintOverflow
    | otherwise -> Right (UintV (truncate number))
  StringV text -> case unsignedDecimal text of
    Just number | number <= toInteger (maxBound :: Word64) -> Right (UintV (fromInteger number))
    _ -> Left (InvalidArgument ("cannot convert '" <> text <> "' to uint"))
  other -> noOverload "uint" [other]

toDouble :: Value -> Either CelError Value
toDouble = \case
  DoubleV number -> Right (DoubleV number)
  IntV number -> Right (DoubleV (fromIntegral number))
  UintV number -> Right (DoubleV (fromIntegral number))
  StringV text -> maybe (Left (InvalidArgument ("cannot convert '" <> text <> "' to double"))) (Right . DoubleV) (parseDouble text)
  other -> noOverload "double" [other]

toText :: Value -> Either CelError Value
toText = \case
  StringV text -> Right (StringV text)
  BoolV flag -> Right (StringV (if flag then "true" else "false"))
  IntV number -> Right (StringV (T.pack (show number)))
  UintV number -> Right (StringV (T.pack (show number)))
  DoubleV number -> Right (StringV (generalDouble number))
  BytesV bytes -> case TE.decodeUtf8' bytes of
    Right text -> Right (StringV text)
    Left _ -> Left (InvalidArgument "invalid UTF-8 in bytes, cannot convert to string")
  TimestampV nanos -> Right (StringV (renderTimestamp nanos))
  DurationV nanos -> Right (StringV (renderDuration nanos))
  other -> noOverload "string" [other]

signedDecimal :: T.Text -> Maybe Integer
signedDecimal text = case T.uncons text of
  Just ('-', digits) -> negate <$> unsignedDecimal digits
  Just ('+', digits) -> unsignedDecimal digits
  _ -> unsignedDecimal text

unsignedDecimal :: T.Text -> Maybe Integer
unsignedDecimal text
  | not (T.null text) && T.all isDigit text = Just (T.foldl' (\acc digit -> acc * 10 + toInteger (digitValue digit)) 0 text)
  | otherwise = Nothing

parseDouble :: T.Text -> Maybe Double
parseDouble text = case T.toLower unsigned of
  "inf" -> Just (sign (1 / 0))
  "infinity" -> Just (sign (1 / 0))
  "nan" -> Just (0 / 0)
  _ -> sign . fromRational <$> decimal unsigned
 where
  (negative, unsigned) = case T.uncons text of
    Just ('-', rest) -> (True, rest)
    Just ('+', rest) -> (False, rest)
    _ -> (False, text)
  sign = if negative then negate else id
  decimal written = do
    let (mantissa, exponentPart) = T.break (\c -> c == 'e' || c == 'E') written
        (whole, fraction) = T.break (== '.') mantissa
        decimals = T.drop 1 fraction
    if T.null (whole <> decimals) || not (T.all isDigit whole) || not (T.all isDigit decimals)
      then Nothing
      else do
        scale10 <- if T.null exponentPart then Just 0 else signedDecimal (T.drop 1 exponentPart)
        mantissaValue <- unsignedDecimal (if T.null (whole <> decimals) then "0" else whole <> decimals)
        let power = scale10 - toInteger (T.length decimals)
        if abs power > 400
          then Just (if power > 0 && mantissaValue /= 0 then 1.0e400 else 0)
          else Just (toRational mantissaValue * 10 ^^ power)

unique :: [Value] -> Either CelError Bool
unique values = go Set.empty values
 where
  go _ [] = Right True
  go seen (value : rest) = case keyed value of
    Nothing -> noOverload "unique" values
    Just Nothing -> go seen rest
    Just (Just key)
      | key `Set.member` seen -> Right False
      | otherwise -> go (Set.insert key seen) rest
  keyed = \case
    BoolV flag -> Just (Just (UniqueBool flag))
    IntV number -> Just (Just (UniqueInt number))
    UintV number -> Just (Just (UniqueUint number))
    DoubleV number
      | isNaN number -> Just Nothing
      | otherwise -> Just (Just (UniqueDouble (if number == 0 then 0 else number)))
    StringV text -> Just (Just (UniqueText text))
    BytesV bytes -> Just (Just (UniqueBytes bytes))
    _ -> Nothing

data UniqueKey
  = UniqueBool Bool
  | UniqueInt Int64
  | UniqueUint Word64
  | UniqueDouble Double
  | UniqueText T.Text
  | UniqueBytes BS.ByteString
  deriving stock (Eq, Ord)

indexOf :: T.Text -> T.Text -> Int64 -> Either CelError Value
indexOf text part start
  | start < 0 || start > fromIntegral (T.length text) = Left IndexOutOfRange
  | T.null part = Right (IntV start)
  | otherwise =
      let rest = T.drop (fromIntegral start) text
          (before, after) = T.breakOn part rest
       in Right (IntV (if T.null after then -1 else start + fromIntegral (T.length before)))

lastIndexOf :: T.Text -> T.Text -> Int64 -> Either CelError Value
lastIndexOf text part start
  | start < 0 || start > fromIntegral (T.length text) = Left IndexOutOfRange
  | T.null part = Right (IntV start)
  | otherwise =
      let candidates = [position | position <- [0 .. min start (fromIntegral (T.length text - T.length part))], part `T.isPrefixOf` T.drop (fromIntegral position) text]
       in Right (IntV (if null candidates then -1 else foldl' max (-1) candidates))

replaced :: T.Text -> T.Text -> T.Text -> Int64 -> T.Text
replaced text old new limit
  | limit == 0 = text
  | T.null old = T.concat (interleave (fmap T.singleton (T.unpack text)))
  | limit < 0 = T.replace old new text
  | otherwise = go limit text
 where
  interleave pieces = new : concatMap (\piece -> [piece, new]) pieces
  go 0 rest = rest
  go remaining rest =
    let (before, after) = T.breakOn old rest
     in if T.null after then rest else before <> new <> go (remaining - 1) (T.drop (T.length old) after)

splitText :: T.Text -> T.Text -> Int64 -> [T.Text]
splitText text separator limit
  | limit == 0 = []
  | limit == 1 = [text]
  | T.null separator = limited (fmap T.singleton (T.unpack text))
  | otherwise = limited (T.splitOn separator text)
 where
  limited pieces
    | limit < 0 = pieces
    | otherwise =
        let (kept, rest) = splitAt (fromIntegral limit - 1) pieces
         in if null rest then kept else kept <> [T.intercalate (if T.null separator then "" else separator) rest]

substring :: T.Text -> Int64 -> Int64 -> Either CelError Value
substring text start end
  | start < 0 || start > size || end < start || end > size = Left IndexOutOfRange
  | otherwise = Right (StringV (T.take (fromIntegral (end - start)) (T.drop (fromIntegral start) text)))
 where
  size = fromIntegral (T.length text)

quoted :: T.Text -> T.Text
quoted text = "\"" <> T.concatMap escape text <> "\""
 where
  escape = \case
    '\a' -> "\\a"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\v' -> "\\v"
    '\\' -> "\\\\"
    '"' -> "\\\""
    c -> T.singleton c

arithmetic :: Operator -> Value -> Value -> Either CelError Value
arithmetic operator left right = case (operator, left, right) of
  (Plus, IntV a, IntV b) -> IntV <$> checkedInt (toInteger a + toInteger b)
  (Plus, UintV a, UintV b) -> UintV <$> checkedUint (toInteger a + toInteger b)
  (Plus, DoubleV a, DoubleV b) -> Right (DoubleV (a + b))
  (Plus, StringV a, StringV b) -> Right (StringV (a <> b))
  (Plus, BytesV a, BytesV b) -> Right (BytesV (a <> b))
  (Plus, ListV a, ListV b) -> Right (ListV (a <> b))
  (Plus, DurationV a, DurationV b) -> DurationV <$> checkedInt (toInteger a + toInteger b)
  (Plus, TimestampV a, DurationV b) -> shifted a (toInteger b)
  (Plus, DurationV a, TimestampV b) -> shifted b (toInteger a)
  (Minus, IntV a, IntV b) -> IntV <$> checkedInt (toInteger a - toInteger b)
  (Minus, UintV a, UintV b) -> UintV <$> checkedUint (toInteger a - toInteger b)
  (Minus, DoubleV a, DoubleV b) -> Right (DoubleV (a - b))
  (Minus, DurationV a, DurationV b) -> DurationV <$> checkedInt (toInteger a - toInteger b)
  (Minus, TimestampV a, TimestampV b) -> DurationV <$> checkedInt (a - b)
  (Minus, TimestampV a, DurationV b)
    | b == minBound -> Left IntOverflow
    | otherwise -> shifted a (negate (toInteger b))
  (Times, IntV a, IntV b) -> IntV <$> checkedInt (toInteger a * toInteger b)
  (Times, UintV a, UintV b) -> UintV <$> checkedUint (toInteger a * toInteger b)
  (Times, DoubleV a, DoubleV b) -> Right (DoubleV (a * b))
  (Divide, IntV a, IntV b)
    | b == 0 -> Left DivideByZero
    | otherwise -> IntV <$> checkedInt (quotient (toInteger a) (toInteger b))
  (Divide, UintV a, UintV b)
    | b == 0 -> Left DivideByZero
    | otherwise -> Right (UintV (quotient a b))
  (Divide, DoubleV a, DoubleV b) -> Right (DoubleV (a / b))
  (Remainder, IntV a, IntV b)
    | b == 0 -> Left ModulusByZero
    | a == minBound && b == -1 -> Left IntOverflow
    | otherwise -> Right (IntV (remainder a b))
  (Remainder, UintV a, UintV b)
    | b == 0 -> Left ModulusByZero
    | otherwise -> Right (UintV (remainder a b))
  _ -> noOverload (operatorName operator) [left, right]
 where
  shifted nanos offset =
    let result = nanos + offset
        (seconds, _) = floorQuotRem result 1000000000
     in if seconds < minimumSeconds || seconds > maximumSeconds then Left TimestampOverflow else Right (TimestampV result)

operatorName :: Operator -> T.Text
operatorName = \case
  Equals -> "_==_"
  NotEquals -> "_!=_"
  Less -> "_<_"
  LessOrEqual -> "_<=_"
  Greater -> "_>_"
  GreaterOrEqual -> "_>=_"
  Within -> "@in"
  Plus -> "_+_"
  Minus -> "_-_"
  Times -> "_*_"
  Divide -> "_/_"
  Remainder -> "_%_"

checkedInt :: Integer -> Either CelError Int64
checkedInt value
  | value < toInteger (minBound :: Int64) || value > toInteger (maxBound :: Int64) = Left IntOverflow
  | otherwise = Right (fromInteger value)

checkedUint :: Integer -> Either CelError Word64
checkedUint value
  | value < 0 || value > toInteger (maxBound :: Word64) = Left UintOverflow
  | otherwise = Right (fromInteger value)

negated :: Value -> Either CelError Value
negated = \case
  IntV number
    | number == minBound -> Left IntOverflow
    | otherwise -> Right (IntV (negate number))
  DoubleV number -> Right (DoubleV (negate number))
  DurationV nanos
    | nanos == minBound -> Left IntOverflow
    | otherwise -> Right (DurationV (negate nanos))
  other -> noOverload "-_" [other]

ordering :: Value -> Value -> Either CelError Ordering
ordering left right = case numericOrdering left right of
  Just result -> result
  Nothing -> case (left, right) of
    (BoolV a, BoolV b) -> Right (compare a b)
    (StringV a, StringV b) -> Right (compare a b)
    (BytesV a, BytesV b) -> Right (compare a b)
    (TimestampV a, TimestampV b) -> Right (compare a b)
    (DurationV a, DurationV b) -> Right (compare a b)
    _ -> noOverload "_<_" [left, right]

member :: Value -> Value -> Either CelError Bool
member element = \case
  ListV values -> Right (any (equal element) values)
  MapV pairs -> Right (isJust (mapLookup element pairs))
  other -> noOverload "@in" [element, other]

mapLookup :: Value -> Map.Map Key Value -> Maybe Value
mapLookup key pairs = case key of
  DoubleV number
    | not (isNaN number || isInfinite number) && fromIntegral (truncate number :: Integer) == number ->
        let whole = truncate number :: Integer
         in asum
              [ if whole >= toInteger (minBound :: Int64) && whole <= toInteger (maxBound :: Int64) then lookupKey (IntK (fromInteger whole)) pairs else Nothing
              , if whole >= 0 && whole <= toInteger (maxBound :: Word64) then lookupKey (UintK (fromInteger whole)) pairs else Nothing
              ]
    | otherwise -> Nothing
  _ -> (`lookupKey` pairs) =<< valueKey key

indexed :: Value -> Value -> Either CelError Value
indexed target position = case target of
  ListV values -> do
    offset <- case position of
      IntV number -> Right (toInteger number)
      UintV number -> Right (toInteger number)
      DoubleV number
        | fromIntegral (truncate number :: Integer) == number -> Right (truncate number)
        | otherwise -> Left (InvalidArgument "unsupported index value")
      other -> noOverload "_[_]" [target, other]
    if offset < 0 || offset >= toInteger (length values)
      then Left IndexOutOfRange
      else case drop (fromInteger offset) values of
        value : _ -> Right value
        [] -> Left IndexOutOfRange
  MapV pairs -> case mapLookup position pairs of
    Just value -> Right value
    Nothing -> Left (NoSuchKey (renderKey position))
  other -> noOverload "_[_]" [other, position]
 where
  renderKey = \case
    StringV text -> text
    IntV number -> T.pack (show number)
    UintV number -> T.pack (show number)
    BoolV flag -> if flag then "true" else "false"
    DoubleV number -> generalDouble number
    other -> typeName (typeOf other)

selected :: Schema -> Value -> T.Text -> Either CelError Value
selected schema target name = case target of
  MessageV message -> case fieldNamed name message.described of
    Just field -> fieldValue schema field message
    Nothing -> Left (NoSuchField name)
  MapV pairs -> case Map.lookup (StringK name) pairs of
    Just value -> Right value
    Nothing -> Left (NoSuchKey name)
  other -> Left (NoSuchOverload ("type '" <> typeName (typeOf other) <> "' does not support field selection"))

present :: Value -> T.Text -> Either CelError Bool
present target name = case target of
  MessageV message -> case fieldNamed name message.described of
    Just field -> Right (has field message)
    Nothing -> Left (NoSuchField name)
  MapV pairs -> Right (Map.member (StringK name) pairs)
  other -> Left (NoSuchOverload ("type '" <> typeName (typeOf other) <> "' does not support field selection"))
