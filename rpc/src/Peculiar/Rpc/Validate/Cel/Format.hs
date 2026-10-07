module Peculiar.Rpc.Validate.Cel.Format
  ( formatString
  , generalDouble
  ) where

import Data.ByteString qualified as BS
import Data.Char (isDigit, toUpper)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (floatToDigits, showHex, showIntAtBase, showOct)
import Peculiar.Rpc.Validate.Arithmetic (digitValue, hexDigit, quotient, remainder)
import Peculiar.Rpc.Validate.Cel.Time (renderDuration, renderTimestamp, shortestDecimal)
import Peculiar.Rpc.Validate.Cel.Value

formatString :: T.Text -> [Value] -> Either CelError T.Text
formatString template arguments = T.concat <$> go (T.unpack template) arguments
 where
  go text remaining = case text of
    [] -> Right []
    '%' : '%' : rest -> ("%" :) <$> go rest remaining
    '%' : rest -> case remaining of
      [] -> Left (InvalidArgument "index out of range in format string")
      argument : others -> do
        (precision, afterPrecision) <- precisionOf rest
        case afterPrecision of
          [] -> Left (InvalidArgument "unexpected end of string")
          verb : afterVerb -> do
            rendered <- clause verb precision argument
            (rendered :) <$> go afterVerb others
    c : rest ->
      let (plain, after) = break (== '%') rest
       in (T.pack (c : plain) :) <$> go after remaining
  precisionOf = \case
    '.' : rest ->
      let (digits, after) = span isDigit rest
       in if null after
            then Left (InvalidArgument "could not find end of precision specifier")
            else
              let precision = foldl' (\acc digit -> acc * 10 + digitValue digit) 0 digits
               in if precision > 100 then Left (InvalidArgument "precision exceeds maximum allowed precision 100") else Right (precision, after)
    rest -> Right (6, rest)

clause :: Char -> Int -> Value -> Either CelError T.Text
clause verb precision argument = case verb of
  's' -> plainString argument
  'd' -> case argument of
    IntV number -> Right (T.pack (show number))
    UintV number -> Right (T.pack (show number))
    DoubleV number -> Right (special number (shortestDecimal number))
    other -> Left (clauseError "decimal clause can only be used on ints, uints, and doubles" other)
  'f' -> withDouble "fixed-point clause can only be used on ints, uints, and doubles" (fixed precision)
  'e' -> withDouble "scientific clause can only be used on ints, uints, and doubles" (scientific precision)
  'b' -> case argument of
    BoolV flag -> Right (if flag then "1" else "0")
    IntV number -> Right (signedBase 2 (toInteger number))
    UintV number -> Right (signedBase 2 (toInteger number))
    other -> Left (clauseError "only ints, uints, and bools can be formatted as binary" other)
  'x' -> hex False
  'X' -> hex True
  'o' -> case argument of
    IntV number -> Right (signedBase 8 (toInteger number))
    UintV number -> Right (signedBase 8 (toInteger number))
    other -> Left (clauseError "octal clause can only be used on ints and uints" other)
  other -> Left (InvalidArgument ("could not parse formatting clause: unrecognized formatting clause \"" <> T.singleton other <> "\""))
 where
  withDouble message render = case argument of
    IntV number -> Right (render (fromIntegral number))
    UintV number -> Right (render (fromIntegral number))
    DoubleV number -> Right (special number (render number))
    other -> Left (clauseError message other)
  hex upper =
    (if upper then T.map toUpper else id) <$> case argument of
      IntV number -> Right (signedBase 16 (toInteger number))
      UintV number -> Right (signedBase 16 (toInteger number))
      StringV text -> Right (bytesHex (TE.encodeUtf8 text))
      BytesV bytes -> Right (bytesHex bytes)
      other -> Left (clauseError "only ints, uints, bytes, and strings can be formatted as hex" other)

special :: Double -> T.Text -> T.Text
special number rendered
  | isNaN number = "NaN"
  | isInfinite number = if number > 0 then "Infinity" else "-Infinity"
  | otherwise = rendered

clauseError :: T.Text -> Value -> CelError
clauseError message argument = InvalidArgument ("error during formatting: " <> message <> ", was given " <> typeName (typeOf argument))

plainString :: Value -> Either CelError T.Text
plainString = \case
  NullV -> Right "null"
  BoolV flag -> Right (if flag then "true" else "false")
  IntV number -> Right (T.pack (show number))
  UintV number -> Right (T.pack (show number))
  DoubleV number -> Right (special number (shortestDecimal number))
  StringV text -> Right text
  BytesV bytes -> Right (TE.decodeUtf8With lenientDecode bytes)
  DurationV nanos -> Right (renderDuration nanos)
  TimestampV nanos -> Right (renderTimestamp nanos)
  TypeV celType -> Right (typeName celType)
  ListV values -> do
    rendered <- traverse plainString values
    Right ("[" <> T.intercalate ", " rendered <> "]")
  MapV pairs -> do
    rendered <- traverse (\(key, value) -> (,) <$> plainString (keyValue key) <*> plainString value) (Map.toList pairs)
    Right ("{" <> T.intercalate ", " [key <> ": " <> value | (key, value) <- sortOn fst rendered] <> "}")
  other -> Left (clauseError "string clause can only be used on strings, bools, bytes, ints, doubles, maps, lists, types, durations, and timestamps" other)

signedBase :: Integer -> Integer -> T.Text
signedBase base number
  | number < 0 = "-" <> T.pack (render (negate number) "")
  | otherwise = T.pack (render number "")
 where
  render = case base of
    16 -> showHex
    8 -> showOct
    _ -> showIntAtBase base hexDigit

bytesHex :: BS.ByteString -> T.Text
bytesHex = T.pack . concatMap (\byte -> [hexDigit (fromIntegral (quotient byte 16)), hexDigit (fromIntegral (remainder byte 16))]) . BS.unpack

negativeSign :: Double -> T.Text
negativeSign number = if number < 0 || isNegativeZero number then "-" else ""

fixed :: Int -> Double -> T.Text
fixed precision number =
  let scaled = round (abs (toRational number) * 10 ^ precision) :: Integer
      digits = T.justifyRight (precision + 1) '0' (T.pack (show scaled))
      (whole, fraction) = T.splitAt (T.length digits - precision) digits
   in negativeSign number <> whole <> (if precision > 0 then "." <> fraction else "")

scientific :: Int -> Double -> T.Text
scientific precision number
  | number == 0 = negativeSign number <> mantissa 0 <> "e+00"
  | otherwise =
      let magnitude = abs (toRational number)
          estimate = snd (floatToDigits 10 (abs number)) - 1
          power = adjust estimate
          adjust guess
            | magnitude >= 10 ^^ (guess + 1) = adjust (guess + 1)
            | magnitude < 10 ^^ guess = adjust (guess - 1)
            | otherwise = guess
          scaled = round (magnitude / 10 ^^ power * 10 ^ precision) :: Integer
          (digits, finalPower) = if scaled >= 10 ^ (precision + 1) then (quotient scaled 10, power + 1) else (scaled, power)
       in negativeSign number <> mantissa digits <> "e" <> (if finalPower < 0 then "-" else "+") <> T.justifyRight 2 '0' (T.pack (show (abs finalPower)))
 where
  mantissa digits =
    let written = T.justifyRight (precision + 1) '0' (T.pack (show digits))
     in T.take 1 written <> (if precision > 0 then "." <> T.drop 1 written else "")

generalDouble :: Double -> T.Text
generalDouble number
  | isNaN number = "NaN"
  | isInfinite number = if number > 0 then "+Inf" else "-Inf"
  | number == 0 = negativeSign number <> "0"
  | otherwise =
      let (digits, point) = floatToDigits 10 (abs number)
          power = point - 1
          written = T.pack (concatMap show digits)
       in negativeSign number
            <> if power < -4 || power >= 6
              then
                T.take 1 written
                  <> (if T.length written > 1 then "." <> T.drop 1 written else "")
                  <> "e"
                  <> (if power < 0 then "-" else "+")
                  <> T.justifyRight 2 '0' (T.pack (show (abs power)))
              else shortestDecimal (abs number)
