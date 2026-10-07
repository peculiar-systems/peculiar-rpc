module Peculiar.Rpc.Json.Scalar
  ( encodeScalar
  , parseScalar
  , encodeKey
  , parseKey
  , numeral
  ) where

import Data.Aeson (Value (..))
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char (isDigit)
import Data.Int (Int32, Int64)
import Data.ProtoLens (MessageEnum (..), ScalarField (..))
import Data.Scientific (Scientific, floatingOrInteger, fromFloatDigits, scientificP, toBoundedInteger, toBoundedRealFloat)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import Peculiar.Rpc.Json.Error (JsonError, Problem (..), failure)
import Text.ParserCombinators.ReadP (eof, readP_to_S)
import Text.Read (readMaybe)

encodeScalar :: Bool -> ScalarField v -> v -> Value
encodeScalar nullValue = \case
  EnumField -> \value -> if nullValue then Null else enumJson value
  Int32Field -> Number . fromIntegral
  SInt32Field -> Number . fromIntegral
  SFixed32Field -> Number . fromIntegral
  UInt32Field -> Number . fromIntegral
  Fixed32Field -> Number . fromIntegral
  Int64Field -> String . T.pack . show
  SInt64Field -> String . T.pack . show
  SFixed64Field -> String . T.pack . show
  UInt64Field -> String . T.pack . show
  Fixed64Field -> String . T.pack . show
  FloatField -> floating
  DoubleField -> floating
  BoolField -> Bool
  StringField -> String
  BytesField -> String . TE.decodeUtf8 . Base64.encode

enumJson :: (MessageEnum v) => v -> Value
enumJson value = case readMaybe @Int32 name of
  Just number -> Number (fromIntegral number)
  Nothing -> String (T.pack name)
 where
  name = showEnum value

floating :: (RealFloat a) => a -> Value
floating value
  | isNaN value = String "NaN"
  | isInfinite value = String (if value > 0 then "Infinity" else "-Infinity")
  | otherwise = Number (fromFloatDigits value)

parseScalar :: Bool -> ScalarField v -> Value -> Either JsonError v
parseScalar nullValue = \case
  EnumField -> parseEnum nullValue
  Int32Field -> integral @Int32
  SInt32Field -> integral @Int32
  SFixed32Field -> integral @Int32
  UInt32Field -> integral @Word32
  Fixed32Field -> integral @Word32
  Int64Field -> integral @Int64
  SInt64Field -> integral @Int64
  SFixed64Field -> integral @Int64
  UInt64Field -> integral @Word64
  Fixed64Field -> integral @Word64
  FloatField -> fractional @Float
  DoubleField -> fractional @Double
  BoolField -> \case
    Bool value -> Right value
    _ -> failure (Expected "true or false")
  StringField -> \case
    String value -> Right value
    _ -> failure (Expected "a string")
  BytesField -> \case
    String value -> bytes value
    _ -> failure (Expected "a base64 string")

integral :: (Integral i, Bounded i) => Value -> Either JsonError i
integral = \case
  Number value -> bounded value
  String text -> maybe (failure (Expected "an integer")) bounded (numeral text)
  _ -> failure (Expected "an integer")
 where
  bounded value = case toBoundedInteger value of
    Just number -> Right number
    Nothing -> case floatingOrInteger @Double @Integer value of
      Left _ -> failure (Expected "an integer")
      Right _ -> failure OutOfRange

fractional :: (RealFloat a) => Value -> Either JsonError a
fractional = \case
  Number value -> finite value
  String "NaN" -> Right (0 / 0)
  String "Infinity" -> Right (1 / 0)
  String "-Infinity" -> Right (negate (1 / 0))
  String text -> maybe (failure (Expected "a number")) finite (numeral text)
  _ -> failure (Expected "a number")
 where
  finite value = case toBoundedRealFloat value of
    Right number | not (isInfinite number) -> Right number
    _ -> failure OutOfRange

numeral :: T.Text -> Maybe Scientific
numeral text
  | wellFormed (T.unpack text) = case readP_to_S (scientificP <* eof) (T.unpack text) of
      [(value, "")] -> Just value
      _ -> Nothing
  | otherwise = Nothing
 where
  wellFormed = \case
    '-' : rest -> unsigned rest
    rest -> unsigned rest
  unsigned = \case
    '0' : rest -> fraction rest
    c : rest | isDigit c -> fraction (dropWhile isDigit rest)
    _ -> False
  fraction = \case
    '.' : c : rest | isDigit c -> power (dropWhile isDigit rest)
    rest -> power rest
  power = \case
    e : rest | e `elem` ['e', 'E'] -> signedDigits rest
    rest -> null rest
  signedDigits = \case
    s : rest | s `elem` ['+', '-'] -> digits rest
    rest -> digits rest
  digits = \case
    c : rest | isDigit c -> all isDigit rest
    _ -> False

parseEnum :: (MessageEnum v) => Bool -> Value -> Either JsonError v
parseEnum nullValue = \case
  Null | nullValue -> maybe (failure (Expected "NULL_VALUE")) Right (maybeToEnum 0)
  String name -> maybe (failure (UnknownEnum name)) Right (readEnum (T.unpack name))
  Number value -> case toBoundedInteger @Int32 value of
    Just number -> maybe (failure (UnknownEnum (T.pack (show number)))) Right (maybeToEnum (fromIntegral number))
    Nothing -> failure (Expected "an enum number")
  _ -> failure (Expected "an enum name or number")

bytes :: T.Text -> Either JsonError BS.ByteString
bytes text
  | T.all standard normalised = Right (Base64.decodeLenient (TE.encodeUtf8 padded))
  | otherwise = invalid
 where
  normalised = T.map urlSafe (T.dropWhileEnd (== '=') text)
  padded = normalised <> T.replicate ((4 - T.length normalised .&. 3) .&. 3) "="
  urlSafe = \case
    '-' -> '+'
    '_' -> '/'
    c -> c
  standard c = c `elem` ['A' .. 'Z'] || c `elem` ['a' .. 'z'] || isDigit c || c == '+' || c == '/'
  invalid = failure (Malformed "not valid base64")

encodeKey :: ScalarField v -> v -> T.Text
encodeKey kind value = case encodeScalar False kind value of
  String text -> text
  Bool True -> "true"
  Bool False -> "false"
  Number number -> T.pack (either (show @Double) (show @Integer) (floatingOrInteger number))
  _ -> ""

parseKey :: ScalarField v -> T.Text -> Either JsonError v
parseKey kind text = case kind of
  BoolField -> case text of
    "true" -> Right True
    "false" -> Right False
    _ -> failure (Expected "true or false")
  StringField -> Right text
  _ -> parseScalar False kind (String text)
