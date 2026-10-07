module Peculiar.Rpc.Core.Status
  ( Status (..)
  , Detail (..)
  , StatusError (..)
  , ok
  , failing
  , encodeStatus
  , decodeStatus
  , percentEncode
  , percentDecode
  ) where

import Data.Bits (shift, testBit, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import Peculiar.Rpc.Core.Code (Code (..), codeFromNumber, codeNumber)

data Detail = Detail
  { typeUrl :: T.Text
  , value :: BS.ByteString
  }
  deriving stock (Eq, Show)

data Status = Status
  { code :: Code
  , message :: T.Text
  , details :: [Detail]
  }
  deriving stock (Eq, Show)

data StatusError
  = MalformedVarint
  | MalformedLength
  | UnknownWireType Word8
  | InvalidUtf8
  | UnknownCode Int
  deriving stock (Eq, Show)

ok :: Status
ok = Status{code = Ok, message = "", details = []}

failing :: Code -> T.Text -> Status
failing code message = Status{code, message, details = []}

encodeStatus :: Status -> BS.ByteString
encodeStatus status =
  LBS.toStrict . toLazyByteString $
    varintField 1 (fromIntegral (codeNumber status.code))
      <> bytesField 2 (TE.encodeUtf8 status.message)
      <> foldMap (element 3 . encodeDetail) status.details

encodeDetail :: Detail -> BS.ByteString
encodeDetail detail =
  LBS.toStrict . toLazyByteString $
    bytesField 1 (TE.encodeUtf8 detail.typeUrl) <> bytesField 2 detail.value

varintField :: Word64 -> Word64 -> Builder
varintField number n
  | n == 0 = mempty
  | otherwise = varint (shift number 3) <> varint n

bytesField :: Word64 -> BS.ByteString -> Builder
bytesField number bytes
  | BS.null bytes = mempty
  | otherwise = element number bytes

element :: Word64 -> BS.ByteString -> Builder
element number bytes = varint (shift number 3 .|. 2) <> varint (fromIntegral (BS.length bytes)) <> byteString bytes

varint :: Word64 -> Builder
varint n
  | n < 0x80 = word8 (fromIntegral n)
  | otherwise = word8 (fromIntegral (n .&. 0x7f) .|. 0x80) <> varint (shift n (-7))

data Field
  = VarintField Word64 Word64
  | BytesField Word64 BS.ByteString
  | OtherField

decodeStatus :: BS.ByteString -> Either StatusError Status
decodeStatus bytes = do
  fields <- decodeFields bytes
  foldl' (\acc field -> acc >>= apply field) (Right ok) fields
 where
  apply field status = case field of
    VarintField 1 n -> case codeFromNumber (fromIntegral n) of
      Just code -> Right status{code}
      Nothing -> Left (UnknownCode (fromIntegral n))
    BytesField 2 raw -> (\message -> status{message}) <$> utf8 raw
    BytesField 3 raw -> (\detail -> status{details = status.details <> [detail]}) <$> decodeDetail raw
    _ -> Right status

decodeDetail :: BS.ByteString -> Either StatusError Detail
decodeDetail bytes = do
  fields <- decodeFields bytes
  foldl' (\acc field -> acc >>= apply field) (Right (Detail "" "")) fields
 where
  apply field detail = case field of
    BytesField 1 raw -> (\typeUrl -> detail{typeUrl}) <$> utf8 raw
    BytesField 2 raw -> Right detail{value = raw}
    _ -> Right detail

utf8 :: BS.ByteString -> Either StatusError T.Text
utf8 = either (const (Left InvalidUtf8)) Right . TE.decodeUtf8'

decodeFields :: BS.ByteString -> Either StatusError [Field]
decodeFields = go []
 where
  go acc bytes
    | BS.null bytes = Right (reverse acc)
    | otherwise = do
        (key, rest) <- readVarint bytes
        let number = shift key (-3)
        case key .&. 7 of
          0 -> do
            (n, after) <- readVarint rest
            go (VarintField number n : acc) after
          1 -> skip 8 rest >>= go (OtherField : acc)
          2 -> do
            (len, after) <- readVarint rest
            let size = fromIntegral len
            if BS.length after < size
              then Left MalformedLength
              else go (BytesField number (BS.take size after) : acc) (BS.drop size after)
          5 -> skip 4 rest >>= go (OtherField : acc)
          other -> Left (UnknownWireType (fromIntegral other))

  skip n bytes
    | BS.length bytes < n = Left MalformedLength
    | otherwise = Right (BS.drop n bytes)

readVarint :: BS.ByteString -> Either StatusError (Word64, BS.ByteString)
readVarint = go 0 0
 where
  go :: Int -> Word64 -> BS.ByteString -> Either StatusError (Word64, BS.ByteString)
  go offset acc bytes
    | offset > 63 = Left MalformedVarint
    | otherwise = case BS.uncons bytes of
        Nothing -> Left MalformedVarint
        Just (byte, rest) ->
          let acc' = acc .|. shift (fromIntegral (byte .&. 0x7f)) offset
           in if testBit byte 7 then go (offset + 7) acc' rest else Right (acc', rest)

percentEncode :: T.Text -> BS.ByteString
percentEncode = BS.concatMap escape . TE.encodeUtf8
 where
  escape byte
    | byte >= 0x20 && byte <= 0x7e && byte /= 0x25 = BS.singleton byte
    | otherwise = BS.pack [0x25, hex (shift byte (-4)), hex (byte .&. 0x0f)]
  hex nibble = if nibble < 10 then 0x30 + nibble else 0x41 + nibble - 10

percentDecode :: BS.ByteString -> T.Text
percentDecode = TE.decodeUtf8Lenient . BS.pack . go . BS.unpack
 where
  go = \case
    0x25 : a : b : rest | Just high <- unhex a, Just low <- unhex b -> (shift high 4 .|. low) : go rest
    byte : rest -> byte : go rest
    [] -> []
  unhex byte
    | byte >= 0x30 && byte <= 0x39 = Just (byte - 0x30)
    | byte >= 0x41 && byte <= 0x46 = Just (byte - 0x41 + 10)
    | byte >= 0x61 && byte <= 0x66 = Just (byte - 0x61 + 10)
    | otherwise = Nothing
