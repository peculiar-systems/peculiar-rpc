module Peculiar.Rpc.Core.Base64
  ( encodeUnpadded
  , decodeEither
  , decodeUrl
  , encodeUrlUnpadded
  , TextDecoder
  , textDecoder
  , feedText
  , finishText
  , encodeText
  ) where

import Data.Bifunctor (first)
import Data.Bits (complement, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.URL qualified as B64Url
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.Text qualified as T

encodeUrlUnpadded :: BS.ByteString -> BS.ByteString
encodeUrlUnpadded = BC.dropWhileEnd (== '=') . B64Url.encode

encodeUnpadded :: BS.ByteString -> BS.ByteString
encodeUnpadded = BC.dropWhileEnd (== '=') . B64.encode

decodeEither :: BS.ByteString -> Either T.Text BS.ByteString
decodeEither raw = first T.pack (B64.decode (pad (BC.filter (/= ' ') raw)))

decodeUrl :: BS.ByteString -> Either T.Text BS.ByteString
decodeUrl raw = first T.pack (B64Url.decode (pad (BC.map standard raw)))
 where
  standard = \case
    '+' -> '-'
    '/' -> '_'
    c -> c

pad :: BS.ByteString -> BS.ByteString
pad raw = raw <> BC.replicate ((4 - BS.length raw .&. 3) .&. 3) '='

newtype TextDecoder = TextDecoder BS.ByteString

textDecoder :: TextDecoder
textDecoder = TextDecoder BS.empty

feedText :: TextDecoder -> BS.ByteString -> Either T.Text (BS.ByteString, TextDecoder)
feedText (TextDecoder held) chunk =
  let bytes = held <> BC.filter (not . isSpace) chunk
      (ready, rest) = BS.splitAt (BS.length bytes .&. complement 3) bytes
   in (,TextDecoder rest) <$> decodeGroups ready

decodeGroups :: BS.ByteString -> Either T.Text BS.ByteString
decodeGroups bytes = BS.concat <$> traverse decodeEither (segments bytes)
 where
  segments raw
    | BS.null raw = []
    | otherwise = case BC.findIndex (== '=') raw of
        Nothing -> [raw]
        Just at ->
          let (segment, rest) = BS.splitAt (at .&. complement 3 + 4) raw
           in segment : segments rest

finishText :: TextDecoder -> Either T.Text ()
finishText (TextDecoder held)
  | BS.null held = Right ()
  | otherwise = Left "truncated base64"

encodeText :: BS.ByteString -> BS.ByteString
encodeText = B64.encode
