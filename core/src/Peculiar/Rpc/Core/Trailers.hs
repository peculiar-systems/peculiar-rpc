module Peculiar.Rpc.Core.Trailers
  ( Header
  , encodeBlock
  , decodeBlock
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)

type Header = (BS.ByteString, BS.ByteString)

encodeBlock :: [Header] -> BS.ByteString
encodeBlock = foldMap \(name, value) -> BC.map toLower name <> ":" <> value <> "\r\n"

decodeBlock :: BS.ByteString -> [Header]
decodeBlock = concatMap line . BC.lines
 where
  line raw = case BC.break (== ':') (BC.filter (/= '\r') raw) of
    (name, rest)
      | not (BS.null name)
      , Just (_, value) <- BC.uncons rest ->
          [(BC.map toLower (BC.strip name), BC.strip value)]
    _ -> []
