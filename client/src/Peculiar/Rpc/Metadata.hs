module Peculiar.Rpc.Metadata
  ( Metadata (..)
  , Entry (..)
  , binary
  , entries
  , fromHeaders
  , header
  , headers
  , lookupHeader
  , lookupHeaders
  , wireHeaders
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAsciiLower, isDigit, toLower)
import Data.Either (fromRight)
import Data.List (partition)
import Peculiar.Rpc.Core.Base64 qualified as Base64

data Entry = Entry
  { name :: BS.ByteString
  , value :: BS.ByteString
  }
  deriving stock (Eq, Show)

newtype Metadata = Metadata [Entry]
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

binary :: Entry -> Bool
binary entry = "-bin" `BS.isSuffixOf` entry.name

entries :: Metadata -> [Entry]
entries (Metadata list) = joined list

fromHeaders :: [(BS.ByteString, BS.ByteString)] -> Metadata
fromHeaders = foldMap \(name, value) -> admitted (BC.map toLower name) (onArrival name value)
 where
  onArrival name value
    | "-bin" `BS.isSuffixOf` BC.map toLower name = fromRight value (Base64.decodeEither value)
    | otherwise = value

header :: BS.ByteString -> BS.ByteString -> Metadata
header name = admitted (BC.map toLower name)

admitted :: BS.ByteString -> BS.ByteString -> Metadata
admitted name value
  | validName && validValue = Metadata [entry]
  | otherwise = mempty
 where
  entry = Entry{name, value}
  validName = not (BS.null name) && BC.all nameChar name && (name == "grpc-retry-pushback-ms" || not ("grpc-" `BS.isPrefixOf` name))
  nameChar c = isAsciiLower c || isDigit c || BC.elem c "-_."
  validValue = binary entry || BS.all (\byte -> byte >= 0x20 && byte <= 0x7e) value

headers :: Metadata -> [(BS.ByteString, BS.ByteString)]
headers (Metadata list) = [(entry.name, entry.value) | entry <- list]

lookupHeader :: BS.ByteString -> Metadata -> Maybe BS.ByteString
lookupHeader name metadata = case lookupHeaders name metadata of
  value : _ -> Just value
  [] -> Nothing

lookupHeaders :: BS.ByteString -> Metadata -> [BS.ByteString]
lookupHeaders name metadata = [value | (key, value) <- headers metadata, key == BC.map toLower name]

wireHeaders :: Metadata -> [(BS.ByteString, BS.ByteString)]
wireHeaders = fmap onWire . entries
 where
  onWire entry
    | binary entry = (entry.name, Base64.encodeUnpadded entry.value)
    | otherwise = (entry.name, entry.value)

joined :: [Entry] -> [Entry]
joined = \case
  [] -> []
  entry : rest
    | binary entry -> entry : joined rest
    | otherwise ->
        let (same, others) = partition ((== entry.name) . (.name)) rest
         in entry{value = BS.intercalate "," (fmap (.value) (entry : same))} : joined others
