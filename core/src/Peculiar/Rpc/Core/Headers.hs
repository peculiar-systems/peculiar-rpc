module Peculiar.Rpc.Core.Headers
  ( Header
  , lookupHeader
  , statusFromHeaders
  , statusHeaders
  , isReserved
  , metadataOnly
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Peculiar.Rpc.Core.Base64 qualified as Base64
import Peculiar.Rpc.Core.Code (Code (..), codeFromNumber, codeNumber)
import Peculiar.Rpc.Core.Status (Status (..), decodeStatus, encodeStatus, percentDecode, percentEncode)
import Peculiar.Rpc.Core.Trailers (Header)

lookupHeader :: BS.ByteString -> [Header] -> Maybe BS.ByteString
lookupHeader = lookup

statusFromHeaders :: [Header] -> Maybe Status
statusFromHeaders headers = do
  raw <- lookupHeader "grpc-status" headers
  let code = fromMaybe Unknown (readNumber raw >>= codeFromNumber)
      message = maybe "" percentDecode (lookupHeader "grpc-message" headers)
      fromDetails = do
        encoded <- lookupHeader "grpc-status-details-bin" headers
        bytes <- rightToMaybe (Base64.decodeEither encoded)
        rightToMaybe (decodeStatus bytes)
  pure case fromDetails of
    Just detailed -> detailed{code, message}
    Nothing -> Status{code, message, details = []}

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = \case
  Right a -> Just a
  Left _ -> Nothing

readNumber :: BS.ByteString -> Maybe Int
readNumber raw = case BC.readInt raw of
  Just (n, rest) | BS.null rest -> Just n
  _ -> Nothing

statusHeaders :: Status -> [Header]
statusHeaders status =
  [("grpc-status", BC.pack (show (codeNumber status.code)))]
    <> [("grpc-message", percentEncode status.message) | not (T.null status.message)]
    <> [("grpc-status-details-bin", Base64.encodeUnpadded (encodeStatus status)) | not (null status.details)]

isReserved :: BS.ByteString -> Bool
isReserved name =
  BC.isPrefixOf ":" name
    || name
      `elem` [ "content-type"
             , "content-length"
             , "content-encoding"
             , "accept-encoding"
             , "te"
             , "trailer"
             , "transfer-encoding"
             , "connection"
             , "keep-alive"
             , "upgrade"
             , "host"
             , "grpc-status"
             , "grpc-message"
             , "grpc-status-details-bin"
             , "grpc-encoding"
             , "grpc-accept-encoding"
             , "grpc-timeout"
             ]

metadataOnly :: [Header] -> [Header]
metadataOnly = filter (not . isReserved . fst)
