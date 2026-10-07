module Peculiar.Rpc.Core.Cors
  ( Cors (..)
  , Origins (..)
  , permissive
  , isPreflight
  , preflightHeaders
  , responseHeaders
  , exposedHeaders
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (isJust)
import Peculiar.Rpc.Core.Headers (lookupHeader)
import Peculiar.Rpc.Core.Trailers (Header)

data Origins
  = AnyOrigin
  | OnlyOrigins [BS.ByteString]
  deriving stock (Eq, Show)

data Cors = Cors
  { origins :: Origins
  , credentials :: Bool
  , maxAgeSeconds :: Int
  , expose :: [BS.ByteString]
  }
  deriving stock (Eq, Show)

permissive :: Cors
permissive = Cors{origins = AnyOrigin, credentials = False, maxAgeSeconds = 7200, expose = []}

exposedHeaders :: [BS.ByteString]
exposedHeaders =
  [ "grpc-status"
  , "grpc-message"
  , "grpc-status-details-bin"
  , "grpc-encoding"
  , "connect-content-encoding"
  , "content-encoding"
  ]

isPreflight :: BS.ByteString -> [Header] -> Bool
isPreflight method headers =
  method == "OPTIONS" && isJust (lookupHeader "access-control-request-method" headers)

allowed :: Cors -> [Header] -> Maybe BS.ByteString
allowed cors headers = do
  origin <- lookupHeader "origin" headers
  case cors.origins of
    AnyOrigin -> Just (if cors.credentials then origin else "*")
    OnlyOrigins list
      | origin `elem` list -> Just origin
      | otherwise -> Nothing

vary :: Cors -> [Header]
vary cors = case cors.origins of
  AnyOrigin | not cors.credentials -> []
  _ -> [("vary", "origin")]

credentialHeaders :: Cors -> [Header]
credentialHeaders cors = [("access-control-allow-credentials", "true") | cors.credentials]

preflightHeaders :: Cors -> [Header] -> [Header]
preflightHeaders cors headers = case allowed cors headers of
  Nothing -> vary cors
  Just origin ->
    [ ("access-control-allow-origin", origin)
    , ("access-control-allow-methods", "GET, POST")
    , ("access-control-max-age", BC.pack (show cors.maxAgeSeconds))
    ]
      <> [ ("access-control-allow-headers", requested)
         | Just requested <- [lookupHeader "access-control-request-headers" headers]
         ]
      <> credentialHeaders cors
      <> vary cors

responseHeaders :: Cors -> [Header] -> [Header]
responseHeaders cors headers = case allowed cors headers of
  Nothing -> vary cors
  Just origin ->
    [ ("access-control-allow-origin", origin)
    , ("access-control-expose-headers", BS.intercalate ", " (exposedHeaders <> cors.expose))
    ]
      <> credentialHeaders cors
      <> vary cors
