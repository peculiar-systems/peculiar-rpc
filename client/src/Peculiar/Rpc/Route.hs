module Peculiar.Rpc.Route
  ( Route (..)
  , routeOf
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data Route = Route
  { service :: T.Text
  , method :: T.Text
  }
  deriving stock (Eq, Show)

routeOf :: BS.ByteString -> Route
routeOf path = Route{service = TE.decodeUtf8Lenient service, method = TE.decodeUtf8Lenient (BS.drop 1 method)}
 where
  (service, method) = BC.break (== '/') (BS.dropWhile (== 0x2f) path)
