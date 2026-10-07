module Peculiar.Rpc.Routes
  ( routesOf
  ) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Peculiar.Rpc.Catalog (catalogOf)
import Peculiar.Rpc.Json (Registry)
import Peculiar.Rpc.OpenApi (Info, openApi)
import Peculiar.Rpc.Rest (Routes (..), bindingsOf)

routesOf :: Maybe Info -> Registry -> [BS.ByteString] -> [T.Text] -> Routes
routesOf info registry descriptors services =
  Routes
    { bindings
    , catalog
    , registry
    , document = fmap (\described -> LBS.toStrict (encode (openApi described catalog bindings))) info
    }
 where
  catalog = catalogOf descriptors
  bindings = bindingsOf catalog services
