module Peculiar.Rpc.Context
  ( Context (..)
  ) where

import Data.ByteString qualified as BS
import Network.Socket (SockAddr)
import Peculiar.Rpc.Core.Codec (Codec)
import Peculiar.Rpc.Core.Timeout (Timeout)
import Peculiar.Rpc.Metadata (Metadata)

data Context = Context
  { metadata :: Metadata
  , timeout :: Maybe Timeout
  , respondWith :: Metadata -> IO ()
  , trailWith :: Metadata -> IO ()
  , flush :: IO ()
  , peer :: Maybe SockAddr
  , requestPath :: BS.ByteString
  , requestCodec :: Codec
  , requestBytes :: Maybe BS.ByteString
  }
