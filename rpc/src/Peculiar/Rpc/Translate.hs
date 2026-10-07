module Peculiar.Rpc.Translate
  ( Translation (..)
  , translationOf
  , jsonDirect
  , jsonStreaming
  ) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.ProtoLens (Message, decodeMessage)
import Data.Proxy (Proxy)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Encoding (encoded)
import Peculiar.Rpc.Error (rpcError)
import Peculiar.Rpc.Json (Codec (..), JsonError, Parsing, codec, render)
import Peculiar.Rpc.Json qualified as Json
import Peculiar.Rpc.Received (Received (..))

data Translation = Translation
  { fromJson :: BS.ByteString -> Either JsonError BS.ByteString
  , toJson :: BS.ByteString -> Either JsonError BS.ByteString
  }

translationOf :: forall i o. (Message i, Message o) => Proxy i -> Proxy o -> Parsing -> Translation
translationOf _ _ parsing =
  Translation
    { fromJson = fmap encoded . inbound.decoded
    , toJson = either (const (Left undecodable)) outbound.encoded . decodeMessage @o
    }
 where
  inbound = codec @i parsing
  outbound = codec @o parsing
  undecodable = Json.JsonError{path = [], problem = Json.Malformed "the handler's reply is not a valid message"}

jsonDirect :: Translation -> (context -> BS.ByteString -> IO BS.ByteString) -> context -> BS.ByteString -> IO BS.ByteString
jsonDirect translation run context bytes = do
  binary <- either (throwIO . rpcError InvalidArgument . render) pure (translation.fromJson bytes)
  reply <- run context binary
  either (throwIO . rpcError Internal . render) pure (translation.toJson reply)

jsonStreaming
  :: Translation
  -> (context -> IO (Maybe Received) -> (BS.ByteString -> IO ()) -> IO ())
  -> context
  -> IO (Maybe Received)
  -> (BS.ByteString -> IO ())
  -> IO ()
jsonStreaming translation run context next send =
  run
    context
    (next >>= traverse \received -> either (throwIO . rpcError InvalidArgument . render) (\binary -> pure Received{raw = received.raw, binary}) (translation.fromJson received.binary))
    (either (throwIO . rpcError Internal . render) send . translation.toJson)
