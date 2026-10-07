module Peculiar.Rpc.Conformance.Transport
  ( Cancelled (..)
  , Outcome (..)
  , connected
  , compressionOf
  ) where

import Control.Exception (Exception, Handler (..), catches, throwIO)
import Control.Monad ((<=<))
import Data.ByteString qualified as BS
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((^.))
import Network.HTTP.Client qualified as Http
import Peculiar.Rpc.Client (Security (..), withConnection)
import Peculiar.Rpc.Client.Http1 (web)
import Peculiar.Rpc.Compression (Compression (..))
import Peculiar.Rpc.Core.Client qualified as Wire
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Handler (Transport (..))
import Peculiar.Rpc.Tls.Client (ClientTls (..), Trust (..), webManager)
import Proto.Connectrpc.Conformance.V1.ClientCompat (ClientCompatRequest)
import Proto.Connectrpc.Conformance.V1.Config (HTTPVersion (..), Protocol (..))
import Proto.Connectrpc.Conformance.V1.Config qualified as Config

data Cancelled = Cancelled
  deriving stock (Show)
  deriving anyclass (Exception)

data Outcome
  = Completed
  | Canceled
  | Failed RpcError

connected :: Http.Manager -> ClientCompatRequest -> (Transport -> IO ()) -> IO Outcome
connected manager request use
  | request ^. #httpVersion == HTTP_VERSION_1 = do
      let scheme = maybe "http://" (const "https://") secured
          url = scheme <> request ^. #host <> ":" <> T.pack (show (request ^. #port))
      reaching <- maybe (pure manager) (either (throwIO . userError . T.unpack) pure <=< webManager) secured
      target <- either (throwIO . userError . T.unpack) pure (web reaching protocol url)
      attempt (use (Http1 target))
  | otherwise =
      withConnection (maybe Plaintext Secure secured) (request ^. #host) (fromIntegral (request ^. #port)) \connection ->
        attempt (use (Http2 connection protocol))
 where
  protocol = case request ^. #protocol of
    PROTOCOL_GRPC -> Wire.Grpc
    PROTOCOL_GRPC_WEB -> Wire.GrpcWeb
    _ -> Wire.Connect
  secured
    | BS.null (request ^. #serverTlsCert) = Nothing
    | otherwise =
        Just
          ClientTls
            { trust = TrustPem (request ^. #serverTlsCert)
            , identity = fmap (\creds -> (creds ^. #cert, creds ^. #key)) (request ^. #maybe'clientTlsCreds)
            }

compressionOf :: ClientCompatRequest -> Maybe Compression
compressionOf request = case request ^. #compression of
  Config.COMPRESSION_GZIP -> Just Gzip
  Config.COMPRESSION_DEFLATE -> Just Deflate
  Config.COMPRESSION_SNAPPY -> Just Snappy
  Config.COMPRESSION_ZSTD -> Just Zstd
  Config.COMPRESSION_BR -> Just Brotli
  _ -> Nothing

attempt :: IO () -> IO Outcome
attempt action =
  (Completed <$ action)
    `catches` [ Handler \Cancelled -> pure Canceled
              , Handler \(failure :: RpcError) -> pure (Failed failure)
              ]
