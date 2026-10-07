module Peculiar.Rpc.Conformance
  ( run
  ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Conformance.Delimited (readDelimited, writeDelimited)
import Peculiar.Rpc.Conformance.Service (conformance)
import Peculiar.Rpc.Mount (Calls (..), defaultCalls)
import Peculiar.Rpc.Reflection (withReflection)
import Peculiar.Rpc.Serve (Settings (..), defaultSettings, serveEndpoints)
import Peculiar.Rpc.Service (endpoint)
import Peculiar.Rpc.Tls (requireClients, tlsFromPem)
import Proto.Connectrpc.Conformance.V1.ServerCompat (ServerCompatRequest, ServerCompatResponse)
import System.IO (hSetBinaryMode, stdin, stdout)

run :: IO ()
run = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  request <- maybe (ioError (userError "no server request on stdin")) pure =<< readDelimited @ServerCompatRequest
  let certificate = request ^. #serverCreds . #cert
      clientsOf
        | BS.null (request ^. #clientTlsCert) = id
        | otherwise = requireClients (request ^. #clientTlsCert)
  bound <- newEmptyMVar
  secured <-
    if request ^. #useTls
      then either (ioError . userError . T.unpack) (pure . Just . clientsOf) (tlsFromPem certificate (request ^. #serverCreds . #key))
      else pure Nothing
  let limit = fromIntegral (request ^. #messageReceiveLimit)
      settings =
        defaultSettings
          { host = "127.0.0.1"
          , port = 0
          , calls = defaultCalls{receiveLimit = if limit == 0 then Nothing else Just limit, report = const (pure ())}
          , onListening = putMVar bound
          , tls = secured
          }
  withAsync (serveEndpoints settings (withReflection [endpoint conformance])) \server -> do
    port <- takeMVar bound
    output <- newMVar ()
    writeDelimited output (defMessage & #host .~ "127.0.0.1" & #port .~ fromIntegral port & #pemCert .~ (if request ^. #useTls then certificate else BS.empty) :: ServerCompatResponse)
    wait server
