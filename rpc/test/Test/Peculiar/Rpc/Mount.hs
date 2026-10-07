module Test.Peculiar.Rpc.Mount (tests) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Service (Client (..))
import System.Directory (getTemporaryDirectory)
import System.Process (readProcess, readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Peculiar.Rpc.Beside (app, local, quiet, warping)
import Test.Peculiar.Rpc.Echo (EchoClient (..), echo)

tests :: Group
tests =
  Group
    "Mount"
    [ ("an app beside the RPCs gets every other path, over HTTP/1.1 and h2c", withTests 1 besidePlaintext)
    , ("an app beside the RPCs gets every other path, over TLS with HTTP/1.1 and HTTP/2", withTests 1 besideTls)
    , ("the middleware in a plain warp server serves gRPC, Connect and gRPC-Web", withTests 1 inWarp)
    , ("one connection carries many calls in a row", withTests 1 manyCalls)
    ]

serving :: Maybe Rpc.Tls -> (Int -> IO a) -> IO a
serving tls use = do
  bound <- newEmptyMVar
  stop <- newEmptyMVar
  let settings = Rpc.defaultSettings{Rpc.port = 0, Rpc.tls, Rpc.onListening = putMVar bound, Rpc.stopWhen = readMVar stop, Rpc.calls = quiet, Rpc.beside = Just app}
  withAsync (Rpc.serveEndpoints settings [Rpc.endpoint echo]) \_ ->
    (takeMVar bound >>= use) <* putMVar stop ()

curl :: [T.Text] -> IO (T.Text, T.Text)
curl arguments = do
  output <- T.pack <$> readProcess "curl" (fmap T.unpack (["--silent", "--show-error", "--write-out", "\n%{http_version}"] <> arguments)) ""
  pure case T.lines output of
    [body, version] -> (body, version)
    other -> (T.unlines other, "")

say :: EchoClient -> T.Text -> IO T.Text
say remote text = (^. #text) <$> remote.say (defMessage & #text .~ text)

overHttp1 :: Rpc.Protocol -> Int -> IO Rpc.Transport
overHttp1 protocol port = do
  manager <- Http.newManager Http.defaultManagerSettings
  either (ioError . userError . T.unpack) (pure . Rpc.Http1) (Rpc.web manager protocol (T.pack ("http://127.0.0.1:" <> show port)))

besidePlaintext :: Property
besidePlaintext = property do
  (routes, replies) <- liftIO $ serving Nothing \port -> do
    let url = local "http" port "/hello"
    routes <- traverse curl [["--http1.1", url], ["--http2-prior-knowledge", url]]
    replies <- Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> do
      connect1 <- overHttp1 Rpc.Connect port
      grpcWeb1 <- overHttp1 Rpc.GrpcWeb port
      traverse
        (\transport -> say (client Rpc.defaultOptions transport) "hello")
        [Rpc.Http2 connection Rpc.Grpc, Rpc.Http2 connection Rpc.Connect, connect1, grpcWeb1]
    pure (routes, replies)
  routes === [("the app saw /hello", "1.1"), ("the app saw /hello", "2")]
  replies === replicate 4 "hello"

besideTls :: Property
besideTls = property do
  (routes, replies) <- liftIO do
    directory <- getTemporaryDirectory
    let certificate = directory <> "/peculiar-rpc-mount-certificate.pem"
        key = directory <> "/peculiar-rpc-mount-key.pem"
    _ <-
      readProcessWithExitCode
        "openssl"
        ["req", "-x509", "-newkey", "ec", "-pkeyopt", "ec_paramgen_curve:prime256v1", "-nodes", "-days", "1", "-subj", "/CN=127.0.0.1", "-addext", "subjectAltName=DNS:127.0.0.1,IP:127.0.0.1", "-keyout", key, "-out", certificate]
        ""
    pem <- BS.readFile certificate
    tls <- either (ioError . userError . T.unpack) pure . Rpc.tlsFromPem pem =<< BS.readFile key
    serving (Just tls) \port -> do
      let url = local "https" port
      routes <- traverse curl [["--insecure", "--http1.1", url "/hello"], ["--insecure", "--http2", url "/hello"]]
      connect <-
        curl
          ["--insecure", "--http2", "--header", "content-type: application/json", "--data", "{\"text\":\"over curl\"}", url "/peculiar.test.v1.Echo/Say"]
      native <-
        Rpc.withConnection (Rpc.Secure Rpc.ClientTls{Rpc.trust = Rpc.TrustPem pem, Rpc.identity = Nothing}) "127.0.0.1" port \connection ->
          say (client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc)) "over TLS"
      pure (routes <> [connect], native)
  routes === [("the app saw /hello", "1.1"), ("the app saw /hello", "2"), ("{\"text\":\"over curl\"}", "2")]
  replies === "over TLS"

inWarp :: Property
inWarp = property do
  outcome <- liftIO $ Rpc.withServer quiet [Rpc.endpoint echo] \rpc ->
    warping (Rpc.middleware rpc app) \port -> do
      routes <- traverse curl [["--http1.1", local "http" port "/hello"], ["--http2-prior-knowledge", local "http" port "/hello"]]
      Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> do
        let native = client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: EchoClient
        connect1 <- overHttp1 Rpc.Connect port
        grpcWeb1 <- overHttp1 Rpc.GrpcWeb port
        replies <- traverse (\transport -> say (client Rpc.defaultOptions transport) "mounted") [Rpc.Http2 connection Rpc.Grpc, Rpc.Http2 connection Rpc.Connect, connect1, grpcWeb1]
        seen <- newIORef []
        native.count (defMessage & #upto .~ 3) \reply -> modifyIORef' seen (<> [reply ^. #n])
        counted <- readIORef seen
        failed <- try @RpcError (native.say (defMessage & #text .~ "x" & #failCode .~ 5 & #failMessage .~ "not here"))
        pure (routes, replies, counted, either (\failure -> Just (failure.status.code, failure.status.message)) (const Nothing) failed)
  outcome
    === ( [("the app saw /hello", "1.1"), ("the app saw /hello", "2")]
        , replicate 4 "mounted"
        , [1, 2, 3]
        , Just (NotFound, "not here")
        )

manyCalls :: Property
manyCalls = property do
  stalls <- liftIO $ serving Nothing \port -> do
    connect1 <- overHttp1 Rpc.Connect port
    Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
      traverse
        (stalled . client Rpc.defaultOptions)
        [Rpc.Http2 connection Rpc.Grpc, Rpc.Http2 connection Rpc.Connect, connect1]
  stalls === [Nothing, Nothing, Nothing]
 where
  stalled remote = go (1 :: Int)
   where
    go n
      | n > 500 = pure Nothing
      | otherwise =
          timeout 3_000_000 (say remote "again") >>= \case
            Nothing -> pure (Just n)
            Just _ -> go (n + 1)
