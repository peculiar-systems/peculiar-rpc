module Test.Peculiar.Rpc.Middleware (tests) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, property, withTests, (===))
import Lens.Family2 ((&), (.~))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Client.Exchange (Body (..), Exchange (..), Exchanger (..), Outbound (..))
import Peculiar.Rpc.Client.Http2 (http2)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Frame (encodeStrict, message)
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Service (Client (..))
import Proto.Peculiar.Test.V1.Echo (SayRequest, SayResponse)
import System.Process (readProcess)
import Test.Peculiar.Rpc.Beside (app, local, quiet, warping)
import Test.Peculiar.Rpc.Echo (echo)
import Test.Peculiar.Rpc.Library (LibraryClient (..), library)

tests :: Group
tests =
  Group
    "Middleware"
    [ ("the middleware sends grpc-status as an HTTP/2 trailer", withTests 1 trailered)
    , ("the middleware hands a gRPC call to an unknown method to the app", withTests 1 unknownToApp)
    , ("the middleware refuses a request that breaks a rule with INVALID_ARGUMENT", withTests 1 validated)
    , ("the application alone answers an unknown path UNIMPLEMENTED or 404", withTests 1 alone)
    ]

mountedEcho :: (Int -> IO a) -> IO a
mountedEcho use = Rpc.withServer quiet [Rpc.endpoint echo] \rpc -> warping (Rpc.middleware rpc app) use

data Answered = Answered
  { status :: Int
  , headers :: [Header]
  , body :: BS.ByteString
  , trailers :: [Header]
  }

grpcCall :: Int -> BS.ByteString -> BS.ByteString -> IO Answered
grpcCall port path request =
  Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> do
    let Exchanger exchange = http2 connection
        outbound = Outbound{method = "POST", path, headers = [("content-type", "application/grpc"), ("te", "trailers")], body = Whole (encodeStrict (message request))}
    exchange outbound \answer -> do
      body <- drained answer.next
      trailers <- answer.trailers
      pure Answered{status = answer.status, headers = answer.headers, body, trailers}
 where
  drained next = next >>= \chunk -> if BS.null chunk then pure mempty else (chunk <>) <$> drained next

trailered :: Property
trailered = property do
  answered <- liftIO $ mountedEcho \port -> grpcCall port "/peculiar.test.v1.Echo/Say" (encodeMessage (defMessage & #text .~ "trailed" :: SayRequest))
  (answered.status, lookup "grpc-status" answered.headers, lookup "grpc-status" answered.trailers) === (200, Nothing, Just "0")
  decodeMessage @SayResponse (BS.drop 5 answered.body) === Right (defMessage & #text .~ "trailed")

unknownToApp :: Property
unknownToApp = property do
  answered <- liftIO $ mountedEcho \port -> grpcCall port "/peculiar.test.v1.Echo/Shout" (encodeMessage (defMessage :: SayRequest))
  (answered.status, answered.body) === (200, "the app saw /peculiar.test.v1.Echo/Shout")

validated :: Property
validated = property do
  outcome <- liftIO $ Rpc.withServer quiet [Rpc.endpoint library] \rpc ->
    warping (Rpc.middleware rpc app) \port ->
      Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
        try @RpcError ((client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: LibraryClient).createBook unnumbered)
  either (\failure -> Just failure.status.code) (const Nothing) outcome === Just InvalidArgument
 where
  unnumbered = defMessage & #shelf .~ "poetry" & #book .~ (defMessage & #title .~ "Odes" & #isbn .~ "not-an-isbn")

alone :: Property
alone = property do
  (answered, plain) <- liftIO $ Rpc.withServer quiet [Rpc.endpoint echo] \rpc ->
    warping (Rpc.application rpc) \port -> do
      answered <- grpcCall port "/peculiar.test.v1.Echo/Shout" (encodeMessage (defMessage :: SayRequest))
      plain <- readProcess "curl" ["--silent", "--output", "/dev/null", "--write-out", "%{http_code}", T.unpack (local "http" port "/hello")] ""
      pure (answered, plain)
  lookup "grpc-status" (answered.headers <> answered.trailers) === Just "12"
  plain === "404"
