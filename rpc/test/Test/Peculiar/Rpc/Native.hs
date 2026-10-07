module Test.Peculiar.Rpc.Native (tests) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, evalEither, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Handler (defaultOptions)
import Peculiar.Rpc.Service (Client (..))
import Test.Peculiar.Rpc.Echo (EchoClient (..))

tests :: Int -> Group
tests port =
  Group
    "Native"
    [ ("a derived client makes unary calls over h2c", withTests 1 (unary port))
    , ("server streaming arrives in order", withTests 1 (serverStreaming port))
    , ("client streaming folds every message", withTests 1 (clientStreaming port))
    , ("bidi streaming answers each message", withTests 1 (bidi port))
    , ("a handler's error reaches the client typed", withTests 1 (failure port))
    , ("request metadata reaches the handler", withTests 1 (metadata port))
    ]

connected :: Int -> (EchoClient -> IO a) -> IO a
connected port use =
  Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port (use . client defaultOptions)

collecting :: ((a -> IO ()) -> IO ()) -> IO [a]
collecting run = do
  seen <- newIORef []
  run \item -> modifyIORef' seen (item :)
  reverse <$> readIORef seen

unary :: Int -> Property
unary port = property do
  reply <- liftIO $ connected port \echo -> echo.say (defMessage & #text .~ "native")
  reply ^. #text === "native"

serverStreaming :: Int -> Property
serverStreaming port = property do
  numbers <- liftIO $ connected port \echo -> collecting (echo.count (defMessage & #upto .~ 4))
  fmap (^. #n) numbers === [1, 2, 3, 4]

clientStreaming :: Int -> Property
clientStreaming port = property do
  reply <- liftIO $ connected port \echo ->
    echo.sum \send -> for_ [1 .. 10] \n -> send (defMessage & #n .~ n)
  reply ^. #total === 55

bidi :: Int -> Property
bidi port = property do
  replies <- liftIO $ connected port \echo ->
    collecting (echo.chat \send -> for_ ["a", "b", "c"] \text -> send (defMessage & #text .~ text))
  fmap (^. #text) replies === ["A", "B", "C"]

failure :: Int -> Property
failure port = property do
  outcome <- liftIO $ connected port \echo ->
    try @RpcError (echo.say (defMessage & #failCode .~ 5 & #failMessage .~ "gone"))
  case outcome of
    Left failed -> (failed.status.code, failed.status.message) === (NotFound, "gone")
    Right _ -> evalEither (Left @T.Text "the call succeeded")

metadata :: Int -> Property
metadata port = property do
  reply <- liftIO $ Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port \transport -> do
    let echo = client defaultOptions{Rpc.requestMetadata = Rpc.header "Authorization" "Bearer token"} transport :: EchoClient
    echo.say (defMessage & #text .~ "whoami")
  reply ^. #text === "Bearer token"
