module Test.Peculiar.Rpc.Shutdown (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as BC
import Data.Either (isRight)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Hedgehog (Group (..), Property, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Service (Client (..))
import System.Timeout (timeout)
import Test.Peculiar.Rpc.Echo (EchoClient (..), echo)

tests :: Group
tests = Group "Shutdown" [("a stopping server finishes its calls and refuses new ones", withTests 1 draining)]

delaying :: Rpc.Interceptor
delaying = Rpc.Interceptor \_ context continue -> do
  case BC.readInt =<< Rpc.lookupHeader "x-delay-ms" context.metadata of
    Just (millis, _) -> threadDelay (millis * 1000)
    Nothing -> pure ()
  continue context

draining :: Property
draining = property do
  (slow, refused, stopped) <- liftIO do
    bound <- newEmptyMVar
    stop <- newEmptyMVar
    let settings =
          Rpc.defaultSettings
            { Rpc.port = 0
            , Rpc.onListening = putMVar bound
            , Rpc.calls = Rpc.defaultCalls{Rpc.report = const (pure ()), Rpc.interceptor = delaying}
            , Rpc.stopWhen = readMVar stop
            , Rpc.grace = Rpc.seconds 5
            }
    server <- async (Rpc.serveEndpoints settings [Rpc.endpoint echo])
    port <- takeMVar bound
    Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port \transport -> do
      let remote options = client options transport :: EchoClient
          slowly = Rpc.defaultOptions{Rpc.requestMetadata = Rpc.header "x-delay-ms" "400"}
      withAsync ((remote slowly).say (defMessage & #text .~ "slow")) \inFlight -> do
        threadDelay 100_000
        putMVar stop ()
        threadDelay 50_000
        refused <- try @RpcError ((remote Rpc.defaultOptions).say (defMessage & #text .~ "late"))
        slow <- wait inFlight
        stopped <- timeout 3_000_000 (waitCatch server)
        pure (slow ^. #text, either (\failed -> failed.status.code) (const Ok) refused, fmap isRight stopped)
  slow === "slow"
  refused === Unavailable
  stopped === Just True
