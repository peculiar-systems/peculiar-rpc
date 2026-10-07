module Test.Peculiar.Rpc.Harness
  ( withEcho
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString qualified as BS
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Codec (codecName)
import Peculiar.Rpc.Core.Cors (permissive)
import Peculiar.Rpc.Health (Statuses, health, newStatuses)
import Peculiar.Rpc.Mount (Calls (..), defaultCalls)
import Peculiar.Rpc.Reflection (withReflectionFrom)
import Peculiar.Rpc.Serve (Settings (..), defaultSettings, serveEndpoints)
import Peculiar.Rpc.Service (endpoint)
import Test.Peculiar.Rpc.Echo (echo)

refusing :: Rpc.Interceptor
refusing = Rpc.Interceptor \route context continue -> case Rpc.lookupHeader "x-refuse" context.metadata of
  Just _ -> Rpc.throwRpc Rpc.PermissionDenied ("refused before " <> route.service <> "/" <> route.method)
  Nothing -> continue context

echoing :: Rpc.Interceptor
echoing = Rpc.Interceptor \_ context continue -> case Rpc.lookupHeader "x-echo-request" context.metadata of
  Just _ -> do
    context.respondWith (Rpc.header "x-path" context.requestPath <> Rpc.header "x-codec" (codecName context.requestCodec) <> foldMap (Rpc.header "x-request-bin") context.requestBytes)
    continue context
  Nothing -> continue context

withEcho :: (Int -> Statuses -> IO a) -> IO a
withEcho use = do
  bound <- newEmptyMVar
  statuses <- newStatuses
  descriptors <- BS.readFile "gen/echo.descriptors"
  let settings =
        defaultSettings
          { port = 0
          , onListening = putMVar bound
          , calls = defaultCalls{cors = Just permissive, receiveLimit = Just 1024, report = const (pure ()), interceptor = refusing <> echoing}
          }
  withAsync (serveEndpoints settings (withReflectionFrom [descriptors] [endpoint echo, health statuses])) \_ -> takeMVar bound >>= \port -> use port statuses
