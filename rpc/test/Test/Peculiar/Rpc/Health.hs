{-# LANGUAGE TemplateHaskell #-}

module Test.Peculiar.Rpc.Health (tests) where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, evalEither, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Derive (deriveClient)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Handler (defaultOptions)
import Peculiar.Rpc.Health (Serving (..), Statuses, setServing)
import Peculiar.Rpc.Service (Client (..))
import Proto.Grpc.Health.V1.Health

deriveClient ''Health

tests :: Int -> Statuses -> Group
tests port statuses =
  Group
    "Health"
    [ ("the server as a whole is serving", withTests 1 (whole port))
    , ("a service set not serving reports it", withTests 1 (notServing port statuses))
    , ("an unknown service is NOT_FOUND", withTests 1 (unknown port))
    , ("watching starts with the current status", withTests 1 (watching port))
    ]

checking :: Int -> T.Text -> IO (Either RpcError HealthCheckResponse)
checking port service =
  Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port \transport -> do
    let remote = client defaultOptions transport :: HealthClient
    try (remote.check (defMessage & #service .~ service))

whole :: Int -> Property
whole port = property do
  reply <- liftIO (checking port "") >>= evalEither . first show
  reply ^. #status === HealthCheckResponse'SERVING

notServing :: Int -> Statuses -> Property
notServing port statuses = property do
  liftIO (setServing statuses "peculiar.test.v1.Echo" NotServing)
  reply <- liftIO (checking port "peculiar.test.v1.Echo") >>= evalEither . first show
  reply ^. #status === HealthCheckResponse'NOT_SERVING

unknown :: Int -> Property
unknown port = property do
  outcome <- liftIO (checking port "nothing.here")
  case outcome of
    Left failed -> failed.status.code === NotFound
    Right reply -> evalEither (Left (show reply))

watching :: Int -> Property
watching port = property do
  initial <- liftIO $ Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port \transport -> do
    let remote = client defaultOptions transport :: HealthClient
    seen <- newEmptyMVar
    race (takeMVar seen) (remote.watch defMessage (void . tryPutMVar seen . (^. #status)))
  initial === Left HealthCheckResponse'SERVING
