{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Health
  ( Statuses
  , Serving (..)
  , newStatuses
  , setServing
  , health
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Exception (throwIO)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Context (Context)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Derive (deriveServer)
import Peculiar.Rpc.Error (rpcError)
import Peculiar.Rpc.Service (Endpoint, endpoint)
import Proto.Grpc.Health.V1.Health

deriveServer ''Health

data Serving = Serving | NotServing
  deriving stock (Eq, Show, Enum, Bounded)

newtype Statuses = Statuses (TVar (Map.Map T.Text Serving))

newStatuses :: IO Statuses
newStatuses = Statuses <$> newTVarIO (Map.singleton "" Serving)

setServing :: Statuses -> T.Text -> Serving -> IO ()
setServing (Statuses statuses) service serving = atomically (modifyTVar' statuses (Map.insert service serving))

health :: Statuses -> Endpoint
health state = endpoint HealthServer{check = checked state, watch = watched state}

checked :: Statuses -> Context -> HealthCheckRequest -> IO HealthCheckResponse
checked (Statuses statuses) _ request = do
  known <- readTVarIO statuses
  case Map.lookup (request ^. #service) known of
    Just serving -> pure (response (servingStatus serving))
    Nothing -> throwIO (rpcError NotFound ("unknown service " <> request ^. #service))

watched :: Statuses -> Context -> HealthCheckRequest -> (HealthCheckResponse -> IO ()) -> IO ()
watched (Statuses statuses) _ request send = go Nothing
 where
  go previous = do
    current <- atomically do
      known <- readTVar statuses
      let status = maybe HealthCheckResponse'SERVICE_UNKNOWN servingStatus (Map.lookup (request ^. #service) known)
      if Just status == previous then retry else pure status
    send (response current)
    go (Just current)

servingStatus :: Serving -> HealthCheckResponse'ServingStatus
servingStatus = \case
  Serving -> HealthCheckResponse'SERVING
  NotServing -> HealthCheckResponse'NOT_SERVING

response :: HealthCheckResponse'ServingStatus -> HealthCheckResponse
response status = defMessage & #status .~ status
