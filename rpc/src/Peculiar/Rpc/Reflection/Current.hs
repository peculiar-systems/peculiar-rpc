{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Reflection.Current
  ( current
  ) where

import Peculiar.Rpc.Context (Context)
import Peculiar.Rpc.Derive (deriveServer)
import Peculiar.Rpc.Reflection.Registry (Registry, answer)
import Peculiar.Rpc.Service (Endpoint, endpoint)
import Proto.Grpc.Reflection.V1.Reflection

deriveServer ''ServerReflection

current :: Registry -> Endpoint
current known = endpoint ServerReflectionServer{serverReflectionInfo = reflecting known}

reflecting :: Registry -> Context -> IO (Maybe ServerReflectionRequest) -> (ServerReflectionResponse -> IO ()) -> IO ()
reflecting known context receive send =
  receive >>= \case
    Nothing -> pure ()
    Just request -> send (answer known request) *> reflecting known context receive send
