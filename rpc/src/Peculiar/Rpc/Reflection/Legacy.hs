{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Reflection.Legacy
  ( legacy
  ) where

import Data.Either (fromRight)
import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Peculiar.Rpc.Context (Context)
import Peculiar.Rpc.Derive (deriveServer)
import Peculiar.Rpc.Reflection.Registry (Registry, answer)
import Peculiar.Rpc.Service (Endpoint, endpoint)
import Proto.Grpc.Reflection.V1alpha.Reflection

deriveServer ''ServerReflection

legacy :: Registry -> Endpoint
legacy known = endpoint ServerReflectionServer{serverReflectionInfo = reflecting known}

reflecting :: Registry -> Context -> IO (Maybe ServerReflectionRequest) -> (ServerReflectionResponse -> IO ()) -> IO ()
reflecting known context receive send =
  receive >>= \case
    Nothing -> pure ()
    Just request -> send (recoded (answer known (recoded request))) *> reflecting known context receive send

recoded :: (Message from, Message to) => from -> to
recoded = fromRight defMessage . decodeMessage . encodeMessage
