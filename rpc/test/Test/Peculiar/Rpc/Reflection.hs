{-# LANGUAGE TemplateHaskell #-}

module Test.Peculiar.Rpc.Reflection (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (decodeMessage, defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, assert, evalEither, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Derive (deriveClient)
import Peculiar.Rpc.Handler (defaultOptions)
import Peculiar.Rpc.Service (Client (..))
import Proto.Google.Protobuf.Descriptor (FileDescriptorProto)
import Proto.Grpc.Reflection.V1.Reflection
import System.Process (readProcess)

deriveClient ''ServerReflection

tests :: Int -> Group
tests port =
  Group
    "Reflection"
    [ ("every served service is listed", withTests 1 (listing port))
    , ("a service's file comes with its dependencies", withTests 1 (files port))
    , ("grpcurl lists the services", withTests 1 (grpcurlList port))
    , ("grpcurl calls a method through reflected descriptors", withTests 1 (grpcurlCall port))
    , ("grpcurl resolves a type from an enum-only import", withTests 1 (grpcurlEnum port))
    ]

asking :: Int -> [ServerReflectionRequest] -> IO [ServerReflectionResponse]
asking port requests =
  Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" port \transport -> do
    let remote = client defaultOptions transport :: ServerReflectionClient
    seen <- newIORef []
    remote.serverReflectionInfo (`mapM_` requests) \response -> modifyIORef' seen (response :)
    reverse <$> readIORef seen

listing :: Int -> Property
listing port = property do
  responses <- liftIO (asking port [defMessage & #listServices .~ "*"])
  let names = foldMap (fmap (^. #name) . (^. #listServicesResponse . #service)) responses
  names
    === [ "grpc.health.v1.Health"
        , "grpc.reflection.v1.ServerReflection"
        , "grpc.reflection.v1alpha.ServerReflection"
        , "peculiar.test.v1.Echo"
        ]

files :: Int -> Property
files port = property do
  responses <- liftIO (asking port [defMessage & #fileContainingSymbol .~ "peculiar.test.v1.Echo.Say"])
  descriptors <- traverse (evalEither . decodeMessage @FileDescriptorProto) (foldMap (^. #fileDescriptorResponse . #fileDescriptorProto) responses)
  fmap (^. #name) descriptors === ["peculiar/test/v1/echo.proto", "peculiar/test/v1/mood.proto"]

grpcurl :: [T.Text] -> IO T.Text
grpcurl arguments = T.pack <$> readProcess "grpcurl" (fmap T.unpack arguments) ""

address :: Int -> T.Text
address port = "127.0.0.1:" <> T.pack (show port)

grpcurlList :: Int -> Property
grpcurlList port = property do
  listed <- liftIO (grpcurl ["-plaintext", address port, "list"])
  assert ("peculiar.test.v1.Echo" `elem` T.lines listed)

grpcurlCall :: Int -> Property
grpcurlCall port = property do
  replied <- liftIO (grpcurl ["-plaintext", "-d", "{\"text\": \"reflected\"}", address port, "peculiar.test.v1.Echo/Say"])
  assert ("\"text\": \"reflected\"" `T.isInfixOf` replied)

grpcurlEnum :: Int -> Property
grpcurlEnum port = property do
  described <- liftIO (grpcurl ["-plaintext", address port, "describe", "peculiar.test.v1.Mood"])
  assert ("MOOD_CHEERFUL = 1;" `T.isInfixOf` described)
