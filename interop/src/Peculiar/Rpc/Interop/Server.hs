{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Interop.Server
  ( TestServiceServer (..)
  , testService
  , serveInterop
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (codeFromNumber)
import Proto.Grpc.Testing.Empty (Empty)
import Proto.Grpc.Testing.Messages
import Proto.Grpc.Testing.Test (TestService)

Rpc.deriveServer ''TestService

testService :: TestServiceServer
testService =
  TestServiceServer
    { emptyCall = \_ _ -> pure defMessage
    , unaryCall
    , cacheableUnaryCall = unaryCall
    , streamingOutputCall
    , streamingInputCall
    , fullDuplexCall
    , halfDuplexCall
    , unimplementedCall
    }

serveInterop :: Rpc.Settings -> IO ()
serveInterop settings = Rpc.serveEndpoints settings [Rpc.endpoint testService]

unaryCall :: Rpc.Context -> SimpleRequest -> IO SimpleResponse
unaryCall context request = do
  echoMetadata context
  failIfAsked (request ^. #maybe'responseStatus)
  pure (defMessage & #payload .~ payload (request ^. #responseSize))

streamingOutputCall :: Rpc.Context -> StreamingOutputCallRequest -> (StreamingOutputCallResponse -> IO ()) -> IO ()
streamingOutputCall _ = respondTo

streamingInputCall :: Rpc.Context -> IO (Maybe StreamingInputCallRequest) -> IO StreamingInputCallResponse
streamingInputCall _ receive = go 0
 where
  go total =
    receive >>= \case
      Nothing -> pure (defMessage & #aggregatedPayloadSize .~ total)
      Just request -> go (total + fromIntegral (BS.length (request ^. #payload . #body)))

fullDuplexCall :: Rpc.Context -> IO (Maybe StreamingOutputCallRequest) -> (StreamingOutputCallResponse -> IO ()) -> IO ()
fullDuplexCall context receive send = do
  echoMetadata context
  let go =
        receive >>= traverse_ \request -> do
          failIfAsked (request ^. #maybe'responseStatus)
          respondTo request send
          go
  go

halfDuplexCall :: Rpc.Context -> IO (Maybe StreamingOutputCallRequest) -> (StreamingOutputCallResponse -> IO ()) -> IO ()
halfDuplexCall _ receive send = collect [] >>= traverse_ (`respondTo` send)
 where
  collect seen = receive >>= maybe (pure (reverse seen)) (collect . (: seen))

unimplementedCall :: Rpc.Context -> Empty -> IO Empty
unimplementedCall _ _ = Rpc.throwRpc Rpc.Unimplemented "UnimplementedCall is not implemented"

respondTo :: StreamingOutputCallRequest -> (StreamingOutputCallResponse -> IO ()) -> IO ()
respondTo request send =
  for_ (request ^. #responseParameters) \parameters -> do
    when (parameters ^. #intervalUs > 0) (threadDelay (fromIntegral (parameters ^. #intervalUs)))
    send (defMessage & #payload .~ payload (parameters ^. #size))

payload :: (Integral size) => size -> Payload
payload size = defMessage & #type' .~ COMPRESSABLE & #body .~ BS.replicate (fromIntegral size) 0

echoMetadata :: Rpc.Context -> IO ()
echoMetadata context = do
  for_ (Rpc.lookupHeader initialKey context.metadata) (context.respondWith . Rpc.header initialKey)
  for_ (Rpc.lookupHeader trailingKey context.metadata) (context.trailWith . Rpc.header trailingKey)
 where
  initialKey = "x-grpc-test-echo-initial"
  trailingKey = "x-grpc-test-echo-trailing-bin"

failIfAsked :: Maybe EchoStatus -> IO ()
failIfAsked = traverse_ \echo ->
  when (echo ^. #code /= 0) do
    let code = fromMaybe Rpc.Unknown (codeFromNumber (fromIntegral (echo ^. #code)))
    Rpc.throwRpc code (echo ^. #message)
