{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Conformance.Client
  ( runClient
  ) where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async, cancelWith, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException, bracket_, displayException, throwIO, try)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.ProtoLens (Message, defMessage)
import Data.ProtoLens.Any qualified as Any
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Peculiar.Rpc.Conformance.Delimited (readDelimited, writeDelimited)
import Peculiar.Rpc.Conformance.Report (canceledError, errorOf, headersOf)
import Peculiar.Rpc.Conformance.Transport (Cancelled (..), Outcome (..), compressionOf, connected)
import Peculiar.Rpc.Conformance.Types (codecOf, conformanceTypes)
import Peculiar.Rpc.Core.Timeout (milliseconds)
import Peculiar.Rpc.Derive (deriveClient)
import Peculiar.Rpc.Handler (Options (..), defaultOptions)
import Peculiar.Rpc.Metadata (Metadata (..), fromHeaders)
import Peculiar.Rpc.Service (Client (..))
import Proto.Connectrpc.Conformance.V1.ClientCompat
import Proto.Connectrpc.Conformance.V1.Config (StreamType (..))
import Proto.Connectrpc.Conformance.V1.Service
import System.IO (hSetBinaryMode, stdin, stdout)

deriveClient ''ConformanceService

runClient :: IO ()
runClient = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  manager <- Http.newManager Http.defaultManagerSettings{Http.managerResponseTimeout = Http.responseTimeoutNone}
  output <- newMVar ()
  workers <- newQSem . (* 4) =<< getNumCapabilities
  let loop running =
        readDelimited @ClientCompatRequest >>= \case
          Nothing -> mapM_ waitCatch running
          Just request -> do
            running' <- async (bracket_ (waitQSem workers) (signalQSem workers) (answer manager request >>= writeDelimited output))
            loop (running' : running)
  loop []

answer :: Http.Manager -> ClientCompatRequest -> IO ClientCompatResponse
answer manager request = do
  outcome <- try @SomeException (invoke manager request)
  pure $
    defMessage
      & #testName .~ request ^. #testName
      & case outcome of
        Left failure -> #error .~ (defMessage & #message .~ T.pack (displayException failure))
        Right result -> #response .~ result

data Observed = Observed
  { headers :: IORef Metadata
  , trailers :: IORef Metadata
  , payloads :: IORef [ConformancePayload]
  , sent :: IORef Int
  , cancelled :: IORef Bool
  }

invoke :: Http.Manager -> ClientCompatRequest -> IO ClientResponseResult
invoke manager request = do
  observed <- Observed <$> newIORef mempty <*> newIORef mempty <*> newIORef [] <*> newIORef 0 <*> newIORef False
  let options =
        defaultOptions
          { deadline = fmap (milliseconds . fromIntegral) (request ^. #maybe'timeoutMs)
          , requestMetadata = fromHeaders [(TE.encodeUtf8 (header ^. #name), TE.encodeUtf8 value) | header <- request ^. #requestHeaders, value <- header ^. #value]
          , onHeaders = \received -> modifyIORef' observed.headers (<> received)
          , onTrailers = \received -> modifyIORef' observed.trailers (<> received)
          , compression = compressionOf request
          , compressAbove = 0
          , codec = codecOf request
          , registry = conformanceTypes
          }
  outcome <- connected manager request (call request observed . client options)
  gaveUp <- readIORef observed.cancelled
  let failure = case outcome of
        Completed -> Nothing
        Canceled -> Just canceledError
        Failed _ | gaveUp -> Just canceledError
        Failed rpc -> Just (errorOf rpc)
  collected <- reverse <$> readIORef observed.payloads
  heard <- readIORef observed.headers
  trailed <- readIORef observed.trailers
  sentCount <- readIORef observed.sent
  pure $
    defMessage
      & #responseHeaders .~ headersOf heard
      & #payloads .~ collected
      & #maybe'error .~ failure
      & #responseTrailers .~ headersOf trailed
      & #numUnsentRequests .~ fromIntegral (length (request ^. #requestMessages) - sentCount)

call :: ClientCompatRequest -> Observed -> ConformanceServiceClient -> IO ()
call request observed remote = case request ^. #method of
  "Unary" -> single remote.unary
  "IdempotentUnary" -> single remote.idempotentUnary
  "Unimplemented" -> single remote.unimplemented
  "ServerStream" -> do
    input <- first
    closed <- newEmptyMVar
    cancelling closed do
      markSent 1
      void (tryPutMVar closed ())
      remote.serverStream input (record . (^. #payload))
  "ClientStream" -> do
    closed <- newEmptyMVar
    cancelling closed do
      response <- remote.clientStream (produce closed)
      record (response ^. #payload)
  "BidiStream"
    | request ^. #streamType == STREAM_TYPE_FULL_DUPLEX_BIDI_STREAM -> fullDuplex
    | otherwise -> do
        closed <- newEmptyMVar
        cancelling closed (remote.bidiStream (produce closed) (record . (^. #payload)))
  other -> throwIO (userError ("unknown method " <> T.unpack other))
 where
  inputs :: (Message message) => IO [message]
  inputs = traverse (either (throwIO . userError . show) pure . Any.unpack) (request ^. #requestMessages)

  first :: (Message message) => IO message
  first =
    inputs >>= \case
      input : _ -> pure input
      [] -> pure defMessage

  single :: (Message input, HasPayload output) => (input -> IO output) -> IO ()
  single run = do
    input <- first
    closed <- newEmptyMVar
    cancelling closed do
      markSent 1
      void (tryPutMVar closed ())
      response <- run input
      record (payloadOf response)

  markSent n = modifyIORef' observed.sent (+ n)

  record payload = do
    modifyIORef' observed.payloads (payload :)
    count <- length <$> readIORef observed.payloads
    case request ^. #cancel . #maybe'cancelTiming of
      Just (ClientCompatRequest'Cancel'AfterNumResponses n) | count >= fromIntegral n -> throwIO Cancelled
      _ -> pure ()

  pause = when (request ^. #requestDelayMs > 0) (threadDelay (fromIntegral (request ^. #requestDelayMs) * 1000))

  produce :: (Message input) => MVar () -> (input -> IO ()) -> IO ()
  produce closed send = do
    messages <- inputs
    for_ messages \input -> pause *> send input *> markSent 1
    case request ^. #cancel . #maybe'cancelTiming of
      Just (ClientCompatRequest'Cancel'BeforeCloseSend _) -> throwIO Cancelled
      _ -> void (tryPutMVar closed ())

  cancelling closed action = case request ^. #cancel . #maybe'cancelTiming of
    Just (ClientCompatRequest'Cancel'AfterCloseSendMs ms) ->
      withAsync action \running ->
        withAsync (readMVar closed *> threadDelay (fromIntegral ms * 1000) *> writeIORef observed.cancelled True *> cancelWith running Cancelled) \_ ->
          wait running
    _ -> action

  fullDuplex = do
    messages <- inputs @BidiStreamRequest
    received <- newTVarIO (0 :: Int)
    finished <- newTVarIO False
    closed <- newEmptyMVar
    let receive response = do
          record (response ^. #payload)
          atomically (modifyTVar' received (+ 1))
        produceInterleaved send = do
          for_ (zip (iterate (+ 1) 1) messages) \(index, input) -> do
            pause
            send input
            markSent 1
            atomically do
              count <- readTVar received
              done <- readTVar finished
              check (count >= index || done)
          case request ^. #cancel . #maybe'cancelTiming of
            Just (ClientCompatRequest'Cancel'BeforeCloseSend _) -> throwIO Cancelled
            _ -> void (tryPutMVar closed ())
    cancelling closed (remote.bidiStream produceInterleaved receive `finallyDone` atomically (writeTVar finished True))

  finallyDone action after = do
    outcome <- try @SomeException action
    after
    either throwIO pure outcome

class HasPayload message where
  payloadOf :: message -> ConformancePayload

instance HasPayload UnaryResponse where
  payloadOf response = response ^. #payload

instance HasPayload IdempotentUnaryResponse where
  payloadOf response = response ^. #payload

instance HasPayload UnimplementedResponse where
  payloadOf _ = defMessage
