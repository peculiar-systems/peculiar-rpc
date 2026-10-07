{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Interop.Client
  ( Case (..)
  , Server (..)
  , TestServiceClient (..)
  , UnimplementedServiceClient (..)
  , allCases
  , caseName
  , runCase
  , runCases
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (Exception, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forever, replicateM_, unless, void)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Proto.Grpc.Testing.Messages
import Proto.Grpc.Testing.Test (TestService, UnimplementedService)

Rpc.deriveClient ''TestService
Rpc.deriveClient ''UnimplementedService

data Case
  = EmptyUnary
  | LargeUnary
  | ClientStreaming
  | ServerStreaming
  | PingPong
  | EmptyStream
  | TimeoutOnSleepingServer
  | CancelAfterBegin
  | CancelAfterFirstResponse
  | StatusCodeAndMessage
  | SpecialStatusMessage
  | CustomMetadata
  | UnimplementedMethod
  | UnimplementedServiceCase
  | RpcSoak
  | ChannelSoak
  deriving stock (Eq, Show, Enum, Bounded)

data Server = Server
  { security :: Rpc.Security
  , host :: T.Text
  , port :: Int
  }

newtype Mismatch = Mismatch T.Text
  deriving stock (Show)
  deriving anyclass (Exception)

allCases :: [Case]
allCases = [minBound ..]

caseName :: Case -> T.Text
caseName = \case
  EmptyUnary -> "empty_unary"
  LargeUnary -> "large_unary"
  ClientStreaming -> "client_streaming"
  ServerStreaming -> "server_streaming"
  PingPong -> "ping_pong"
  EmptyStream -> "empty_stream"
  TimeoutOnSleepingServer -> "timeout_on_sleeping_server"
  CancelAfterBegin -> "cancel_after_begin"
  CancelAfterFirstResponse -> "cancel_after_first_response"
  StatusCodeAndMessage -> "status_code_and_message"
  SpecialStatusMessage -> "special_status_message"
  CustomMetadata -> "custom_metadata"
  UnimplementedMethod -> "unimplemented_method"
  UnimplementedServiceCase -> "unimplemented_service"
  RpcSoak -> "rpc_soak"
  ChannelSoak -> "channel_soak"

runCases :: Server -> [Case] -> IO Bool
runCases server cases = do
  outcomes <- traverse attempt cases
  pure (and outcomes)
 where
  attempt testCase =
    try @SomeException (runCase server testCase) >>= \case
      Right () -> True <$ TIO.putStrLn ("PASS " <> caseName testCase)
      Left failure -> False <$ TIO.putStrLn ("FAIL " <> caseName testCase <> ": " <> described failure)
  described failure = case fromException failure of
    Just (Mismatch reason) -> reason
    Nothing -> T.pack (displayException failure)

runCase :: Server -> Case -> IO ()
runCase server = \case
  ChannelSoak -> replicateM_ soakIterations (connected server largeUnary)
  testCase -> connected server (`caseOver` testCase)

connected :: Server -> (Rpc.Transport -> IO a) -> IO a
connected server use = Rpc.withConnection server.security server.host server.port \connection ->
  use (Rpc.Http2 connection Rpc.Grpc)

caseOver :: Rpc.Transport -> Case -> IO ()
caseOver transport = \case
  EmptyUnary -> emptyUnary transport
  LargeUnary -> largeUnary transport
  ClientStreaming -> clientStreaming transport
  ServerStreaming -> serverStreaming transport
  PingPong -> pingPong transport
  EmptyStream -> emptyStream transport
  TimeoutOnSleepingServer -> timeoutOnSleepingServer transport
  CancelAfterBegin -> cancelAfterBegin transport
  CancelAfterFirstResponse -> cancelAfterFirstResponse transport
  StatusCodeAndMessage -> statusCodeAndMessage transport
  SpecialStatusMessage -> specialStatusMessage transport
  CustomMetadata -> customMetadata transport
  UnimplementedMethod -> failsWith Rpc.Unimplemented Nothing ((testClient transport).unimplementedCall defMessage)
  UnimplementedServiceCase ->
    failsWith Rpc.Unimplemented Nothing ((Rpc.client Rpc.defaultOptions transport :: UnimplementedServiceClient).unimplementedCall defMessage)
  RpcSoak -> replicateM_ soakIterations (largeUnary transport)
  ChannelSoak -> largeUnary transport

testClient :: Rpc.Transport -> TestServiceClient
testClient = Rpc.client Rpc.defaultOptions

emptyUnary :: Rpc.Transport -> IO ()
emptyUnary transport = do
  reply <- (testClient transport).emptyCall defMessage
  expect (reply == defMessage) "EmptyCall answered with a non-empty message"

largeUnary :: Rpc.Transport -> IO ()
largeUnary transport = do
  reply <- (testClient transport).unaryCall (defMessage & #responseSize .~ 314159 & #payload .~ payload 271828)
  expect (reply ^. #payload . #type' == COMPRESSABLE && BS.length (reply ^. #payload . #body) == 314159) "the large reply is wrong"

clientStreaming :: Rpc.Transport -> IO ()
clientStreaming transport = do
  reply <- (testClient transport).streamingInputCall \send -> for_ requestSizes \size -> send (defMessage & #payload .~ payload size)
  expect (reply ^. #aggregatedPayloadSize == fromIntegral (sum requestSizes)) "the aggregated size is wrong"

serverStreaming :: Rpc.Transport -> IO ()
serverStreaming transport = do
  sizes <- newIORef []
  (testClient transport).streamingOutputCall (streamingRequest responseSizes) \reply -> modifyIORef' sizes (bodySize reply :)
  received <- reverse <$> readIORef sizes
  expect (received == responseSizes) ("the replies had sizes " <> T.pack (show received))

pingPong :: Rpc.Transport -> IO ()
pingPong transport = do
  replies <- newEmptyMVar
  (testClient transport).fullDuplexCall
    ( \send -> for_ (zip requestSizes responseSizes) \(requestSize, responseSize) -> do
        send (streamingRequest [responseSize] & #payload .~ payload requestSize)
        received <- takeMVar replies
        expect (received == responseSize) ("a reply had size " <> T.pack (show received))
    )
    (putMVar replies . bodySize)

emptyStream :: Rpc.Transport -> IO ()
emptyStream transport = do
  count <- newIORef (0 :: Int)
  (testClient transport).fullDuplexCall (const (pure ())) \_ -> modifyIORef' count (+ 1)
  received <- readIORef count
  expect (received == 0) "an empty stream received replies"

timeoutOnSleepingServer :: Rpc.Transport -> IO ()
timeoutOnSleepingServer transport =
  failsWith Rpc.DeadlineExceeded Nothing do
    let sleeping = Rpc.client Rpc.defaultOptions{Rpc.deadline = Just (Rpc.milliseconds 1)} transport :: TestServiceClient
    sleeping.fullDuplexCall (\send -> send (defMessage & #payload .~ payload 27182) *> hang) (const (pure ()))

cancelAfterBegin :: Rpc.Transport -> IO ()
cancelAfterBegin transport = do
  started <- newEmptyMVar
  cancelled ((testClient transport).streamingInputCall \_ -> putMVar started () *> hang) (takeMVar started)
  emptyUnary transport

cancelAfterFirstResponse :: Rpc.Transport -> IO ()
cancelAfterFirstResponse transport = do
  first <- newEmptyMVar
  let call =
        (testClient transport).fullDuplexCall
          (\send -> send (streamingRequest [31415] & #payload .~ payload 27182) *> hang)
          (\_ -> void (tryPutMVar first ()))
  cancelled call (takeMVar first)
  emptyUnary transport

statusCodeAndMessage :: Rpc.Transport -> IO ()
statusCodeAndMessage transport = do
  let message = "test status message"
      failing = defMessage & #code .~ 2 & #message .~ message
  failsWith Rpc.Unknown (Just message) ((testClient transport).unaryCall (defMessage & #responseStatus .~ failing))
  failsWith Rpc.Unknown (Just message) ((testClient transport).fullDuplexCall (\send -> send (defMessage & #responseStatus .~ failing)) (const (pure ())))

specialStatusMessage :: Rpc.Transport -> IO ()
specialStatusMessage transport = do
  let message = "\t\ntest with whitespace\r\nand Unicode BMP \9786 and non-BMP \128520\t\n"
  failsWith Rpc.Unknown (Just message) ((testClient transport).unaryCall (defMessage & #responseStatus .~ (defMessage & #code .~ 2 & #message .~ message)))

customMetadata :: Rpc.Transport -> IO ()
customMetadata transport = do
  checkMetadata \options -> void ((Rpc.client options transport :: TestServiceClient).unaryCall (defMessage & #responseSize .~ 1 & #payload .~ payload 1))
  checkMetadata \options ->
    (Rpc.client options transport :: TestServiceClient).fullDuplexCall
      (\send -> send (streamingRequest [1] & #payload .~ payload 1))
      (const (pure ()))

hang :: IO a
hang = forever (threadDelay 1_000_000)

cancelled :: IO a -> IO () -> IO ()
cancelled call ready =
  withAsync call \running -> do
    ready
    cancel running
    outcome <- waitCatch running
    expect (isLeft outcome) "the call finished although it was cancelled"

checkMetadata :: (Rpc.Options -> IO ()) -> IO ()
checkMetadata call = do
  headers <- newIORef mempty
  trailers <- newIORef mempty
  call
    Rpc.defaultOptions
      { Rpc.requestMetadata = Rpc.header initialKey initialValue <> Rpc.header trailingKey trailingValue
      , Rpc.onHeaders = writeIORef headers
      , Rpc.onTrailers = writeIORef trailers
      }
  initial <- Rpc.lookupHeaders initialKey <$> readIORef headers
  trailing <- Rpc.lookupHeaders trailingKey <$> readIORef trailers
  expect (initial == [initialValue]) ("the initial metadata came back as " <> T.pack (show initial))
  expect (trailing == [trailingValue]) ("the trailing metadata came back as " <> T.pack (show trailing))
 where
  initialKey = "x-grpc-test-echo-initial"
  initialValue = "test_initial_metadata_value"
  trailingKey = "x-grpc-test-echo-trailing-bin"
  trailingValue = BS.pack [0x0a, 0x0b, 0x0a, 0x0b, 0x0a, 0x0b]

failsWith :: Rpc.Code -> Maybe T.Text -> IO a -> IO ()
failsWith code message call =
  try @Rpc.RpcError call >>= \case
    Right _ -> throwIO (Mismatch ("succeeded where " <> T.pack (show code) <> " was expected"))
    Left failure -> do
      expect (failure.status.code == code) ("failed with " <> T.pack (show failure.status.code) <> " where " <> T.pack (show code) <> " was expected")
      for_ message \expected ->
        expect (failure.status.message == expected) ("the message came back as " <> T.pack (show failure.status.message))

expect :: Bool -> T.Text -> IO ()
expect holds reason = unless holds (throwIO (Mismatch reason))

streamingRequest :: [Int] -> StreamingOutputCallRequest
streamingRequest sizes =
  defMessage
    & #responseType .~ COMPRESSABLE
    & #responseParameters .~ [defMessage & #size .~ fromIntegral size | size <- sizes]

payload :: Int -> Payload
payload size = defMessage & #type' .~ COMPRESSABLE & #body .~ BS.replicate size 0

bodySize :: StreamingOutputCallResponse -> Int
bodySize reply = BS.length (reply ^. #payload . #body)

requestSizes :: [Int]
requestSizes = [27182, 8, 1828, 45904]

responseSizes :: [Int]
responseSizes = [31415, 9, 2653, 58979]

soakIterations :: Int
soakIterations = 10
