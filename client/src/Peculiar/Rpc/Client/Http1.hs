module Peculiar.Rpc.Client.Http1
  ( Target (..)
  , web
  , http1
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, catch, throwIO, try)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Peculiar.Rpc.Client.Exchange (Body (..), Exchange (..), Exchanger (..), Outbound (..), Produced (..), failWith)
import Peculiar.Rpc.Core.Client (Protocol (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status, failing)

data Target = Target
  { manager :: Http.Manager
  , base :: Http.Request
  , protocol :: Protocol
  }

web :: Http.Manager -> Protocol -> T.Text -> Either T.Text Target
web manager protocol url
  | protocol == Grpc = Left "gRPC needs HTTP/2; call it through a connection"
  | otherwise = case Http.parseRequest (T.unpack url) of
      Left failure -> Left (T.pack (show failure))
      Right base -> Right Target{manager, base, protocol}

http1 :: Target -> Exchanger
http1 target = Exchanger \outbound use ->
  withBody outbound.body \body -> do
    let request =
          target.base
            { Http.method = outbound.method
            , Http.path = BS.dropWhileEnd (== 0x2f) target.base.path <> outbound.path
            , Http.requestHeaders = [(CI.mk name, value) | (name, value) <- outbound.headers]
            , Http.requestBody = body
            , Http.responseTimeout = Http.responseTimeoutNone
            , Http.decompress = const False
            }
    let exchanged = Http.withResponse request target.manager \response ->
          use
            Exchange
              { status = statusCode (Http.responseStatus response)
              , headers = [(CI.foldedCase name, value) | (name, value) <- Http.responseHeaders response]
              , next = Http.brRead (Http.responseBody response)
              , trailers = pure []
              }
    exchanged `catch` \(failure :: Http.HttpException) -> failWith (httpException failure) mempty

withBody :: Body -> (Http.RequestBody -> IO a) -> IO a
withBody body use = case body of
  Whole bytes -> use (Http.RequestBodyBS bytes)
  Streamed produce -> do
    queue <- newTBQueueIO 16
    let producing = do
          outcome <- try @SomeException (produce (atomically . writeTBQueue queue . Right . Just))
          atomically (writeTBQueue queue (Nothing <$ outcome))
    withAsync producing \_ -> do
      ended <- newIORef False
      let popper = do
            done <- readIORef ended
            if done
              then pure BS.empty
              else
                atomically (readTBQueue queue) >>= \case
                  Right (Just chunk) -> pure chunk
                  Right Nothing -> writeIORef ended True $> BS.empty
                  Left failure -> writeIORef ended True *> throwIO (Produced failure)
      use (Http.RequestBodyStreamChunked \needs -> needs popper)

httpException :: Http.HttpException -> Status
httpException = \case
  Http.HttpExceptionRequest _ content -> failing Unavailable (T.pack (show content))
  Http.InvalidUrlException url reason -> failing InvalidArgument (T.pack (url <> ": " <> reason))
