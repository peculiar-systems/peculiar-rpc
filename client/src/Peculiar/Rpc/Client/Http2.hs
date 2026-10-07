{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Client.Http2
  ( Security (..)
  , Address (..)
  , Dialing (..)
  , defaultDialing
  , Connection
  , connectionDeadlines
  , connect
  , withConnection
  , http2
  ) where

import Control.Concurrent.Async (Async, async, cancel, poll, race, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, isEmptyTBQueue, newTBQueueIO, newTVarIO, readTBQueue, readTVar, readTVarIO, writeTBQueue, writeTVar)
import Control.Exception (Handler (..), IOException, SomeAsyncException, SomeException, catches, displayException, finally, fromException, throwIO, try)
import Control.Monad (replicateM, unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.CaseInsensitive qualified as CI
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Network.HTTP.Semantics (OutBodyIface (..), tokenFoldedKey)
import Network.HTTP.Types (statusCode)
import Network.HTTP2.Client qualified as H2
import Network.Socket qualified as Socket
import Peculiar.Rpc.Channel (Channel (..), withConfig)
import Peculiar.Rpc.Client.Dial (Address (..), Security (..), authority, withChannel, withSocket)
import Peculiar.Rpc.Client.Exchange (Body (..), Exchange (..), Exchanger (..), Outbound (..), Produced (..), failWith, unavailable)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (failing)
import Peculiar.Rpc.Deadline (Deadlines, withDeadlines)
import Peculiar.Rpc.Error (RpcError)
import Peculiar.Rpc.Http2Settings (Http2Settings (..), defaultHttp2, http2Settings)
import Peculiar.Rpc.Liveness (Keepalive, Liveness, Policy (..), heard, newLiveness, supervise, tracking)
import Peculiar.Rpc.Watch (FrameHeader (..), Keep (..), goawayType, watching)
import System.TimeManager qualified as TimeManager
import System.Timeout (timeout)

data Dialing = Dialing
  { security :: Security
  , pinging :: Maybe Keepalive
  , pool :: Int
  , http2 :: Http2Settings
  }

defaultDialing :: Dialing
defaultDialing = Dialing{security = Plaintext, pinging = Nothing, pool = 1, http2 = defaultHttp2}

data Connection = Connection
  { deadlines :: Deadlines
  , dialing :: Dialing
  , address :: Address
  , slots :: [MVar (Maybe Session)]
  , turn :: IORef Int
  , opened :: IORef [Session]
  }

newtype Sender = Sender (forall a. H2.Request -> (H2.Response -> IO a) -> IO a)

data Pending = Pending
  { ready :: MVar Sender
  , done :: MVar ()
  , draining :: TVar Bool
  , liveness :: Liveness
  }

data Session = Session
  { sender :: Sender
  , worker :: Async ()
  , done :: MVar ()
  , draining :: TVar Bool
  , liveness :: Liveness
  }

withConnection :: Security -> T.Text -> Int -> (Connection -> IO a) -> IO a
withConnection security host port = connect defaultDialing{security} (Tcp host port)

connect :: Dialing -> Address -> (Connection -> IO a) -> IO a
connect dialing address use = do
  slots <- replicateM (max 1 dialing.pool) (newMVar Nothing)
  turn <- newIORef 0
  opened <- newIORef []
  withDeadlines \deadlines ->
    use Connection{deadlines, dialing, address, slots, turn, opened}
      `finally` (readIORef opened >>= mapM_ close)

connectionDeadlines :: Connection -> Deadlines
connectionDeadlines connection = connection.deadlines

close :: Session -> IO ()
close session = do
  void (tryPutMVar session.done ())
  timeout 1_000_000 (waitCatch session.worker) >>= \case
    Just _ -> pure ()
    Nothing -> cancel session.worker

usable :: Session -> IO Bool
usable session = do
  leaving <- readTVarIO session.draining
  running <- isNothing <$> poll session.worker
  pure (running && not leaving)

acquire :: Connection -> IO Session
acquire connection = do
  index <- atomicModifyIORef' connection.turn \n -> (if n + 1 >= length connection.slots then 0 else n + 1, n)
  case drop index connection.slots of
    [] -> unavailable "the connection has no slots"
    slot : _ -> modifyMVar slot \case
      Just session ->
        usable session >>= \case
          True -> pure (Just session, session)
          False -> fresh index
      Nothing -> fresh index
 where
  fresh index = do
    session <- dial connection index
    modifyIORef' connection.opened (session :)
    pure (Just session, session)

dial :: Connection -> Int -> IO Session
dial connection index = do
  ready <- newEmptyMVar
  done <- newEmptyMVar
  draining <- newTVarIO False
  liveness <- newLiveness
  worker <- async (run connection index Pending{ready, done, draining, liveness})
  race (waitCatch worker) (takeMVar ready) >>= \case
    Right sender -> pure Session{sender, worker, done, draining, liveness}
    Left (Left failure) -> unavailable ("could not connect: " <> T.pack (displayException failure))
    Left (Right ()) -> unavailable "the connection closed before it was ready"

run :: Connection -> Int -> Pending -> IO ()
run connection index Pending{ready, done, draining, liveness} =
  withSocket connection.address index \socket ->
    withChannel connection.dialing.security connection.address socket \channel -> do
      lock <- newMVar ()
      receive <- watching 0 (noticing draining) channel.receive
      let send bytes = withMVar lock \_ -> channel.send bytes
          heardFrom = do
            chunk <- receive
            unless (BS.null chunk) (heard liveness)
            pure chunk
          policy =
            Policy
              { keepalive = connection.dialing.pinging
              , idle = Nothing
              , maxAge = Nothing
              , closing = readTVar draining
              , announce = False
              , grace = Nothing
              }
      TimeManager.withManager 0 \manager ->
        withConfig manager socket channel{send, receive = heardFrom} \config ->
          withAsync (supervise policy liveness send *> hangUp socket) \_ ->
            H2.run (clientConfig connection) config \sendRequest _ -> do
              putMVar ready (Sender sendRequest)
              takeMVar done

hangUp :: Socket.Socket -> IO ()
hangUp socket = void (try @IOException (Socket.shutdown socket Socket.ShutdownBoth))

noticing :: TVar Bool -> FrameHeader -> IO Keep
noticing draining header
  | header.kind == goawayType = Drop <$ atomically (writeTVar draining True)
  | otherwise = pure Keep

clientConfig :: Connection -> H2.ClientConfig
clientConfig connection =
  H2.defaultClientConfig
    { H2.scheme = case connection.dialing.security of
        Plaintext -> "http"
        Secure _ -> "https"
    , H2.authority = T.unpack (authority connection.address)
    , H2.connectionWindowSize = connection.dialing.http2.connectionWindow
    , H2.settings = http2Settings connection.dialing.http2
    }

http2 :: Connection -> Exchanger
http2 connection = Exchanger \outbound use -> do
  session <- acquire connection
  let Sender send = session.sender
  stop <- newIORef (const (pure ()))
  producerFailure <- newIORef Nothing
  let body iface = do
        writeIORef stop iface.outBodyCancel
        case outbound.body of
          Whole bytes -> iface.outBodyPushFinal (byteString bytes)
          Streamed produce -> do
            pending <- newTBQueueIO 64
            let producer = try @SomeException (produce (atomically . writeTBQueue pending . Right)) >>= atomically . writeTBQueue pending . Left
                pump =
                  atomically (readTBQueue pending) >>= \case
                    Right chunk -> do
                      iface.outBodyPush (byteString chunk)
                      idle <- atomically (isEmptyTBQueue pending)
                      when idle (iface.outBodyPush mempty)
                      pump
                    Left (Right ()) -> iface.outBodyPushFinal mempty
                    Left (Left failure) -> writeIORef producerFailure (Just failure) *> iface.outBodyCancel (Just failure)
            withAsync producer (const pump)
      request = case outbound.body of
        Whole bytes | BS.null bytes, outbound.method == "GET" -> H2.requestNoBody outbound.method outbound.path headers
        _ -> H2.requestStreamingIface outbound.method outbound.path headers body
      headers = [(CI.mk name, value) | (name, value) <- outbound.headers]
  outcome <-
    tracking session.liveness (try @SomeException (send request (try @SomeException . use . exchangeOf)))
      `finally` (readIORef stop >>= ($ Nothing))
  produced <- readIORef producerFailure
  case outcome of
    Left failure
      | Just _ <- fromException @SomeAsyncException failure -> throwIO failure
    _ | Just failure <- produced -> throwIO (Produced failure)
    Right (Right result) -> pure result
    Right (Left failure) -> throwIO failure
    Left failure -> transport failure

exchangeOf :: H2.Response -> Exchange
exchangeOf response =
  Exchange
    { status = maybe 0 statusCode (H2.responseStatus response)
    , headers = [(name, value) | (token, value) <- fst (H2.responseHeaders response), let name = tokenFoldedKey token, not (":" `BS.isPrefixOf` name)]
    , next = mapped (H2.getResponseBodyChunk response)
    , trailers = mapped (foldMap (\table -> [(tokenFoldedKey token, value) | (token, value) <- fst table]) <$> H2.getResponseTrailers response)
    }

mapped :: IO a -> IO a
mapped action =
  action
    `catches` [ Handler \(failure :: H2.HTTP2Error) -> fromHttp2 failure
              , Handler \(failure :: IOException) -> unavailable (T.pack (displayException failure))
              ]

transport :: SomeException -> IO a
transport failure
  | Just (_ :: RpcError) <- fromException failure = throwIO failure
  | Just (http2Failure :: H2.HTTP2Error) <- fromException failure = fromHttp2 http2Failure
  | Just (ioFailure :: IOException) <- fromException failure = unavailable (T.pack (displayException ioFailure))
  | otherwise = throwIO failure

fromHttp2 :: H2.HTTP2Error -> IO a
fromHttp2 = \case
  H2.StreamErrorIsReceived H2.Cancel _ -> failWith (failing Cancelled "the server cancelled the stream") mempty
  failure -> unavailable (T.pack (displayException failure))
