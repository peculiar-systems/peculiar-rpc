module Peculiar.Rpc.Native
  ( Natives (..)
  , Arrival (..)
  , native
  , answer
  ) where

import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, isEmptyTQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (SomeException, displayException, evaluate, finally, fromException, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Char8 qualified as BC
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Semantics (NextTrailersMaker (..), OutBody (..), OutBodyIface (..), OutObj (..), TrailersMaker)
import Network.HTTP.Semantics.Server (Server, auxPeerSockAddr, getRequestBodyChunk')
import Network.HTTP.Semantics.Server.Internal (Response (..))
import Network.Socket (SockAddr)
import Peculiar.Rpc.Compression (Compression (..), compress, compressionName, compressionNamed)
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..), decoder, encode, feed)
import Peculiar.Rpc.Core.Headers (lookupHeader, metadataOnly, statusHeaders)
import Peculiar.Rpc.Core.Status (Status, failing, ok)
import Peculiar.Rpc.Core.Timeout (parseGrpcTimeout)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Core.Wire (Codec (..), grpcContentType, nativeContentType)
import Peculiar.Rpc.Error (RpcError (..), rpcError)
import Peculiar.Rpc.Handler (Streaming, unchanged)
import Peculiar.Rpc.Metadata (fromHeaders, wireHeaders)
import Peculiar.Rpc.Web.Http (Respond, headerList, toHttp)

data Natives = Natives
  { find :: BS.ByteString -> Maybe Streaming
  , report :: SomeException -> IO ()
  , reveal :: Bool
  }

data Arrival = Arrival
  { codec :: Codec
  , path :: BS.ByteString
  , peer :: Maybe SockAddr
  , headers :: [Header]
  , next :: IO (Maybe BS.ByteString)
  }

data Event
  = Send Builder
  | Flush
  | End [Header]

native :: Natives -> Server
native natives request aux respond =
  case natives.find path of
    Nothing -> respond (trailersOnly [] (failing Unimplemented ("no method at " <> TE.decodeUtf8Lenient path)) []) []
    Just run -> messages (getRequestBodyChunk' request) >>= \next -> answer natives run Arrival{codec = Proto, path, peer = Just (auxPeerSockAddr aux), headers, next} respond
 where
  headers = headerList request
  path = fromMaybe "" (lookupHeader ":path" headers)

answer :: Natives -> Streaming -> Arrival -> Respond -> IO ()
answer natives run Arrival{codec, path, peer, headers, next} respond = do
  events <- newTQueueIO
  initial <- newIORef mempty
  trailed <- newIORef mempty
  final <- newIORef []
  let squeeze = chosen headers
      context =
        Context
          { metadata = fromHeaders (metadataOnly headers)
          , timeout = parseGrpcTimeout =<< lookupHeader "grpc-timeout" headers
          , respondWith = writeIORef initial
          , trailWith = \more -> modifyIORef' trailed (<> more)
          , flush = atomically (writeTQueue events Flush)
          , peer
          , requestPath = path
          , requestCodec = codec
          , requestBytes = Nothing
          }
      send bytes = framed squeeze bytes >>= atomically . writeTQueue events . Send
      handle = do
        outcome <- try @SomeException (run context (fmap unchanged <$> next) send)
        extra <- readIORef trailed
        closing <- case outcome of
          Right () -> pure (statusHeaders ok <> metadataOnly (wireHeaders extra))
          Left failure
            | Just rpc <- fromException @RpcError failure -> pure (statusHeaders rpc.status <> metadataOnly (wireHeaders (extra <> rpc.metadata)))
            | otherwise -> do
                natives.report failure
                let message = if natives.reveal then T.pack (displayException failure) else "the handler failed"
                pure (statusHeaders (failing Unknown message) <> metadataOnly (wireHeaders extra))
        atomically (writeTQueue events (End closing))
  withAsync handle \worker -> do
    first <- atomically (readTQueue events)
    opening <- readIORef initial
    let top = [(":status", "200"), ("content-type", grpcContentType codec)] <> foldMap announced squeeze <> metadataOnly (wireHeaders opening)
    case first of
      End closing -> respond (Response OutObj{outObjHeaders = fmap toHttp (top <> closing), outObjBody = OutBodyNone, outObjTrailers = trailing (pure [])}) []
      _ ->
        respond
          ( Response
              OutObj
                { outObjHeaders = fmap toHttp top
                , outObjBody = OutBodyStreamingIface \iface -> pump events final iface first `finally` cancel worker
                , outObjTrailers = trailing (readIORef final)
                }
          )
          []
    void (waitCatch worker)

pump :: TQueue Event -> IORef [Header] -> OutBodyIface -> Event -> IO ()
pump events final iface = go
 where
  go = \case
    Send frame -> do
      iface.outBodyPush frame
      idle <- atomically (isEmptyTQueue events)
      when idle (iface.outBodyPush mempty)
      atomically (readTQueue events) >>= go
    Flush -> iface.outBodyFlush *> (atomically (readTQueue events) >>= go)
    End closing -> writeIORef final closing

trailing :: IO [Header] -> TrailersMaker
trailing closing = \case
  Nothing -> Trailers . fmap toHttp <$> closing
  Just _ -> pure (NextTrailersMaker (trailing closing))

trailersOnly :: [Header] -> Status -> [Header] -> Response
trailersOnly extra status metadata =
  Response
    OutObj
      { outObjHeaders = fmap toHttp ([(":status", "200"), ("content-type", nativeContentType)] <> extra <> statusHeaders status <> metadata)
      , outObjBody = OutBodyNone
      , outObjTrailers = trailing (pure [])
      }

messages :: IO (BS.ByteString, Bool) -> IO (IO (Maybe BS.ByteString))
messages body = do
  state <- newIORef (decoder Nothing, [], False)
  let next = do
        (current, pending, ended) <- readIORef state
        case pending of
          payload : rest -> Just payload <$ writeIORef state (current, rest, ended)
          []
            | ended -> pure Nothing
            | otherwise -> do
                (chunk, done) <- body
                case feed current chunk of
                  Left _ -> throwIO (rpcError Internal "the request was not framed")
                  Right (frames, current') -> do
                    writeIORef state (current', [frame.payload | frame <- frames, frame.kind == Message], done || BS.null chunk)
                    next
  pure next

chosen :: [Header] -> Maybe Compression
chosen headers = find (`elem` accepted) preferred
 where
  accepted = mapMaybe compressionNamed (BC.split ',' (fromMaybe "" (lookupHeader "grpc-accept-encoding" headers)))
  preferred = [Zstd, Gzip, Deflate, Snappy]

announced :: Compression -> [Header]
announced compression = [("grpc-encoding", compressionName compression)]

framed :: Maybe Compression -> BS.ByteString -> IO Builder
framed squeeze bytes = do
  plain <- evaluate bytes
  case squeeze of
    Just compression
      | BS.length plain >= 1024 -> encode . Frame Message True <$> evaluate (compress compression plain)
    _ -> pure (encode (Frame Message False plain))
