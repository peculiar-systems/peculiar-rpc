module Peculiar.Rpc.Web.Direct
  ( Directs (..)
  , Arrived (..)
  , Outcome (..)
  , invoke
  , connectDirectly
  , grpcWebDirectly
  , grpcDirectly
  ) where

import Control.Exception (SomeException, displayException, evaluate, fromException, try)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Network.HTTP.Semantics (NextTrailersMaker (..), OutBody (..), OutObj (..), TrailersMaker)
import Network.HTTP.Semantics.Server.Internal (Response (..))
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec)
import Peculiar.Rpc.Core.Connect (trailerPrefix)
import Peculiar.Rpc.Core.Frame (Frame (..), encode, encodeStrict, message)
import Peculiar.Rpc.Core.Headers (lookupHeader, metadataOnly, statusHeaders)
import Peculiar.Rpc.Core.Response (Opening (..), Reply (..), closing, opening, statusReply, textual, unaryReply)
import Peculiar.Rpc.Core.Status (Status, failing, ok)
import Peculiar.Rpc.Core.Timeout (parseGrpcTimeout)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Core.Wire (Plan (..), WebEncoding, Wire (..), grpcContentType)
import Peculiar.Rpc.Deadline (Deadlines, within)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Handler (Direct)
import Peculiar.Rpc.Metadata (fromHeaders, wireHeaders)
import Peculiar.Rpc.Native (Arrival)
import Peculiar.Rpc.Rest (Routes)
import Peculiar.Rpc.Web.Http (Respond, toHttp)
import Peculiar.Rpc.Web.Inbound (Call (..), Messages, Pull, whole)

data Directs = Directs
  { find :: Codec -> BS.ByteString -> Maybe Direct
  , streaming :: Codec -> BS.ByteString -> Maybe (Arrival -> Respond -> IO ())
  , deadlines :: Deadlines
  , routes :: Routes
  , report :: SomeException -> IO ()
  , reveal :: Bool
  }

data Arrived = Arrived
  { call :: Call
  , headers :: [Header]
  , next :: Messages
  }

data Outcome = Outcome
  { heads :: [Header]
  , tails :: [Header]
  , result :: Either Status BS.ByteString
  }

connectDirectly :: Directs -> Direct -> Call -> Plan -> Pull -> IO Reply
connectDirectly directs run call planned source =
  whole call planned.compressed source >>= \case
    Left status -> pure (statusReply status [])
    Right frame -> do
      outcome <- invoke directs run call planned.headers frame.payload
      pure case outcome.result of
        Right bytes -> unaryReply call.codec outcome.heads [message bytes] (statusHeaders ok <> outcome.tails)
        Left status -> statusReply status (metadataOnly outcome.heads <> fmap (first (trailerPrefix <>)) (metadataOnly outcome.tails))

grpcWebDirectly :: Directs -> Direct -> WebEncoding -> Arrived -> IO Reply
grpcWebDirectly directs run encoding arrived = do
  outcome <- single directs run arrived
  let wire = GrpcWeb encoding
  pure case outcome.result of
    Right bytes ->
      let open = opening arrived.call.codec wire outcome.heads
       in Reply
            { httpStatus = 200
            , headers = open.headers
            , body = textual encoding (encodeStrict (message bytes)) <> closing wire open (statusHeaders ok <> outcome.tails)
            }
    Left status ->
      Reply{httpStatus = 200, headers = (opening arrived.call.codec wire (outcome.heads <> statusHeaders status <> outcome.tails)).headers, body = BS.empty}

grpcDirectly :: Directs -> Direct -> Arrived -> IO Response
grpcDirectly directs run arrived = do
  outcome <- single directs run arrived
  let top = [(":status", "200"), ("content-type", grpcContentType arrived.call.codec)]
  pure case outcome.result of
    Right bytes ->
      Response
        OutObj
          { outObjHeaders = fmap toHttp (top <> metadataOnly outcome.heads)
          , outObjBody = OutBodyBuilder (encode (message bytes))
          , outObjTrailers = trailing (statusHeaders ok <> metadataOnly outcome.tails)
          }
    Left status ->
      Response
        OutObj
          { outObjHeaders = fmap toHttp (top <> metadataOnly outcome.heads <> statusHeaders status <> metadataOnly outcome.tails)
          , outObjBody = OutBodyNone
          , outObjTrailers = trailing []
          }

trailing :: [Header] -> TrailersMaker
trailing headers = \case
  Nothing -> pure (Trailers (fmap toHttp headers))
  Just _ -> pure (NextTrailersMaker (trailing headers))

single :: Directs -> Direct -> Arrived -> IO Outcome
single directs run arrived = do
  received <- arrived.next
  surplus <- maybe (pure Nothing) (const arrived.next) received
  cut <- readIORef arrived.call.override
  case (cut, received, surplus) of
    (Just status, _, _) -> pure Outcome{heads = [], tails = [], result = Left status}
    (_, Just payload, Nothing) -> invoke directs run arrived.call arrived.headers payload
    (_, Nothing, _) -> pure (refused "a unary request carried no message")
    (_, Just _, Just _) -> pure (refused "a unary request carried more than one message")
 where
  refused reason = Outcome{heads = [], tails = [], result = Left (failing Unimplemented reason)}

invoke :: Directs -> Direct -> Call -> [Header] -> BS.ByteString -> IO Outcome
invoke directs run call headers payload = do
  responded <- newIORef mempty
  trailed <- newIORef mempty
  let limit = parseGrpcTimeout =<< lookupHeader "grpc-timeout" headers
      context =
        Context
          { metadata = fromHeaders (metadataOnly headers)
          , timeout = limit
          , respondWith = writeIORef responded
          , trailWith = \more -> modifyIORef' trailed (<> more)
          , flush = pure ()
          , peer = call.peer
          , requestPath = call.path
          , requestCodec = call.codec
          , requestBytes = Just payload
          }
      bounded = maybe (fmap Just) (within call.deadlines) call.deadline
  outcome <- try @SomeException (bounded (run context payload >>= evaluate))
  heads <- wireHeaders <$> readIORef responded
  tails <- wireHeaders <$> readIORef trailed
  let done result = pure Outcome{heads, tails, result}
  case outcome of
    Right (Just bytes) -> done (Right bytes)
    Right Nothing -> done (Left (failing DeadlineExceeded "the deadline passed before the call finished"))
    Left failure
      | Just rpc <- fromException @RpcError failure -> pure Outcome{heads, tails = tails <> wireHeaders rpc.metadata, result = Left rpc.status}
      | otherwise -> do
          directs.report failure
          done (Left (failing Unknown (if directs.reveal then T.pack (displayException failure) else "the handler failed")))
