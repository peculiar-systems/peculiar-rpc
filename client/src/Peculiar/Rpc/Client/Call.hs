module Peculiar.Rpc.Client.Call
  ( Call (..)
  , unary
  , serverStream
  , clientStream
  , bidiStream
  ) where

import Control.Exception (Handler (..), catches, evaluate, throwIO)
import Control.Monad (unless, (<=<))
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.ProtoLens (Message, decodeMessage)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Peculiar.Rpc.Client.Exchange (Body (..), Exchange (..), Exchanger (..), Outbound (..), Produced (..), failWith)
import Peculiar.Rpc.Compression (Compression, acceptedNames, compress, compressionName, compressionNamed, decompress)
import Peculiar.Rpc.Core.Base64 qualified as Base64
import Peculiar.Rpc.Core.Client
  ( Outgoing (..)
  , Protocol (..)
  , Shape (..)
  , contentTypeFailure
  , httpFailure
  , requestHeaders
  , responseEncoding
  , responseHeaders
  , streamStatus
  , streamTrailers
  , unaryOutcome
  , unaryTrailers
  )
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec (..), codecName)
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..), decoder, encodeStrict, feed, finish)
import Peculiar.Rpc.Core.Status (Status (..), failing)
import Peculiar.Rpc.Core.Timeout (Timeout (..), timeoutMicros)
import Peculiar.Rpc.Deadline (Deadlines, within)
import Peculiar.Rpc.Encoding (encoded)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Json (Parsing, render)
import Peculiar.Rpc.Json qualified as Json
import Peculiar.Rpc.Metadata (Metadata, fromHeaders, wireHeaders)
import System.Timeout qualified as Timeout

data Call = Call
  { exchanger :: Exchanger
  , protocol :: Protocol
  , path :: BS.ByteString
  , timeout :: Maybe Timeout
  , deadlines :: Maybe Deadlines
  , codec :: Codec
  , parsing :: Parsing
  , metadata :: Metadata
  , compression :: Maybe Compression
  , compressAbove :: Int
  , cacheable :: Bool
  , heard :: Metadata -> IO ()
  , trailed :: Metadata -> IO ()
  }

unary :: (Message i, Message o) => Call -> i -> IO o
unary call input = guarded call case call.protocol of
  Connect -> do
    plain <- encodedFor call input
    let (used, payload) = squeezed call plain
        sent = compressedWith used call
        query = getQuery sent payload
        outbound = case query of
          Just url -> (outboundOf sent Unary (Whole BS.empty)){method = "GET", path = call.path <> url, headers = filter ((`notElem` ["content-type", "connect-protocol-version", "content-encoding"]) . fst) (outboundOf sent Unary (Whole BS.empty)).headers}
          Nothing -> outboundOf sent Unary (Whole payload)
    exchangeWith call outbound \response -> do
      raw <- drained response.next
      let trailers = fromHeaders (unaryTrailers response.headers)
      call.heard (fromHeaders (responseHeaders Connect response.headers))
      call.trailed trailers
      body <- case compressionNamed =<< responseEncoding Connect Unary response.headers of
        Nothing -> pure raw
        Just compression -> decompress compression Nothing raw >>= either (\reason -> failWith (failing Internal reason) trailers) pure
      case unaryOutcome response.status body of
        Left status -> failWith status trailers
        Right bytes -> do
          for_ (contentTypeFailure Connect Unary call.codec (lookup "content-type" response.headers)) (`failWith` trailers)
          decodedFor call bytes
  _ -> single "a unary response" =<< collected call Unary . Whole =<< framed call input

serverStream :: (Message i, Message o) => Call -> i -> (o -> IO ()) -> IO ()
serverStream call input receive = guarded call do
  body <- framed call input
  streamed call Streaming (Whole body) receive

clientStream :: (Message i, Message o) => Call -> ((i -> IO ()) -> IO ()) -> IO o
clientStream call produce =
  guarded call $ single "a client-streaming response" =<< collected call Streaming (producing call produce)

bidiStream :: (Message i, Message o) => Call -> ((i -> IO ()) -> IO ()) -> (o -> IO ()) -> IO ()
bidiStream call produce = guarded call . streamed call Streaming (producing call produce)

producing :: (Message i) => Call -> ((i -> IO ()) -> IO ()) -> Body
producing call produce = Streamed \push -> produce (push <=< framed call)

framed :: (Message i) => Call -> i -> IO BS.ByteString
framed call input = do
  bytes <- encodedFor call input
  evaluate $ encodeStrict case worthIt call bytes of
    Nothing -> Frame Message False bytes
    Just compression -> Frame Message True (compress compression bytes)

encodedFor :: (Message i) => Call -> i -> IO BS.ByteString
encodedFor call input = case call.codec of
  Proto -> pure (encoded input)
  Json -> either (\problem -> failWith (failing Internal (render problem)) mempty) pure ((Json.codec call.parsing).encoded input)

compressedWith :: Maybe Compression -> Call -> Call
compressedWith used Call{..} = Call{compression = used, ..}

worthIt :: Call -> BS.ByteString -> Maybe Compression
worthIt call bytes
  | BS.length bytes >= call.compressAbove = call.compression
  | otherwise = Nothing

squeezed :: Call -> BS.ByteString -> (Maybe Compression, BS.ByteString)
squeezed call bytes = case worthIt call bytes of
  Nothing -> (Nothing, bytes)
  Just compression -> (Just compression, compress compression bytes)

single :: T.Text -> [o] -> IO o
single what = \case
  [output] -> pure output
  _ -> failWith (failing Unimplemented (what <> " carried no single message")) mempty

collected :: (Message o) => Call -> Shape -> Body -> IO [o]
collected call shape body = do
  seen <- newIORef []
  streamed call shape body \output -> modifyIORef' seen (output :)
  reverse <$> readIORef seen

streamed :: (Message o) => Call -> Shape -> Body -> (o -> IO ()) -> IO ()
streamed call shape body receive = exchange call shape body \response -> do
  let headers = response.headers
      encoding = responseEncoding call.protocol shape headers
      trailersOnly = any ((== "grpc-status") . fst) headers
  call.heard (fromHeaders (responseHeaders call.protocol headers))
  if response.status /= 200
    then drained response.next >>= \bytes -> failWith (httpFailure response.status bytes) mempty
    else do
      unless (call.protocol == Grpc && trailersOnly) do
        for_ (contentTypeFailure call.protocol Streaming call.codec (lookup "content-type" headers)) (`failWith` mempty)
      closing <- newIORef []
      bodied <- newIORef False
      let deliver frame = do
            payload <- inflated encoding frame
            case frame.kind of
              Message -> decodedFor call payload >>= receive
              _ -> modifyIORef' closing (<> [frame{compressed = False, payload}])
          go state = do
            chunk <- response.next
            if BS.null chunk
              then either (const (failWith (failing Internal "the response ended inside a message") mempty)) pure (finish state)
              else case feed state chunk of
                Left failure -> failWith (failing Internal (T.pack (show failure))) mempty
                Right (frames, state') -> writeIORef bodied True *> mapM_ deliver frames *> go state'
      go (decoder Nothing)
      ends <- readIORef closing
      hadBody <- readIORef bodied
      trailers <- response.trailers
      let statusFrom = if call.protocol == Grpc && hadBody then filter ((`notElem` statusNames) . fst) headers else headers
          final = streamStatus call.protocol statusFrom ends trailers
          metadata = fromHeaders (streamTrailers call.protocol headers ends trailers)
      call.trailed metadata
      unless (final.code == Ok) (failWith final metadata)

statusNames :: [BS.ByteString]
statusNames = ["grpc-status", "grpc-message", "grpc-status-details-bin"]

inflated :: Maybe BS.ByteString -> Frame -> IO BS.ByteString
inflated encoding frame
  | not frame.compressed = pure frame.payload
  | otherwise = case compressionNamed =<< encoding of
      Just compression -> decompress compression Nothing frame.payload >>= either (\reason -> failWith (failing Internal reason) mempty) pure
      Nothing -> failWith (failing Internal "the server compressed a message without declaring how") mempty

getQuery :: Call -> BS.ByteString -> Maybe BS.ByteString
getQuery call body
  | not call.cacheable = Nothing
  | BS.length url > 8192 = Nothing
  | otherwise = Just url
 where
  url =
    "?connect=v1&encoding="
      <> codecName call.codec
      <> "&base64=1&message="
      <> Base64.encodeUrlUnpadded body
      <> foldMap (("&compression=" <>) . compressionName) call.compression

exchange :: Call -> Shape -> Body -> (Exchange -> IO a) -> IO a
exchange call shape body = exchangeWith call (outboundOf call shape body)

exchangeWith :: Call -> Outbound -> (Exchange -> IO a) -> IO a
exchangeWith call outbound use = case call.exchanger of
  Exchanger run -> run outbound use

outboundOf :: Call -> Shape -> Body -> Outbound
outboundOf call shape body =
  Outbound
    { method = "POST"
    , path = call.path
    , headers =
        requestHeaders
          Outgoing
            { protocol = call.protocol
            , shape
            , codec = call.codec
            , timeout = call.timeout
            , compression = compressionName <$> call.compression
            , accepts = acceptedNames
            , metadata = wireHeaders call.metadata
            }
    , body
    }

drained :: IO BS.ByteString -> IO BS.ByteString
drained next = go []
 where
  go chunks =
    next >>= \chunk ->
      if BS.null chunk then pure (BS.concat (reverse chunks)) else go (chunk : chunks)

decodedFor :: (Message a) => Call -> BS.ByteString -> IO a
decodedFor call bytes = case call.codec of
  Proto -> either (\reason -> failWith (failing Internal (T.pack reason)) mempty) pure (decodeMessage bytes)
  Json -> either (\problem -> failWith (failing Internal (render problem)) mempty) pure ((Json.codec call.parsing).decoded bytes)

guarded :: Call -> IO a -> IO a
guarded call action = do
  started <- getMonotonicTimeNSec
  let expired = do
        now <- getMonotonicTimeNSec
        pure (any (\limit -> toInteger (now - started) >= limit.nanos) call.timeout)
      caught =
        action
          `catches` [ Handler \(Produced failure) -> throwIO failure
                    , Handler \(failure :: RpcError) -> do
                        late <- expired
                        throwIO (if late && failure.status.code == Unavailable then deadlineError else failure)
                    ]
  outcome <- case (call.timeout, call.deadlines) of
    (Nothing, _) -> Just <$> caught
    (Just limit, Just deadlines) -> within deadlines (started + fromIntegral limit.nanos) caught
    (Just limit, Nothing) -> Timeout.timeout (fromIntegral (timeoutMicros limit)) caught
  maybe (throwIO deadlineError) pure outcome

deadlineError :: RpcError
deadlineError = RpcError{status = failing DeadlineExceeded "the deadline passed before the call finished", metadata = mempty}
