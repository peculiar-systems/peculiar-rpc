module Peculiar.Rpc.Web.Outbound
  ( unary
  , streamed
  , native
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Network.HTTP.Semantics
  ( NextTrailersMaker (..)
  , OutBody (..)
  , OutBodyIface (..)
  , OutObj (..)
  , TrailersMaker
  , defaultTrailersMaker
  )
import Network.HTTP.Semantics.Server.Internal (Response (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Frame (decodeAll)
import Peculiar.Rpc.Core.Headers (statusHeaders)
import Peculiar.Rpc.Core.Response (Opening (..), closing, opening, statusReply, textual, unaryReply)
import Peculiar.Rpc.Core.Status (failing)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Core.Wire (WebEncoding (..), Wire (..))
import Peculiar.Rpc.Web.Http (Respond, build, fromHttp, fromReply, statusOf, strict, toHttp, withoutStatus)
import Peculiar.Rpc.Web.Inbound (Call (..), bounded, failed, settle)

unary :: Call -> Respond -> Respond
unary call respond (Response out) _ = case statusOf out of
  Just "200" -> do
    bytes <- drain out.outObjBody
    trailers <- finalTrailers call out.outObjTrailers
    let headers = withoutStatus out
    final <- if early headers then pure trailers else settle call trailers
    replaced <- readIORef call.override
    let reply = case (replaced, decodeAll Nothing bytes) of
          (Just status, _) -> statusReply status []
          (_, Right frames) -> unaryReply call.codec headers frames final
          (_, Left _) -> statusReply (failing Internal "the server's response was not framed") []
    respond (fromReply reply) []
  _ -> respond (Response out) []
 where
  early = any ((== "grpc-status") . fst)

streamed :: Wire -> Call -> Respond -> Respond
streamed wire call respond (Response out) _ = case statusOf out of
  Just "200" -> case out.outObjBody of
    OutBodyNone -> do
      replaced <- readIORef call.override
      let headers = maybe (withoutStatus out) (replaceStatus (withoutStatus out) . statusHeaders) replaced
          early' = opening call.codec wire headers
      respond (build 200 early'.headers (closing wire early' [])) []
    body ->
      respond
        ( Response
            OutObj
              { outObjHeaders = (":status", "200") : fmap toHttp open.headers
              , outObjBody = OutBodyStreamingIface (relay body)
              , outObjTrailers = defaultTrailersMaker
              }
        )
        []
  _ -> respond (Response out) []
 where
  open = opening call.codec wire (withoutStatus out)

  relay body iface = do
    let push chunk = do
          dropped <- failed call
          if dropped then pure () else iface.outBodyPush (encodeChunk chunk)
    bounded call (run body push iface)
    trailers <- finalTrailers call out.outObjTrailers >>= settle call
    iface.outBodyPushFinal (byteString (closing wire open trailers))

  encodeChunk = case wire of
    GrpcWeb Text -> byteString . textual Text . strict
    _ -> id

  run :: OutBody -> (Builder -> IO ()) -> OutBodyIface -> IO ()
  run body push iface = case body of
    OutBodyStreamingIface stream -> settled (stream iface{outBodyPush = push, outBodyPushFinal = push, outBodyCancel = const (pure ())})
    OutBodyStreaming stream -> stream push iface.outBodyFlush
    OutBodyBuilder builder -> push builder
    _ -> pure ()

native :: Call -> Respond -> Respond
native call respond (Response out) pushes = case out.outObjBody of
  OutBodyNone -> do
    replaced <- readIORef call.override
    let headers = maybe out.outObjHeaders (fmap toHttp . replaceStatus (fmap fromHttp out.outObjHeaders) . statusHeaders) replaced
    respond (Response out{outObjHeaders = headers}) pushes
  OutBodyStreamingIface stream ->
    respond
      ( Response
          out
            { outObjBody = OutBodyStreamingIface \iface -> bounded call (settled (stream iface{outBodyPush = guarded iface.outBodyPush, outBodyPushFinal = guarded iface.outBodyPushFinal, outBodyCancel = const (pure ())}))
            , outObjTrailers = settling out.outObjTrailers
            }
      )
      pushes
  _ -> respond (Response out{outObjTrailers = settling out.outObjTrailers}) pushes
 where
  settling :: TrailersMaker -> TrailersMaker
  settling maker = \case
    Just chunk ->
      maker (Just chunk) >>= \case
        NextTrailersMaker next -> pure (NextTrailersMaker (settling next))
        Trailers trailers -> Trailers . fmap toHttp <$> settle call (fmap fromHttp trailers)
    Nothing -> Trailers . fmap toHttp <$> (finalTrailers call maker >>= settle call)

  guarded push chunk = do
    dropped <- failed call
    if dropped then pure () else push chunk

settled :: IO () -> IO ()
settled action = void (try @SomeException action)

replaceStatus :: [Header] -> [Header] -> [Header]
replaceStatus headers status =
  filter ((`notElem` ["grpc-status", "grpc-message", "grpc-status-details-bin"]) . fst) headers <> status

finalTrailers :: Call -> TrailersMaker -> IO [Header]
finalTrailers call maker = do
  dropped <- failed call
  if dropped
    then pure []
    else
      maker Nothing >>= \case
        Trailers trailers -> pure (fmap fromHttp trailers)
        NextTrailersMaker next -> finalTrailers call next

drain :: OutBody -> IO BS.ByteString
drain = \case
  OutBodyNone -> pure BS.empty
  OutBodyBuilder builder -> pure (strict builder)
  OutBodyStreaming stream -> collecting \push -> stream push (pure ())
  OutBodyStreamingIface stream ->
    collecting \push ->
      stream
        OutBodyIface
          { outBodyUnmask = id
          , outBodyPush = push
          , outBodyPushFinal = push
          , outBodyCancel = const (pure ())
          , outBodyFlush = pure ()
          }
  OutBodyFile _ -> pure BS.empty
 where
  collecting :: ((Builder -> IO ()) -> IO ()) -> IO BS.ByteString
  collecting run = do
    chunks <- newIORef []
    run \chunk -> modifyIORef' chunks (chunk :)
    strict . mconcat . reverse <$> readIORef chunks
