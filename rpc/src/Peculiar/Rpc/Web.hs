module Peculiar.Rpc.Web
  ( WebSettings (..)
  , webWith
  , declined
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Semantics.Server (Server, auxPeerSockAddr, getRequestBodyChunk')
import Network.HTTP.Semantics.Server.Internal (Request (..), Response)
import Peculiar.Rpc.Compression (acceptedNames)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Cors (Cors, isPreflight, preflightHeaders, responseHeaders)
import Peculiar.Rpc.Core.Frame (encodeStrict)
import Peculiar.Rpc.Core.Headers (lookupHeader)
import Peculiar.Rpc.Core.Response (Reply (..), refusal, rejectionReply, statusReply)
import Peculiar.Rpc.Core.Status (Status, failing)
import Peculiar.Rpc.Core.Wire
  ( Body (..)
  , Classified (..)
  , Incoming (..)
  , Plan (..)
  , Rejection (..)
  , WebEncoding (..)
  , Wire (..)
  , classify
  , plan
  )
import Peculiar.Rpc.Native (Arrival (..))
import Peculiar.Rpc.Rest (Lookup (..), Routes (..), lookupBinding)
import Peculiar.Rpc.Web.Direct (Arrived (..), Directs (..), connectDirectly, grpcDirectly, grpcWebDirectly)
import Peculiar.Rpc.Web.Http (build, fromReply, headerList, rebuilt, withHeaders)
import Peculiar.Rpc.Web.Inbound (Arriving (..), Call (..), framedFrom, fromText, messagesOf, newCall, once, supported, whole)
import Peculiar.Rpc.Web.Outbound qualified as Outbound
import Peculiar.Rpc.Web.Rest (restCall)

data WebSettings = WebSettings
  { cors :: Maybe Cors
  , receiveLimit :: Maybe Int
  , getAllowed :: BS.ByteString -> Bool
  , routes :: Maybe (BS.ByteString -> Bool)
  , headerLimit :: Maybe Int
  }

webWith :: Directs -> WebSettings -> Server -> Server
webWith directs settings inner request aux respond0
  | Just policy <- settings.cors
  , isPreflight incoming.method incoming.headers =
      respond0 (build 204 (preflightHeaders policy incoming.headers) BS.empty) []
  | any (sum (fmap (\(name, value) -> BS.length name + BS.length value + 32) incoming.headers) >) settings.headerLimit =
      respond (declined headersTooLarge incoming) []
  | incoming.method == "GET"
  , incoming.path == "/openapi.json"
  , Just document <- directs.routes.document =
      respond (build 200 [("content-type", "application/json")] document) []
  | Routed binding bound query <- routed = restCall directs settings.receiveLimit binding bound query request aux respond
  | WrongVerb verbs <- routed = respond (build 405 [("allow", BC.intercalate ", " verbs)] BS.empty) []
  | otherwise = case classify incoming of
      Native codec
        | not (supported (lookupHeader "grpc-encoding" incoming.headers)) ->
            answer (refusal codec Nothing unsupportedCompression acceptEncoding)
        | otherwise -> do
            call <- newCall directs.deadlines Arriving{peer = Just (auxPeerSockAddr aux), path = incoming.path, codec, limit = settings.receiveLimit} incoming.headers
            next <- messagesOf call (getRequestBodyChunk' request)
            case (directs.find codec incoming.path, directs.streaming codec incoming.path) of
              (Just run, _) -> grpcDirectly directs run Arrived{call, headers = incoming.headers, next} >>= \response -> respond response []
              (_, Just run) -> run Arrival{codec, path = incoming.path, peer = call.peer, headers = uncompressed incoming.headers, next} (Outbound.native call respond)
              _ -> do
                request' <- rebuilt (uncompressed incoming.headers) (framedFrom next)
                inner request' aux (Outbound.native call respond)
      Rejected rejection -> answer (rejectionReply Proto Nothing rejection)
      Translated wire codec
        | isConnect wire && any (\known -> not (known route)) settings.routes ->
            answer (unknownRoute route)
        | otherwise -> case plan wire codec incoming of
            Left rejection -> answer (rejectionReply codec (Just wire) rejection)
            Right planned
              | wire == ConnectGet && not (settings.getAllowed planned.path) ->
                  answer (rejectionReply codec Nothing MethodNotAllowed)
              | not (supported (lookupHeader "grpc-encoding" planned.headers)) ->
                  answer (refusal codec (Just wire) unsupportedCompression acceptEncoding)
              | otherwise -> translated directs settings inner planned request aux respond
 where
  incoming = incomingOf request
  route = BC.takeWhile (/= '?') incoming.path
  routed = lookupBinding directs.routes.bindings incoming.method incoming.path
  corsHeaders = foldMap (`responseHeaders` incoming.headers) settings.cors
  respond response = respond0 (withHeaders corsHeaders response)
  answer reply = respond (fromReply reply) []

declined :: Status -> Incoming -> Response
declined status incoming = fromReply case classify incoming of
  Translated wire codec -> refusal codec (Just wire) status []
  Native codec -> refusal codec Nothing status []
  Rejected _ -> refusal Proto Nothing status []

isConnect :: Wire -> Bool
isConnect = \case
  GrpcWeb _ -> False
  _ -> True

headersTooLarge :: Status
headersTooLarge = failing ResourceExhausted "the request's headers are larger than this server accepts"

unsupportedCompression :: Status
unsupportedCompression = failing Unimplemented "this server does not support that compression"

acceptEncoding :: [(BS.ByteString, BS.ByteString)]
acceptEncoding = [("grpc-accept-encoding", acceptedNames)]

uncompressed :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
uncompressed = filter ((/= "grpc-encoding") . fst)

unknownRoute :: BS.ByteString -> Reply
unknownRoute path =
  (statusReply (failing Unimplemented ("no method at " <> TE.decodeUtf8Lenient path)) []){httpStatus = 404}

incomingOf :: Request -> Incoming
incomingOf request =
  Incoming
    { method = fromMaybe "" (lookupHeader ":method" headers)
    , path = fromMaybe "" (lookupHeader ":path" headers)
    , headers
    }
 where
  headers = headerList request

translated :: Directs -> WebSettings -> Server -> Plan -> Server
translated directs settings inner planned request aux respond = do
  call <- newCall directs.deadlines Arriving{peer = Just (auxPeerSockAddr aux), path = planned.path, codec = planned.codec, limit = settings.receiveLimit} planned.headers
  case planned.wire of
    GrpcWeb encoding -> streamed call encoding
    ConnectStream -> streamed call Binary
    _ -> do
      source <- case planned.body of
        Fixed bytes -> once bytes
        FromRequest -> pure (getRequestBodyChunk' request)
      case directs.find planned.codec planned.path of
        Just run -> connectDirectly directs run call planned source >>= \reply -> respond (fromReply reply) []
        Nothing -> viaGrpc call source
 where
  viaGrpc call source =
    whole call planned.compressed source >>= \case
      Left status -> respond (fromReply (statusReply status [])) []
      Right frame -> do
        body <- once (encodeStrict frame)
        request' <- rebuilt (uncompressed planned.headers) body
        inner request' aux (Outbound.unary call respond)

  streamed call encoding = do
    raw <- case encoding of
      Text -> fromText call (getRequestBodyChunk' request)
      Binary -> pure (getRequestBodyChunk' request)
    next <- messagesOf call raw
    case (planned.wire, directs.find planned.codec planned.path, directs.streaming planned.codec planned.path) of
      (GrpcWeb _, Just run, _) -> grpcWebDirectly directs run encoding Arrived{call, headers = planned.headers, next} >>= \reply -> respond (fromReply reply) []
      (_, _, Just run) -> run Arrival{codec = planned.codec, path = planned.path, peer = call.peer, headers = uncompressed planned.headers, next} (Outbound.streamed planned.wire call respond)
      _ -> do
        request' <- rebuilt (uncompressed planned.headers) (framedFrom next)
        inner request' aux (Outbound.streamed planned.wire call respond)
