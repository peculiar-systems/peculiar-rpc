module Peculiar.Rpc.Core.Response
  ( Reply (..)
  , Opening (..)
  , opening
  , closing
  , textual
  , unaryReply
  , rejectionReply
  , refusal
  , statusReply
  ) where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromMaybe)
import Peculiar.Rpc.Core.Base64 (encodeText)
import Peculiar.Rpc.Core.Code (Code (..), connectHttpStatus)
import Peculiar.Rpc.Core.Codec (Codec, codecName)
import Peculiar.Rpc.Core.Connect (encodeJson, endStream, errorJson, trailerPrefix)
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..), encodeStrict)
import Peculiar.Rpc.Core.Headers (lookupHeader, metadataOnly, statusFromHeaders, statusHeaders)
import Peculiar.Rpc.Core.Status (Status (..), failing)
import Peculiar.Rpc.Core.Trailers (Header, encodeBlock)
import Peculiar.Rpc.Core.Wire (Rejection (..), WebEncoding (..), Wire (..), grpcContentType, responseContentType)

data Reply = Reply
  { httpStatus :: Int
  , headers :: [Header]
  , body :: BS.ByteString
  }
  deriving stock (Eq, Show)

data Opening = Opening
  { headers :: [Header]
  , early :: Maybe Status
  , earlyMetadata :: [Header]
  }
  deriving stock (Eq, Show)

opening :: Codec -> Wire -> [Header] -> Opening
opening codec wire headers =
  Opening
    { headers = ("content-type", responseContentType codec wire) : translated
    , early
    , earlyMetadata = metadataOnly headers
    }
 where
  early = statusFromHeaders headers
  translated = case wire of
    GrpcWeb _ -> filter ((`notElem` ["content-type", "content-length", "trailer"]) . fst) headers
    _ ->
      [("connect-content-encoding", value) | Just value <- [lookupHeader "grpc-encoding" headers]]
        <> maybe (metadataOnly headers) (const []) early

closing :: Wire -> Opening -> [Header] -> BS.ByteString
closing wire open trailers = case wire of
  GrpcWeb encoding -> case open.early of
    Just _ -> BS.empty
    Nothing -> textual encoding (encodeStrict (Frame GrpcWebTrailers False (encodeBlock trailers)))
  _ ->
    let (status, metadata) = case open.early of
          Just status' -> (status', open.earlyMetadata)
          Nothing -> (statusOr trailers, metadataOnly trailers)
     in encodeStrict (Frame ConnectEndStream False (encodeJson (endStream status metadata)))

textual :: WebEncoding -> BS.ByteString -> BS.ByteString
textual = \case
  Binary -> id
  Text -> encodeText

statusOr :: [Header] -> Status
statusOr = fromMaybe (failing Internal "response ended without a status") . statusFromHeaders

unaryReply :: Codec -> [Header] -> [Frame] -> [Header] -> Reply
unaryReply codec headers frames trailers = case (status.code, frames) of
  (Ok, [frame]) ->
    Reply
      { httpStatus = 200
      , headers =
          ("content-type", "application/" <> codecName codec)
            : [("content-encoding", encoding) | frame.compressed, Just encoding <- [lookupHeader "grpc-encoding" headers]]
              <> metadata
      , body = frame.payload
      }
  (Ok, _) -> statusReply (failing Unimplemented "unary response carried no single message") metadata
  _ -> statusReply status metadata
 where
  status = fromMaybe (statusOr trailers) (statusFromHeaders headers)
  metadata = metadataOnly headers <> fmap (first (trailerPrefix <>)) (metadataOnly trailers)

statusReply :: Status -> [Header] -> Reply
statusReply status metadata =
  Reply
    { httpStatus = connectHttpStatus status.code
    , headers = ("content-type", "application/json") : metadata
    , body = encodeJson (errorJson status)
    }

rejectionReply :: Codec -> Maybe Wire -> Rejection -> Reply
rejectionReply codec wire = \case
  UnsupportedCodec name ->
    Reply{httpStatus = 415, headers = [("accept-post", acceptPost)], body = "unsupported codec: " <> name}
  MethodNotAllowed -> Reply{httpStatus = 405, headers = [("allow", "GET, POST")], body = BS.empty}
  BadRequest status -> refusal codec wire status []

refusal :: Codec -> Maybe Wire -> Status -> [Header] -> Reply
refusal codec wire status metadata = case wire of
  Nothing ->
    Reply
      { httpStatus = 200
      , headers = ("content-type", grpcContentType codec) : statusHeaders status <> metadata
      , body = BS.empty
      }
  Just (GrpcWeb encoding) ->
    Reply
      { httpStatus = 200
      , headers = ("content-type", responseContentType codec (GrpcWeb encoding)) : statusHeaders status <> metadata
      , body = BS.empty
      }
  Just ConnectStream ->
    Reply
      { httpStatus = 200
      , headers = [("content-type", responseContentType codec ConnectStream)]
      , body = encodeStrict (Frame ConnectEndStream False (encodeJson (endStream status metadata)))
      }
  Just _ -> statusReply status metadata

acceptPost :: BS.ByteString
acceptPost =
  BC.intercalate
    ", "
    [ "application/grpc"
    , "application/grpc+proto"
    , "application/grpc-web"
    , "application/grpc-web+proto"
    , "application/grpc-web-text"
    , "application/grpc-web-text+proto"
    , "application/connect+proto"
    , "application/proto"
    , "application/grpc+json"
    , "application/grpc-web+json"
    , "application/grpc-web-text+json"
    , "application/connect+json"
    , "application/json"
    ]
