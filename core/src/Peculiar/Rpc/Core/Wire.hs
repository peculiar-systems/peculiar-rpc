module Peculiar.Rpc.Core.Wire
  ( Wire (..)
  , WebEncoding (..)
  , Incoming (..)
  , Rejection (..)
  , Classified (..)
  , Codec (..)
  , Body (..)
  , Plan (..)
  , classify
  , plan
  , nativeContentType
  , grpcContentType
  , responseContentType
  ) where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Network.HTTP.Types.URI (parseQuery)
import Peculiar.Rpc.Core.Base64 qualified as Base64
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec (..), codecName, codecNamed)
import Peculiar.Rpc.Core.Headers (lookupHeader)
import Peculiar.Rpc.Core.Status (Status, failing)
import Peculiar.Rpc.Core.Timeout (parseConnectTimeout, renderGrpcTimeout)
import Peculiar.Rpc.Core.Trailers (Header)

data WebEncoding = Binary | Text
  deriving stock (Eq, Show, Enum, Bounded)

data Wire
  = GrpcWeb WebEncoding
  | ConnectUnary
  | ConnectGet
  | ConnectStream
  deriving stock (Eq, Show)

data Incoming = Incoming
  { method :: BS.ByteString
  , path :: BS.ByteString
  , headers :: [Header]
  }
  deriving stock (Eq, Show)

data Rejection
  = UnsupportedCodec BS.ByteString
  | MethodNotAllowed
  | BadRequest Status
  deriving stock (Eq, Show)

data Classified
  = Native Codec
  | Translated Wire Codec
  | Rejected Rejection
  deriving stock (Eq, Show)

data Body
  = FromRequest
  | Fixed BS.ByteString
  deriving stock (Eq, Show)

data Plan = Plan
  { wire :: Wire
  , path :: BS.ByteString
  , headers :: [Header]
  , body :: Body
  , compressed :: Bool
  , codec :: Codec
  }
  deriving stock (Eq, Show)

nativeContentType :: BS.ByteString
nativeContentType = "application/grpc+proto"

lower :: BS.ByteString -> BS.ByteString
lower = BC.map toLower

mediaType :: [Header] -> BS.ByteString
mediaType headers = maybe "" (lower . BC.strip . BC.takeWhile (/= ';')) (lookupHeader "content-type" headers)

classify :: Incoming -> Classified
classify incoming
  | incoming.method == "GET" = Translated ConnectGet Proto
  | incoming.method /= "POST" = Rejected MethodNotAllowed
  | otherwise = case BC.break (== '+') (mediaType incoming.headers) of
      ("application/grpc", suffix) -> named Native suffix
      ("application/grpc-web", suffix) -> named (Translated (GrpcWeb Binary)) suffix
      ("application/grpc-web-text", suffix) -> named (Translated (GrpcWeb Text)) suffix
      ("application/connect", suffix) -> named (Translated ConnectStream) suffix
      ("application/proto", "") -> Translated ConnectUnary Proto
      ("application/json", "") -> Translated ConnectUnary Json
      (other, _)
        | Just codec <- BC.stripPrefix "application/" other -> Rejected (UnsupportedCodec codec)
        | otherwise -> Rejected (UnsupportedCodec other)
 where
  named classified suffix = maybe (Rejected (UnsupportedCodec (BS.drop 1 suffix))) classified (codecNamed (BS.drop 1 suffix))

plan :: Wire -> Codec -> Incoming -> Either Rejection Plan
plan wire codec incoming = case wire of
  GrpcWeb _ -> Right (base FromRequest False (passThrough incoming.headers))
  ConnectStream -> do
    timeout <- connectTimeout
    Right
      ( base FromRequest False $
          timeout
            <> renamed [("connect-content-encoding", "grpc-encoding"), ("connect-accept-encoding", "grpc-accept-encoding")]
            <> passThrough (connectless incoming.headers)
      )
  ConnectUnary -> do
    timeout <- connectTimeout
    let encoding = lookupHeader "content-encoding" incoming.headers
    Right
      ( base FromRequest (compressedBy encoding) $
          timeout
            <> encodingHeader encoding
            <> renamed [("accept-encoding", "grpc-accept-encoding")]
            <> passThrough (connectless incoming.headers)
      )
  ConnectGet -> do
    timeout <- connectTimeout
    let (route, query) = BC.break (== '?') incoming.path
        params = parseQuery query
        param name = mapMaybe (\(key, value) -> if key == name then value else Nothing) params
        single name = case param name of
          value : _ -> Just value
          [] -> Nothing
        encoding = single "compression"
    requested <- case single "encoding" of
      Just name -> maybe (Left (UnsupportedCodec name)) Right (codecNamed name)
      Nothing -> Left (BadRequest (failing InvalidArgument "missing encoding parameter"))
    raw <- case single "message" of
      Nothing -> Right ""
      Just value
        | single "base64" == Just "1" ->
            first (BadRequest . failing InvalidArgument) (Base64.decodeUrl value)
        | otherwise -> Right value
    Right
      Plan
        { wire
        , path = route
        , headers =
            pseudo route
              <> timeout
              <> encodingHeader encoding
              <> renamed [("accept-encoding", "grpc-accept-encoding")]
              <> passThrough (connectless incoming.headers)
        , body = Fixed raw
        , compressed = compressedBy encoding
        , codec = requested
        }
 where
  base body compressed headers =
    Plan{wire, path = incoming.path, headers = pseudo incoming.path <> headers, body, compressed, codec}

  pseudo route =
    [ (":method", "POST")
    , (":path", route)
    ]
      <> [(name, value) | name <- [":authority", ":scheme"], Just value <- [lookupHeader name incoming.headers]]
      <> [ ("content-type", nativeContentType)
         , ("te", "trailers")
         ]

  connectTimeout = case lookupHeader "connect-timeout-ms" incoming.headers of
    Nothing -> Right []
    Just raw -> case parseConnectTimeout raw of
      Just timeout -> Right [("grpc-timeout", renderGrpcTimeout timeout)]
      Nothing -> Left (BadRequest (failing InvalidArgument "invalid connect-timeout-ms"))

  renamed pairs =
    [ (to, value)
    | (from, to) <- pairs
    , Just value <- [lookupHeader from incoming.headers]
    ]

  encodingHeader = \case
    Just encoding | compressedEncoding encoding -> [("grpc-encoding", encoding)]
    _ -> []

  compressedBy = any compressedEncoding

  compressedEncoding encoding = encoding /= "identity" && not (BS.null encoding)

passThrough :: [Header] -> [Header]
passThrough = filter (keep . fst)
 where
  keep name =
    not (BC.isPrefixOf ":" name)
      && name
        `notElem` [ "content-type"
                  , "content-length"
                  , "content-encoding"
                  , "accept-encoding"
                  , "te"
                  , "connection"
                  , "keep-alive"
                  , "transfer-encoding"
                  , "upgrade"
                  , "host"
                  , "x-grpc-web"
                  ]

connectless :: [Header] -> [Header]
connectless = filter (not . BC.isPrefixOf "connect-" . fst)

responseContentType :: Codec -> Wire -> BS.ByteString
responseContentType codec = \case
  GrpcWeb Binary -> "application/grpc-web+" <> codecName codec
  GrpcWeb Text -> "application/grpc-web-text+" <> codecName codec
  ConnectUnary -> "application/" <> codecName codec
  ConnectGet -> "application/" <> codecName codec
  ConnectStream -> "application/connect+" <> codecName codec

grpcContentType :: Codec -> BS.ByteString
grpcContentType codec = "application/grpc+" <> codecName codec
