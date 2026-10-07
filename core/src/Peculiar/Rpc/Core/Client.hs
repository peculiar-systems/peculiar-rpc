module Peculiar.Rpc.Core.Client
  ( Protocol (..)
  , Shape (..)
  , Outgoing (..)
  , requestHeaders
  , responseEncoding
  , contentType
  , unaryOutcome
  , streamStatus
  , streamTrailers
  , httpFailure
  , responseHeaders
  , unaryTrailers
  , contentTypeFailure
  ) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Peculiar.Rpc.Core.Code (Code (..), codeFromHttpStatus)
import Peculiar.Rpc.Core.Codec (Codec (..), codecName)
import Peculiar.Rpc.Core.Connect (EndStream (..), decodeEndStream, decodeErrorJson, statusFromErrorJson, trailerPrefix)
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..))
import Peculiar.Rpc.Core.Headers (metadataOnly, statusFromHeaders)
import Peculiar.Rpc.Core.Status (Status (..), failing, ok)
import Peculiar.Rpc.Core.Timeout (Timeout, renderConnectTimeout, renderGrpcTimeout)
import Peculiar.Rpc.Core.Trailers (Header, decodeBlock)

data Protocol = Grpc | GrpcWeb | Connect
  deriving stock (Eq, Show, Enum, Bounded)

data Shape = Unary | Streaming
  deriving stock (Eq, Show, Enum, Bounded)

contentType :: Protocol -> Shape -> Codec -> BS.ByteString
contentType protocol shape codec = case (protocol, shape) of
  (Grpc, _) -> "application/grpc+" <> codecName codec
  (Connect, Unary) -> "application/" <> codecName codec
  (Connect, Streaming) -> "application/connect+" <> codecName codec
  (GrpcWeb, _) -> "application/grpc-web+" <> codecName codec

data Outgoing = Outgoing
  { protocol :: Protocol
  , shape :: Shape
  , codec :: Codec
  , timeout :: Maybe Timeout
  , compression :: Maybe BS.ByteString
  , accepts :: BS.ByteString
  , metadata :: [Header]
  }

requestHeaders :: Outgoing -> [Header]
requestHeaders outgoing =
  ("content-type", contentType outgoing.protocol outgoing.shape outgoing.codec) : protocolHeaders <> outgoing.metadata
 where
  encoding name = [(name, value) | Just value <- [outgoing.compression]]
  protocolHeaders = case (outgoing.protocol, outgoing.shape) of
    (Connect, shape) ->
      ("connect-protocol-version", "1")
        : [("connect-timeout-ms", renderConnectTimeout t) | Just t <- [outgoing.timeout]]
          <> case shape of
            Unary -> encoding "content-encoding" <> [("accept-encoding", outgoing.accepts)]
            Streaming -> encoding "connect-content-encoding" <> [("connect-accept-encoding", outgoing.accepts)]
    (grpc, _) ->
      [(if grpc == Grpc then "te" else "x-grpc-web", if grpc == Grpc then "trailers" else "1")]
        <> [("grpc-timeout", renderGrpcTimeout t) | Just t <- [outgoing.timeout]]
        <> encoding "grpc-encoding"
        <> [("grpc-accept-encoding", outgoing.accepts)]

responseEncoding :: Protocol -> Shape -> [Header] -> Maybe BS.ByteString
responseEncoding protocol shape = lookup case (protocol, shape) of
  (Connect, Unary) -> "content-encoding"
  (Connect, Streaming) -> "connect-content-encoding"
  _ -> "grpc-encoding"

unaryOutcome :: Int -> BS.ByteString -> Either Status BS.ByteString
unaryOutcome status body
  | status == 200 = Right body
  | otherwise = Left (httpFailure status body)

httpFailure :: Int -> BS.ByteString -> Status
httpFailure status body = case decodeErrorJson body of
  Right json -> statusFromErrorJson (codeFromHttpStatus status) json
  Left _ -> failing (codeFromHttpStatus status) ("HTTP status " <> T.pack (show status))

streamStatus :: Protocol -> [Header] -> [Frame] -> [Header] -> Status
streamStatus protocol headers frames trailers = case protocol of
  Connect -> case [frame.payload | frame <- frames, frame.kind == ConnectEndStream] of
    payload : _ -> either (const malformed) fromEnd (decodeEndStream payload)
    [] -> failing Internal "the stream ended without an end-stream message"
  GrpcWeb -> case [frame.payload | frame <- frames, frame.kind == GrpcWebTrailers] of
    block : _ -> fromMaybe missing (statusFromHeaders (decodeBlock block))
    [] -> fromMaybe missing (statusFromHeaders trailers <|> statusFromHeaders headers)
  Grpc -> fromMaybe missing (statusFromHeaders trailers <|> statusFromHeaders headers)
 where
  malformed = failing Internal "the end-stream message was not valid JSON"
  missing = failing Internal "the response carried no grpc-status"
  fromEnd end = maybe ok (statusFromErrorJson Unknown) end.endError

responseHeaders :: Protocol -> [Header] -> [Header]
responseHeaders protocol headers = case protocol of
  Connect -> filter (not . connectOwned . fst) (metadataOnly headers)
  _
    | any ((== "grpc-status") . fst) headers -> []
    | otherwise -> metadataOnly headers
 where
  connectOwned name = trailerPrefix `BS.isPrefixOf` name || "connect-" `BS.isPrefixOf` name

unaryTrailers :: [Header] -> [Header]
unaryTrailers headers = [(name, value) | (prefixed, value) <- headers, Just name <- [BS.stripPrefix trailerPrefix prefixed]]

streamTrailers :: Protocol -> [Header] -> [Frame] -> [Header] -> [Header]
streamTrailers protocol headers frames trailers = case protocol of
  Connect -> case [frame.payload | frame <- frames, frame.kind == ConnectEndStream] of
    payload : _ -> either (const []) fromEnd (decodeEndStream payload)
    [] -> []
  GrpcWeb -> case [frame.payload | frame <- frames, frame.kind == GrpcWebTrailers] of
    block : _ -> metadataOnly (decodeBlock block)
    [] -> fromHttp
  Grpc -> fromHttp
 where
  fromHttp
    | any ((== "grpc-status") . fst) trailers = metadataOnly trailers
    | any ((== "grpc-status") . fst) headers = metadataOnly headers
    | otherwise = metadataOnly trailers
  fromEnd end =
    [ (lowered name, TE.encodeUtf8 value)
    | (name, values) <- foldMap Map.toList end.metadata
    , value <- values
    ]
  lowered = BC.map toLower . TE.encodeUtf8

contentTypeFailure :: Protocol -> Shape -> Codec -> Maybe BS.ByteString -> Maybe Status
contentTypeFailure protocol shape codec received
  | given `elem` accepted = Nothing
  | sameFamily = Just (failing Internal ("unexpected codec in content type " <> TE.decodeUtf8Lenient given))
  | otherwise = Just (failing Unknown ("unexpected content type " <> TE.decodeUtf8Lenient given))
 where
  given = maybe "" (BC.map toLower . BC.strip . BC.takeWhile (/= ';')) received
  base = BC.takeWhile (/= '+')
  sameFamily = case (protocol, shape) of
    (Connect, Unary) -> "application/" `BS.isPrefixOf` given && not ("+" `BS.isInfixOf` given) && not ("application/grpc" `BS.isPrefixOf` given) && not ("application/connect" `BS.isPrefixOf` given)
    _ -> base given == base (contentType protocol shape codec)
  accepted = case (protocol, codec) of
    (Grpc, Proto) -> ["application/grpc", "application/grpc+proto"]
    (GrpcWeb, Proto) -> ["application/grpc-web", "application/grpc-web+proto"]
    _ -> [contentType protocol shape codec]
