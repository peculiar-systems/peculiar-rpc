module Peculiar.Rpc.Web.Rest
  ( restCall
  , statusJson
  ) where

import Data.Aeson (Value (Null), encode)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight, isRight)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~))
import Network.HTTP.Semantics.Server (Server, auxPeerSockAddr, getRequestBodyChunk')
import Peculiar.Rpc.Core.Code (Code (..), codeNumber, connectHttpStatus)
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Frame (Frame (..))
import Peculiar.Rpc.Core.Headers (lookupHeader, metadataOnly)
import Peculiar.Rpc.Core.Status (Detail (..), Status (..), failing)
import Peculiar.Rpc.Core.Timeout (milliseconds, timeoutSeconds)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Json (Registry, toJson)
import Peculiar.Rpc.Json qualified as Json
import Peculiar.Rpc.Rest (Binding (..), Routes (..), requestValue, responseValue)
import Peculiar.Rpc.Web.Direct (Directs (..), Outcome (..), invoke)
import Peculiar.Rpc.Web.Http (build, headerList)
import Peculiar.Rpc.Web.Inbound (Arriving (..), newCall, whole)
import Proto.Google.Protobuf.Any qualified as Proto
import Proto.Google.Rpc.Status qualified as Proto

restCall :: Directs -> Maybe Int -> Binding -> [([T.Text], T.Text)] -> [(T.Text, T.Text)] -> Server
restCall directs limit binding bound query request aux respond = case directs.find Json binding.rpc of
  _ | binding.streaming -> failed (failing Unimplemented "streaming methods are not served over REST") []
  Nothing -> failed (failing Unimplemented ("no handler for " <> T.pack (show binding.rpc))) []
  Just run -> do
    let headers = headerList request
        encoded = [("grpc-encoding", encoding) | Just encoding <- [lookupHeader "content-encoding" headers], encoding /= "identity"]
    call <- newCall directs.deadlines Arriving{peer = Just (auxPeerSockAddr aux), path = binding.rpc, codec = Json, limit} (encoded <> headers)
    received <- whole call (not (null encoded)) (getRequestBodyChunk' request)
    case received of
      Left status -> failed status []
      Right frame -> case parsedBody frame.payload >>= requestValue directs.routes.catalog binding bound query of
        Left reason -> failed (failing InvalidArgument reason) []
        Right value -> do
          outcome <- invoke directs run call headers (LBS.toStrict (encode value))
          case outcome.result of
            Left status -> failed status (retryAfter outcome.tails <> metadataOnly (outcome.heads <> outcome.tails))
            Right bytes -> case Json.decodeValue bytes of
              Left _ -> respond (build 200 (json <> metadataOnly (outcome.heads <> outcome.tails)) bytes) []
              Right reply -> respond (build 200 (json <> metadataOnly (outcome.heads <> outcome.tails)) (LBS.toStrict (encode (responseValue directs.routes.catalog binding reply)))) []
 where
  json = [("content-type", "application/json")]
  parsedBody bytes
    | BS.all (`elem` [32, 9, 10, 13]) bytes = Right Nothing
    | otherwise = either (Left . Json.render) (Right . Just) (Json.decodeValue bytes)
  failed status metadata = respond (build (connectHttpStatus status.code) (json <> metadata) (statusJson directs.routes.registry status)) []

retryAfter :: [Header] -> [Header]
retryAfter trailers = case BC.readInteger =<< lookup "grpc-retry-pushback-ms" trailers of
  Just (millis, rest) | BS.null rest, millis >= 0 -> [("retry-after", BC.pack (show (timeoutSeconds (milliseconds millis))))]
  _ -> []

statusJson :: Registry -> Status -> BS.ByteString
statusJson registry status = LBS.toStrict (encode (fromRight (fromRight Null (toJson registry message)) (toJson registry withDetails)))
 where
  message = defMessage & #code .~ fromIntegral (codeNumber status.code) & #message .~ status.message :: Proto.Status
  known = [detail | detail <- fmap anyOf status.details, isRight (toJson registry detail)]
  withDetails = message & #details .~ known
  anyOf detail = defMessage & #typeUrl .~ detail.typeUrl & #value .~ detail.value :: Proto.Any
