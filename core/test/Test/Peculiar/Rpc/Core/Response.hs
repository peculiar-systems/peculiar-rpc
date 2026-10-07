module Test.Peculiar.Rpc.Core.Response (tests) where

import Hedgehog (Group (..), Property, property, withTests, (===))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Connect (encodeJson, errorJson)
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..), decodeAll, message)
import Peculiar.Rpc.Core.Response (Opening (..), Reply (..), closing, opening, unaryReply)
import Peculiar.Rpc.Core.Status (failing)
import Peculiar.Rpc.Core.Wire (WebEncoding (..), Wire (..))

tests :: Group
tests =
  Group
    "Response"
    [ ("a successful unary reply is the bare message with trailers as headers", unaryOk)
    , ("a failed unary reply is Connect's error JSON under the mapped status", unaryFailed)
    , ("gRPC-Web ends with its trailers in a frame", webTrailers)
    , ("a trailers-only answer ends a Connect stream with its status", connectEarly)
    ]

unaryOk :: Property
unaryOk = withTests 1 $ property do
  let reply =
        unaryReply
          Proto
          [("content-type", "application/grpc+proto"), ("x-a", "1")]
          [message "payload"]
          [("grpc-status", "0"), ("x-t", "2")]
  reply
    === Reply
      { httpStatus = 200
      , headers = [("content-type", "application/proto"), ("x-a", "1"), ("trailer-x-t", "2")]
      , body = "payload"
      }

unaryFailed :: Property
unaryFailed = withTests 1 $ property do
  let reply = unaryReply Proto [("grpc-status", "5"), ("grpc-message", "gone")] [] []
  reply.httpStatus === 404
  reply.body === encodeJson (errorJson (failing NotFound "gone"))

webTrailers :: Property
webTrailers = withTests 1 $ property do
  let open = opening Proto (GrpcWeb Binary) [("content-type", "application/grpc+proto"), ("x-a", "1")]
  open.headers === [("content-type", "application/grpc-web+proto"), ("x-a", "1")]
  decodeAll Nothing (closing (GrpcWeb Binary) open [("grpc-status", "0")])
    === Right [Frame GrpcWebTrailers False "grpc-status:0\r\n"]

connectEarly :: Property
connectEarly = withTests 1 $ property do
  let open = opening Proto ConnectStream [("grpc-status", "7"), ("x-a", "1")]
  open.headers === [("content-type", "application/connect+proto")]
  decodeAll Nothing (closing ConnectStream open [])
    === Right [Frame ConnectEndStream False "{\"error\":{\"code\":\"permission_denied\"},\"metadata\":{\"x-a\":[\"1\"]}}"]
