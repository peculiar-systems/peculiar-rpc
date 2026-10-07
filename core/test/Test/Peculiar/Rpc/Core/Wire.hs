module Test.Peculiar.Rpc.Core.Wire (tests) where

import Control.Monad (void)
import Data.ByteString qualified as BS
import Hedgehog (Group (..), Property, property, withTests, (===))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Status (failing)
import Peculiar.Rpc.Core.Wire (Body (..), Classified (..), Incoming (..), Plan (..), Rejection (..), WebEncoding (..), Wire (..), classify, plan)

tests :: Group
tests =
  Group
    "Wire"
    [ ("content types pick the protocol", classification)
    , ("a Connect unary call becomes a framed gRPC call", connectUnary)
    , ("a Connect GET reads its message from the query", connectGet)
    , ("a bad Connect timeout is refused", badTimeout)
    ]

incoming :: [(BS.ByteString, BS.ByteString)] -> Incoming
incoming headers = Incoming{method = "POST", path = "/pkg.S/M", headers}

classification :: Property
classification = withTests 1 $ property do
  let of' contentType = classify (incoming [("content-type", contentType)])
  of' "application/grpc" === Native Proto
  of' "application/grpc+proto" === Native Proto
  of' "application/grpc+json" === Native Json
  of' "application/grpc-web" === Translated (GrpcWeb Binary) Proto
  of' "application/grpc-web-text+proto" === Translated (GrpcWeb Text) Proto
  of' "application/grpc-web+json" === Translated (GrpcWeb Binary) Json
  of' "application/connect+proto" === Translated ConnectStream Proto
  of' "application/connect+json" === Translated ConnectStream Json
  of' "application/proto" === Translated ConnectUnary Proto
  of' "application/json" === Translated ConnectUnary Json
  of' "application/grpc+xml" === Rejected (UnsupportedCodec "xml")
  classify (incoming []){method = "PUT"} === Rejected MethodNotAllowed

connectUnary :: Property
connectUnary = withTests 1 $ property do
  let request =
        incoming
          [ ("content-type", "application/proto")
          , ("connect-protocol-version", "1")
          , ("connect-timeout-ms", "250")
          , ("content-encoding", "gzip")
          , ("accept-encoding", "gzip")
          , ("x-custom", "yes")
          , ("content-length", "12")
          ]
  fmap (\p -> (p.headers, p.body, p.compressed)) (plan ConnectUnary Proto request)
    === Right
      (
        [ (":method", "POST")
        , (":path", "/pkg.S/M")
        , ("content-type", "application/grpc+proto")
        , ("te", "trailers")
        , ("grpc-timeout", "250m")
        , ("grpc-encoding", "gzip")
        , ("grpc-accept-encoding", "gzip")
        , ("x-custom", "yes")
        ]
      , FromRequest
      , True
      )

connectGet :: Property
connectGet = withTests 1 $ property do
  let request = Incoming{method = "GET", path = "/pkg.S/M?encoding=proto&base64=1&message=AQI&connect=v1", headers = []}
  fmap (\p -> (p.path, p.body)) (plan ConnectGet Proto request) === Right ("/pkg.S/M", Fixed "\1\2")

badTimeout :: Property
badTimeout = withTests 1 $ property do
  void (plan ConnectUnary Proto (incoming [("connect-timeout-ms", "soon")]))
    === Left (BadRequest (failing InvalidArgument "invalid connect-timeout-ms"))
