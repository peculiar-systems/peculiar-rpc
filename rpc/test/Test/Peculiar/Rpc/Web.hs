module Test.Peculiar.Rpc.Web (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64Url
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Group (..), Property, PropertyT, assert, evalEither, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Peculiar.Rpc.Core.Base64 (encodeText, feedText, finishText, textDecoder)
import Peculiar.Rpc.Core.Frame (Frame (..), Kind (..), decodeAll, encodeStrict, message)
import Proto.Peculiar.Test.V1.Echo (CountRequest, CountResponse, SayRequest, SayResponse)

data Answer = Answer
  { status :: Int
  , headers :: [(BS.ByteString, BS.ByteString)]
  , body :: BS.ByteString
  }

tests :: Int -> Group
tests port =
  Group
    "Web"
    [ ("Connect unary answers with the bare message", withTests 1 (connectUnary port))
    , ("Connect unary failures are JSON under the mapped status", withTests 1 (connectFailure port))
    , ("Connect GET serves an idempotent method from the query", withTests 1 (connectGet port))
    , ("Connect GET is refused for other methods", withTests 1 (connectGetRefused port))
    , ("Connect streams end with an end-stream message", withTests 1 (connectStream port))
    , ("gRPC-Web carries trailers in the last frame", withTests 1 (grpcWeb port))
    , ("gRPC-Web text is base64 both ways", withTests 1 (grpcWebText port))
    , ("an oversized Connect message is resource_exhausted", withTests 1 (connectTooLarge port))
    , ("an oversized gRPC-Web message is RESOURCE_EXHAUSTED", withTests 1 (grpcWebTooLarge port))
    , ("an unrouted Connect path is 404", withTests 1 (unknownMethod port))
    , ("an unrouted gRPC-Web path is UNIMPLEMENTED", withTests 1 (unknownWebMethod port))
    , ("an unsupported codec is 415", withTests 1 (unsupportedCodec port))
    , ("a CORS preflight is answered", withTests 1 (preflight port))
    , ("Connect unary in JSON answers in JSON", withTests 1 (connectJson port))
    , ("a JSON request with an unknown field is invalid_argument", withTests 1 (connectJsonUnknown port))
    , ("Connect GET takes a JSON message", withTests 1 (connectGetJson port))
    ]

call :: Int -> BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString -> PropertyT IO Answer
call port method path headers body = liftIO do
  manager <- Http.newManager Http.defaultManagerSettings
  let request =
        Http.defaultRequest
          { Http.host = "127.0.0.1"
          , Http.port = port
          , Http.method = method
          , Http.path = fst (BS.break (== 0x3f) path)
          , Http.queryString = snd (BS.break (== 0x3f) path)
          , Http.requestHeaders = [(CI.mk name, value) | (name, value) <- headers]
          , Http.requestBody = Http.RequestBodyBS body
          }
  response <- Http.httpLbs request manager
  pure
    Answer
      { status = statusCode (Http.responseStatus response)
      , headers = [(CI.foldedCase name, value) | (name, value) <- Http.responseHeaders response]
      , body = LBS.toStrict (Http.responseBody response)
      }

say :: T.Text -> SayRequest
say text = defMessage & #text .~ text

decoded :: (Message a) => BS.ByteString -> PropertyT IO a
decoded = evalEither . decodeMessage

connectUnary :: Int -> Property
connectUnary port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/proto"), ("connect-protocol-version", "1")] (encodeMessage (say "hi"))
  answer.status === 200
  lookup "content-type" answer.headers === Just "application/proto"
  reply :: SayResponse <- decoded answer.body
  reply ^. #text === "hi"

connectFailure :: Int -> Property
connectFailure port = property do
  let request = say "hi" & #failCode .~ 5 & #failMessage .~ "gone"
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/proto")] (encodeMessage request)
  answer.status === 404
  lookup "content-type" answer.headers === Just "application/json"
  answer.body === "{\"code\":\"not_found\",\"message\":\"gone\"}"

connectGet :: Int -> Property
connectGet port = property do
  let encoded = B64Url.encodeUnpadded (encodeMessage (say "cached"))
  answer <- call port "GET" ("/peculiar.test.v1.Echo/Say?encoding=proto&base64=1&connect=v1&message=" <> encoded) [] ""
  answer.status === 200
  reply :: SayResponse <- decoded answer.body
  reply ^. #text === "cached"

connectGetRefused :: Int -> Property
connectGetRefused port = property do
  answer <- call port "GET" "/peculiar.test.v1.Echo/Count?encoding=proto" [] ""
  answer.status === 405

connectStream :: Int -> Property
connectStream port = property do
  let request = defMessage & #upto .~ 3 :: CountRequest
  answer <- call port "POST" "/peculiar.test.v1.Echo/Count" [("content-type", "application/connect+proto")] (encodeStrict (message (encodeMessage request)))
  answer.status === 200
  lookup "content-type" answer.headers === Just "application/connect+proto"
  frames <- evalEither (decodeAll Nothing answer.body)
  numbers <- traverse (\frame -> decoded @CountResponse frame.payload) (init' frames)
  fmap (^. #n) numbers === [1, 2, 3]
  fmap (.kind) (last' frames) === Just ConnectEndStream
  fmap (.payload) (last' frames) === Just "{}"

grpcWeb :: Int -> Property
grpcWeb port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/grpc-web+proto"), ("x-grpc-web", "1")] (encodeStrict (message (encodeMessage (say "web"))))
  answer.status === 200
  lookup "content-type" answer.headers === Just "application/grpc-web+proto"
  frames <- evalEither (decodeAll Nothing answer.body)
  case frames of
    [reply, trailers] -> do
      text <- (^. #text) <$> decoded @SayResponse reply.payload
      text === "web"
      trailers.kind === GrpcWebTrailers
      BS.isInfixOf "grpc-status:0" trailers.payload === True
    other -> fmap (.kind) other === [Message, GrpcWebTrailers]

grpcWebText :: Int -> Property
grpcWebText port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/grpc-web-text")] (encodeText (encodeStrict (message (encodeMessage (say "text")))))
  answer.status === 200
  lookup "content-type" answer.headers === Just "application/grpc-web-text+proto"
  bytes <- evalEither (decodeTextChunks answer.body)
  frames <- evalEither (decodeAll Nothing bytes)
  fmap (.kind) frames === [Message, GrpcWebTrailers]

connectTooLarge :: Int -> Property
connectTooLarge port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/proto")] (encodeMessage (say (T.replicate 2000 "x")))
  answer.status === 429
  answer.body === "{\"code\":\"resource_exhausted\",\"message\":\"message is larger than this server accepts\"}"

grpcWebTooLarge :: Int -> Property
grpcWebTooLarge port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/grpc-web+proto")] (encodeStrict (message (encodeMessage (say (T.replicate 2000 "x")))))
  frames <- evalEither (decodeAll Nothing answer.body)
  let inTrailers = [frame.payload | frame <- frames, frame.kind == GrpcWebTrailers]
      statuses = foldMap pure (lookup "grpc-status" answer.headers) <> fmap (\block -> if BS.isInfixOf "grpc-status:8" block then "8" else block) inTrailers
  statuses === ["8"]

unknownMethod :: Int -> Property
unknownMethod port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Nothing" [("content-type", "application/proto")] ""
  answer.status === 404

unknownWebMethod :: Int -> Property
unknownWebMethod port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Nothing" [("content-type", "application/grpc-web+proto")] (encodeStrict (message ""))
  lookup "grpc-status" answer.headers === Just "12"

unsupportedCodec :: Int -> Property
unsupportedCodec port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/xml")] "<say/>"
  answer.status === 415

preflight :: Int -> Property
preflight port = property do
  answer <- call port "OPTIONS" "/peculiar.test.v1.Echo/Say" [("origin", "https://example.org"), ("access-control-request-method", "POST"), ("access-control-request-headers", "content-type,x-grpc-web")] ""
  answer.status === 204
  lookup "access-control-allow-origin" answer.headers === Just "*"
  lookup "access-control-allow-headers" answer.headers === Just "content-type,x-grpc-web"

decodeTextChunks :: BS.ByteString -> Either T.Text BS.ByteString
decodeTextChunks bytes = do
  (decoded', state) <- feedText textDecoder bytes
  decoded' <$ finishText state

init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

last' :: [a] -> Maybe a
last' = foldl' (\_ x -> Just x) Nothing

connectJson :: Int -> Property
connectJson port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/json")] "{\"text\": \"hello\"}"
  answer.status === 200
  lookup "content-type" answer.headers === Just "application/json"
  answer.body === "{\"text\":\"hello\"}"

connectJsonUnknown :: Int -> Property
connectJsonUnknown port = property do
  answer <- call port "POST" "/peculiar.test.v1.Echo/Say" [("content-type", "application/json")] "{\"shout\": true}"
  answer.status === 400
  assert ("invalid_argument" `BS.isInfixOf` answer.body)
  assert ("shout" `BS.isInfixOf` answer.body)

connectGetJson :: Int -> Property
connectGetJson port = property do
  answer <- call port "GET" "/peculiar.test.v1.Echo/Say?connect=v1&encoding=json&message=%7B%22text%22%3A%22got%22%7D" [] ""
  answer.status === 200
  answer.body === "{\"text\":\"got\"}"
