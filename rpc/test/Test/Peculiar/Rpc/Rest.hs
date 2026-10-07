module Test.Peculiar.Rpc.Rest (tests) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.ProtoLens (defMessage)
import Hedgehog (Group (..), Property, PropertyT, assert, property, withTests, (===))
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Peculiar.Rpc qualified as Rpc
import Test.Peculiar.Rpc.Library (LibraryClient (..))

data Answer = Answer
  { status :: Int
  , headers :: [(BS.ByteString, BS.ByteString)]
  , body :: BS.ByteString
  }

tests :: Int -> Group
tests port =
  Group
    "REST"
    [ ("a GET binds a multi-segment path variable", withTests 1 (getBook port))
    , ("query parameters fill scalar and repeated fields", withTests 1 (listBooks port))
    , ("a body bound to one field, with the rest from the path", withTests 1 (createBook port))
    , ("a custom verb with the whole body", withTests 1 (renameBook port))
    , ("an error is a google.rpc.Status under its HTTP status", withTests 1 (deleteMissing port))
    , ("a server's pushback is Retry-After over REST", withTests 1 (retryAfter port))
    , ("the wrong verb is 405 with Allow", withTests 1 (wrongVerb port))
    , ("a query value of the wrong type is 400", withTests 1 (badQuery port))
    , ("the OpenAPI document describes the routes and schemas", withTests 1 (document port))
    , ("a request that breaks a rule is a BadRequest over REST", withTests 1 (invalidOverRest port))
    , ("a query that breaks a rule is refused", withTests 1 (invalidQuery port))
    , ("a request that breaks a rule is INVALID_ARGUMENT over gRPC", withTests 1 (invalidOverGrpc port))
    , ("the OpenAPI schemas carry the rules", withTests 1 (documentRules port))
    ]

call :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> PropertyT IO Answer
call port method path body = liftIO do
  manager <- Http.newManager Http.defaultManagerSettings
  let request =
        Http.defaultRequest
          { Http.host = "127.0.0.1"
          , Http.port = port
          , Http.method = method
          , Http.path = fst (BS.break (== 0x3f) path)
          , Http.queryString = snd (BS.break (== 0x3f) path)
          , Http.requestHeaders = [(CI.mk "content-type", "application/json") | not (BS.null body)]
          , Http.requestBody = Http.RequestBodyBS body
          }
  response <- Http.httpLbs request manager
  pure
    Answer
      { status = statusCode (Http.responseStatus response)
      , headers = [(CI.foldedCase name, value) | (name, value) <- Http.responseHeaders response]
      , body = LBS.toStrict (Http.responseBody response)
      }

getBook :: Int -> Property
getBook port = property do
  answer <- call port "GET" "/v1/shelves/fiction/books/dune" ""
  answer.status === 200
  answer.body === "{\"name\":\"shelves/fiction/books/dune\",\"title\":\"Title of shelves/fiction/books/dune\"}"

listBooks :: Int -> Property
listBooks port = property do
  answer <- call port "GET" "/v1/shelves/fiction/books?pageSize=7&availableOnly=true&tags=a&tags=b" ""
  answer.status === 200
  answer.body
    === "{\"books\":[{\"available\":true,\"name\":\"fiction/a\",\"pages\":\"7\",\"title\":\"a\"},{\"available\":true,\"name\":\"fiction/b\",\"pages\":\"7\",\"title\":\"b\"}]}"

createBook :: Int -> Property
createBook port = property do
  answer <- call port "POST" "/v1/shelves/poetry/books" "{\"title\": \"Odes\", \"pages\": \"80\"}"
  answer.status === 200
  answer.body === "{\"name\":\"shelves/poetry/books/1\",\"pages\":\"80\",\"title\":\"Odes\"}"

renameBook :: Int -> Property
renameBook port = property do
  answer <- call port "PATCH" "/v1/shelves/poetry/books/1:rename" "{\"title\": \"Later Odes\"}"
  answer.status === 200
  answer.body === "{\"name\":\"shelves/poetry/books/1\",\"title\":\"Later Odes\"}"

deleteMissing :: Int -> Property
deleteMissing port = property do
  answer <- call port "DELETE" "/v1/shelves/poetry/books/missing" ""
  answer.status === 404
  answer.body === "{\"code\":5,\"message\":\"no book shelves/poetry/books/missing\"}"

retryAfter :: Int -> Property
retryAfter port = property do
  answer <- call port "DELETE" "/v1/shelves/poetry/books/busy" ""
  answer.status === 503
  lookup "retry-after" answer.headers === Just "2"

wrongVerb :: Int -> Property
wrongVerb port = property do
  answer <- call port "PUT" "/v1/shelves/fiction/books/dune" ""
  answer.status === 405
  lookup "allow" answer.headers === Just "GET, DELETE"

badQuery :: Int -> Property
badQuery port = property do
  answer <- call port "GET" "/v1/shelves/fiction/books?availableOnly=maybe" ""
  answer.status === 400
  assert ("maybe is not true or false" `BS.isInfixOf` answer.body)

document :: Int -> Property
document port = property do
  answer <- call port "GET" "/openapi.json" ""
  answer.status === 200
  let has fragment = assert (fragment `BS.isInfixOf` answer.body)
  has "\"openapi\":\"3.1.0\""
  has "\"/v1/{name}\""
  has "\"/v1/{name}:rename\""
  has "\"operationId\":\"Library_GetBook\""
  has "\"summary\":\"Fetches one book by its resource name.\""
  has "\"required\":[\"title\"]"
  has "\"readOnly\":true"
  has "\"format\":\"int64\""
  has "\"$ref\":\"#/components/schemas/google.rpc.Status\""

invalidOverRest :: Int -> Property
invalidOverRest port = property do
  answer <- call port "POST" "/v1/shelves/poetry/books" "{\"title\": \"\", \"contact\": \"nobody\", \"tags\": [\"\"]}"
  answer.status === 400
  let has fragment = assert (fragment `BS.isInfixOf` answer.body)
  has "\"@type\":\"type.googleapis.com/google.rpc.BadRequest\""
  has "\"field\":\"book.title\""
  has "must be at least 1 characters"
  has "\"field\":\"book.contact\""
  has "\"field\":\"book.tags[0]\""

invalidQuery :: Int -> Property
invalidQuery port = property do
  answer <- call port "GET" "/v1/shelves/fiction/books?pageSize=500" ""
  answer.status === 400
  assert ("must be less than or equal to 100" `BS.isInfixOf` answer.body)

invalidOverGrpc :: Int -> Property
invalidOverGrpc port = property do
  outcome <- liftIO $ Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
    try @Rpc.RpcError ((Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: LibraryClient).createBook defMessage)
  case outcome of
    Left failed -> do
      failed.status.code === Rpc.InvalidArgument
      fmap (.typeUrl) failed.status.details === ["type.googleapis.com/google.rpc.BadRequest"]
    Right _ -> assert False

documentRules :: Int -> Property
documentRules port = property do
  answer <- call port "GET" "/openapi.json" ""
  let has fragment = assert (fragment `BS.isInfixOf` answer.body)
  has "\"minLength\":1"
  has "\"maxLength\":200"
  has "\"maximum\":100"
  has "\"format\":\"email\""
  has "\"maxItems\":5"
