module Test.Peculiar.Rpc.Core.Connect (tests) where

import Control.Monad (void)
import Hedgehog (Group (..), Property, forAll, property, tripping, withTests, (===))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Connect (decodeEndStream, decodeErrorJson, encodeJson, endStream, errorJson, statusFromErrorJson)
import Peculiar.Rpc.Core.Status (Detail (..), Status (..), failing)
import Test.Peculiar.Rpc.Core.Gen (status)

tests :: Group
tests =
  Group
    "Connect"
    [ ("a failed status survives Connect's error JSON", errorRoundTrip)
    , ("an end-stream message carries error and metadata", endStreamShape)
    , ("details drop the type URL prefix on the wire", detailShape)
    ]

errorRoundTrip :: Property
errorRoundTrip = property do
  value <- forAll status
  let prefixed = value{details = fmap (\d -> d{typeUrl = "type.googleapis.com/" <> d.typeUrl}) value.details}
  tripping prefixed (encodeJson . errorJson) (fmap (statusFromErrorJson Unknown) . decodeErrorJson)

endStreamShape :: Property
endStreamShape = withTests 1 $ property do
  encodeJson (endStream (failing NotFound "gone") [("x-a", "1"), ("x-a", "2")])
    === "{\"error\":{\"code\":\"not_found\",\"message\":\"gone\"},\"metadata\":{\"x-a\":[\"1\",\"2\"]}}"
  void (decodeEndStream "{}") === Right ()

detailShape :: Property
detailShape = withTests 1 $ property do
  let detailed = Status Internal "" [Detail "type.googleapis.com/google.rpc.ErrorInfo" "\1\2"]
  encodeJson (errorJson detailed) === "{\"code\":\"internal\",\"details\":[{\"type\":\"google.rpc.ErrorInfo\",\"value\":\"AQI\"}]}"
