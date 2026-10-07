module Test.Peculiar.Rpc.Core.Status (tests) where

import Hedgehog (Group (..), Property, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Headers (statusFromHeaders, statusHeaders)
import Peculiar.Rpc.Core.Status (decodeStatus, encodeStatus, percentDecode, percentEncode)
import Peculiar.Rpc.Core.Trailers (decodeBlock, encodeBlock)
import Test.Peculiar.Rpc.Core.Gen (headerName, headerValue, status)

tests :: Group
tests =
  Group
    "Status"
    [ ("google.rpc.Status survives its wire form", statusWire)
    , ("a message survives percent-encoding", percent)
    , ("a status survives its headers", headers)
    , ("a trailer block survives its encoding", block)
    ]

statusWire :: Property
statusWire = property do
  value <- forAll status
  tripping value encodeStatus decodeStatus

percent :: Property
percent = property do
  text <- forAll (Gen.text (Range.linear 0 60) Gen.unicode)
  tripping text percentEncode (Just . percentDecode)

headers :: Property
headers = property do
  value <- forAll status
  tripping value statusHeaders statusFromHeaders

block :: Property
block = property do
  pairs <- forAll (Gen.list (Range.linear 0 8) ((,) <$> headerName <*> headerValue))
  tripping pairs encodeBlock (Just . decodeBlock)
