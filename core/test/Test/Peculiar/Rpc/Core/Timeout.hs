module Test.Peculiar.Rpc.Core.Timeout (tests) where

import Hedgehog (Group (..), Property, assert, evalMaybe, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Timeout (Timeout (..), parseConnectTimeout, parseGrpcTimeout, renderGrpcTimeout)

tests :: Group
tests =
  Group
    "Timeout"
    [ ("a rendered grpc-timeout never shortens the deadline", neverShorter)
    , ("connect-timeout-ms is milliseconds", connect)
    , ("grpc-timeout refuses more than eight digits", eightDigits)
    ]

neverShorter :: Property
neverShorter = property do
  ns <- forAll (Gen.integral (Range.exponential 0 (3_600_000_000_000 * 1000)))
  parsed <- evalMaybe (parseGrpcTimeout (renderGrpcTimeout (Timeout ns)))
  assert (parsed.nanos >= ns)

connect :: Property
connect = withTests 1 $ property do
  parseConnectTimeout "1500" === Just (Timeout 1_500_000_000)
  parseConnectTimeout "15x" === Nothing

eightDigits :: Property
eightDigits = withTests 1 $ property do
  parseGrpcTimeout "123456789m" === Nothing
  parseGrpcTimeout "12345678m" === Just (Timeout 12_345_678_000_000)
