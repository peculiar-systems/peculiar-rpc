module Test.Peculiar.Rpc.Core.Base64 (tests) where

import Data.ByteString qualified as BS
import Hedgehog (Group (..), Property, forAll, property, tripping, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Base64 (decodeEither, decodeUrl, encodeText, encodeUnpadded, feedText, finishText, textDecoder)
import Test.Peculiar.Rpc.Core.Gen (bytes, splits)

tests :: Group
tests =
  Group
    "Base64"
    [ ("unpadded binary metadata survives", unpadded)
    , ("url-safe query messages survive", url)
    , ("grpc-web-text decodes independently padded chunks split anywhere", text)
    ]

unpadded :: Property
unpadded = property do
  value <- forAll bytes
  tripping value encodeUnpadded decodeEither

url :: Property
url = property do
  value <- forAll bytes
  tripping value (BS.map safe . encodeUnpadded) decodeUrl
 where
  safe = \case
    0x2b -> 0x2d
    0x2f -> 0x5f
    other -> other

text :: Property
text = property do
  pieces <- forAll (Gen.list (Range.linear 0 5) bytes)
  let wire = foldMap encodeText pieces
  chunks <- forAll (splits wire)
  let step acc chunk = do
        (done, state) <- acc
        (more, next) <- feedText state chunk
        pure (done <> more, next)
      result = do
        (decoded, state) <- foldl' step (Right (BS.empty, textDecoder)) chunks
        decoded <$ finishText state
  result === Right (BS.concat pieces)
