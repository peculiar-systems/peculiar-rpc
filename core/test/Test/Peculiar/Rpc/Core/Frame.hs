module Test.Peculiar.Rpc.Core.Frame (tests) where

import Data.ByteString qualified as BS
import Hedgehog (Gen, Group (..), Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Frame (Frame (..), FrameError (..), decodeAll, decoder, encodeStrict, feed, finish)
import Test.Peculiar.Rpc.Core.Gen (bytes, splits)

tests :: Group
tests =
  Group
    "Frame"
    [ ("a run of frames survives any split", splitAnywhere)
    , ("a frame over the limit is refused from its header", overLimit)
    , ("a frame cut short is truncated", truncated)
    ]

frame :: Gen Frame
frame = Frame <$> Gen.enumBounded <*> Gen.bool <*> bytes

splitAnywhere :: Property
splitAnywhere = property do
  frames <- forAll (Gen.list (Range.linear 0 6) frame)
  let wire = foldMap encodeStrict frames
  chunks <- forAll (splits wire)
  let step acc chunk = do
        (done, state) <- acc
        (more, next) <- feed state chunk
        pure (done <> more, next)
      result = do
        (decoded, state) <- foldl' step (Right ([], decoder Nothing)) chunks
        decoded <$ finish state
  result === Right frames

overLimit :: Property
overLimit = withTests 1 $ property do
  let wire = encodeStrict (Frame minBound False (BS.replicate 20 0))
  fmap fst (feed (decoder (Just 10)) (BS.take 5 wire)) === Left (TooLarge 20)

truncated :: Property
truncated = withTests 1 $ property do
  let wire = encodeStrict (Frame minBound False "hello")
  decodeAll Nothing (BS.take 7 wire) === Left Truncated
