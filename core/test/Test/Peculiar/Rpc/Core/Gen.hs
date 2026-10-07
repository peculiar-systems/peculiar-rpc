module Test.Peculiar.Rpc.Core.Gen
  ( bytes
  , code
  , status
  , splits
  , headerName
  , headerValue
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Code (Code)
import Peculiar.Rpc.Core.Status (Detail (..), Status (..))

bytes :: Gen BS.ByteString
bytes = Gen.bytes (Range.linear 0 300)

code :: Gen Code
code = Gen.enumBounded

status :: Gen Status
status =
  Status
    <$> code
    <*> Gen.text (Range.linear 0 40) Gen.unicode
    <*> Gen.list (Range.linear 0 3) detail
 where
  detail = Detail <$> Gen.text (Range.linear 0 30) Gen.alphaNum <*> bytes

splits :: BS.ByteString -> Gen [BS.ByteString]
splits whole
  | BS.null whole = pure []
  | otherwise = do
      n <- Gen.int (Range.linear 1 (BS.length whole))
      let (front, rest) = BS.splitAt n whole
      (front :) <$> splits rest

headerName :: Gen BS.ByteString
headerName = ("x-" <>) . BC.pack <$> Gen.list (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-"))

headerValue :: Gen BS.ByteString
headerValue = BC.pack <$> Gen.list (Range.linear 1 20) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_."))
