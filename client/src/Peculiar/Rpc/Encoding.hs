module Peculiar.Rpc.Encoding
  ( encoded
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra (defaultChunkSize, toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (Message, buildMessage)

encoded :: (Message a) => a -> BS.ByteString
encoded = LBS.toStrict . toLazyByteStringWith (untrimmedStrategy 512 defaultChunkSize) LBS.empty . buildMessage
