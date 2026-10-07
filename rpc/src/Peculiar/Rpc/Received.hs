module Peculiar.Rpc.Received
  ( Received (..)
  , unchanged
  ) where

import Data.ByteString qualified as BS

data Received = Received
  { raw :: BS.ByteString
  , binary :: BS.ByteString
  }

unchanged :: BS.ByteString -> Received
unchanged bytes = Received{raw = bytes, binary = bytes}
