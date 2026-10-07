module Peculiar.Rpc.Error
  ( RpcError (..)
  , Code (..)
  , Status (..)
  , Detail (..)
  , rpcError
  , throwRpc
  , withMetadata
  ) where

import Control.Exception (Exception, throwIO)
import Data.Text qualified as T
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Detail (..), Status (..))
import Peculiar.Rpc.Metadata (Metadata)

data RpcError = RpcError
  { status :: Status
  , metadata :: Metadata
  }
  deriving stock (Show)
  deriving anyclass (Exception)

rpcError :: Code -> T.Text -> RpcError
rpcError code message = RpcError{status = Status{code, message, details = []}, metadata = mempty}

throwRpc :: Code -> T.Text -> IO a
throwRpc code message = throwIO (rpcError code message)

withMetadata :: Metadata -> RpcError -> RpcError
withMetadata more failure = RpcError{status = failure.status, metadata = failure.metadata <> more}
