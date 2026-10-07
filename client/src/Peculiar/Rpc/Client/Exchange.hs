{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Client.Exchange
  ( Outbound (..)
  , Body (..)
  , Exchange (..)
  , Exchanger (..)
  , Produced (..)
  , unavailable
  , failWith
  ) where

import Control.Exception (Exception, SomeException, throwIO)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status, failing)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Metadata (Metadata)

data Outbound = Outbound
  { method :: BS.ByteString
  , path :: BS.ByteString
  , headers :: [Header]
  , body :: Body
  }

data Body
  = Whole BS.ByteString
  | Streamed ((BS.ByteString -> IO ()) -> IO ())

data Exchange = Exchange
  { status :: Int
  , headers :: [Header]
  , next :: IO BS.ByteString
  , trailers :: IO [Header]
  }

newtype Exchanger = Exchanger (forall a. Outbound -> (Exchange -> IO a) -> IO a)

newtype Produced = Produced SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

unavailable :: T.Text -> IO a
unavailable reason = failWith (failing Unavailable reason) mempty

failWith :: Status -> Metadata -> IO a
failWith status metadata = throwIO RpcError{status, metadata}
