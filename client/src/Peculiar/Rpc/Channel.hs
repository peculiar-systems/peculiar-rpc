module Peculiar.Rpc.Channel
  ( Channel (..)
  , withConfig
  ) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Foreign (free, mallocBytes)
import Network.HTTP2.Server qualified as HTTP2
import Network.Socket qualified as Socket
import Network.Socket.BufferPool qualified as Recv
import System.TimeManager qualified as TimeManager

data Channel = Channel
  { send :: BS.ByteString -> IO ()
  , receive :: IO BS.ByteString
  , opening :: BS.ByteString
  }

withConfig :: TimeManager.Manager -> Socket.Socket -> Channel -> (HTTP2.Config -> IO a) -> IO a
withConfig manager connection channel use =
  bracket (mallocBytes bufferSize) free \buffer -> do
    readN <- Recv.makeRecvN channel.opening channel.receive
    mine <- Socket.getSocketName connection
    peer <- Socket.getPeerName connection
    use
      HTTP2.defaultConfig
        { HTTP2.confWriteBuffer = buffer
        , HTTP2.confBufferSize = bufferSize
        , HTTP2.confSendAll = channel.send
        , HTTP2.confReadN = readN
        , HTTP2.confTimeoutManager = manager
        , HTTP2.confMySockAddr = mine
        , HTTP2.confPeerSockAddr = peer
        }
 where
  bufferSize = 16384 + 9
