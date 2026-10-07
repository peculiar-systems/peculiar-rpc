module Peculiar.Rpc.Client.Dial
  ( Security (..)
  , Address (..)
  , withSocket
  , withChannel
  , authority
  ) where

import Control.Exception (IOException, SomeException, bracketOnError, finally, throwIO, try)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.Socket qualified as Socket
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as SocketBS
import Network.TLS qualified as TLS
import Peculiar.Rpc.Channel (Channel (..))
import Peculiar.Rpc.Tls.Client (ClientTls, clientParams)

data Security
  = Plaintext
  | Secure ClientTls

data Address
  = Tcp T.Text Int
  | Unix FilePath

withSocket :: Address -> Int -> (Socket.Socket -> IO a) -> IO a
withSocket address index use = case address of
  Unix path -> do
    socket <- bracketOnError (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close \socket ->
      socket <$ Socket.connect socket (Socket.SockAddrUnix path)
    use socket `finally` Socket.close socket
  Tcp host port -> do
    let hints = Socket.defaultHints{Socket.addrSocketType = Socket.Stream}
    resolved <- Socket.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
    attempt (foldl' (\order _ -> rotated order) resolved (replicate index ()))
 where
  attempt = \case
    [] -> ioError (userError "no address could be reached")
    candidate : rest ->
      try @IOException (open candidate) >>= \case
        Right socket -> use socket `finally` Socket.close socket
        Left failure
          | null rest -> throwIO failure
          | otherwise -> attempt rest
  open candidate =
    bracketOnError (Socket.openSocket candidate) Socket.close \socket -> do
      Socket.connect socket (Socket.addrAddress candidate)
      Socket.setSocketOption socket Socket.NoDelay 1
      pure socket

rotated :: [a] -> [a]
rotated = \case
  first : rest -> rest <> [first]
  [] -> []

authority :: Address -> T.Text
authority = \case
  Tcp host _ -> host
  Unix _ -> "localhost"

withChannel :: Security -> Address -> Socket.Socket -> (Channel -> IO a) -> IO a
withChannel security address socket use = case security of
  Plaintext -> do
    pool <- Recv.newBufferPool 2048 16384
    use Channel{send = SocketBS.sendAll socket, receive = Recv.receive socket pool, opening = BS.empty}
  Secure tls -> do
    parameters <- clientParams tls (authority address) ["h2"] >>= either (ioError . userError . T.unpack) pure
    context <- TLS.contextNew socket parameters
    TLS.handshake context
    negotiated <- TLS.getNegotiatedProtocol context
    unless (negotiated == Just "h2") (ioError (userError "the server did not agree to HTTP/2"))
    use Channel{send = TLS.sendData context . LBS.fromStrict, receive = TLS.recvData context, opening = BS.empty}
      `finally` void (try @SomeException (TLS.bye context))
