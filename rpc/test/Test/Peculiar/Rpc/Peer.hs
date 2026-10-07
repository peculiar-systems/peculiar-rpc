module Test.Peculiar.Rpc.Peer (tests) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hedgehog (Group (..), Property, assert, diff, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Frame (encodeStrict, message)
import Peculiar.Rpc.Service (Client (..))
import Proto.Peculiar.Test.V1.Echo (SayRequest)
import Test.Peculiar.Rpc.Echo (EchoClient (..))

tests :: Int -> Group
tests port =
  Group
    "Peer"
    [ ("unary native gRPC calls see their connection's peer", withTests 1 (nativeUnary port))
    , ("streaming native gRPC calls see their connection's peer", withTests 1 (nativeStream port))
    , ("gRPC-Web over HTTP/1.1 sees exactly the connecting socket", withTests 1 (grpcWebHttp1 port))
    ]

whereami :: EchoClient -> IO T.Text
whereami remote = (^. #text) <$> remote.say (defMessage & #text .~ "whereami")

onConnection :: Int -> (EchoClient -> IO a) -> IO a
onConnection port use = Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
  use (client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc))

nativeUnary :: Int -> Property
nativeUnary port = property do
  first <- liftIO (onConnection port whereami)
  second <- liftIO (onConnection port whereami)
  assert ("Just 127.0.0.1:" `T.isPrefixOf` first)
  assert ("Just 127.0.0.1:" `T.isPrefixOf` second)
  diff first (/=) second

nativeStream :: Int -> Property
nativeStream port = property do
  seen <- liftIO do
    replies <- newIORef []
    onConnection port \remote ->
      remote.chat (\send -> send (defMessage & #text .~ "whereami")) \reply -> modifyIORef' replies (reply ^. #text :)
    readIORef replies
  unary <- liftIO (onConnection port whereami)
  case seen of
    [streamed] -> do
      assert ("Just 127.0.0.1:" `T.isPrefixOf` streamed)
      diff streamed (/=) unary
    _ -> length seen === 1

grpcWebHttp1 :: Int -> Property
grpcWebHttp1 port = property do
  (mine, answer) <- liftIO do
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \socket -> do
      Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      mine <- Socket.getSocketName socket
      let body = encodeStrict (message (encodeMessage (defMessage & #text .~ "whereami" :: SayRequest)))
          request =
            BS.concat
              [ "POST /peculiar.test.v1.Echo/Say HTTP/1.1\r\n"
              , "Host: 127.0.0.1\r\n"
              , "Content-Type: application/grpc-web+proto\r\n"
              , "Connection: close\r\n"
              , "Content-Length: " <> BC.pack (show (BS.length body)) <> "\r\n\r\n"
              , body
              ]
      SocketBS.sendAll socket request
      answer <- readAll socket
      pure (mine, answer)
  let expected = TE.encodeUtf8 (T.pack (show (Just mine)))
  assert (expected `BS.isInfixOf` answer)
 where
  readAll socket = go []
   where
    go chunks =
      SocketBS.recv socket 4096 >>= \chunk ->
        if BS.null chunk then pure (BS.concat (reverse chunks)) else go (chunk : chunks)
