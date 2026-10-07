module Peculiar.Rpc.Listen
  ( Dispatch (..)
  , Listening (..)
  , Http2Settings (..)
  , defaultHttp2
  , http2Settings
  , withListening
  , dispatch
  , made
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (IOException, SomeException, bracket, bracketOnError, finally, try)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Text qualified as T
import Network.HTTP2.Server qualified as HTTP2
import Network.Socket qualified as Socket
import Network.Socket.BufferPool qualified as Recv
import Network.Socket.ByteString qualified as SocketBS
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Peculiar.Rpc.Channel (Channel (..), withConfig)
import Peculiar.Rpc.Http2Settings (Http2Settings (..), defaultHttp2, http2Settings)
import Peculiar.Rpc.Liveness (Policy, heard, newLiveness, supervise, tracking)
import Peculiar.Rpc.Tls (Secured (..), Tls, secure)
import System.Directory (removeFile)
import System.TimeManager qualified as TimeManager

data Listening
  = OnTcp T.Text Int
  | OnUnix FilePath

withListening :: Listening -> (Socket.Socket -> IO a) -> IO a
withListening listening use = case listening of
  OnUnix path -> do
    void (try @IOException (removeFile path))
    bracket (open (Socket.SockAddrUnix path) Socket.AF_UNIX) Socket.close use
  OnTcp host port -> do
    let hints = Socket.defaultHints{Socket.addrFlags = [Socket.AI_PASSIVE], Socket.addrSocketType = Socket.Stream}
    addresses <- Socket.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
    case addresses of
      [] -> ioError (userError ("cannot resolve " <> T.unpack host))
      address : _ -> bracket (open (Socket.addrAddress address) (Socket.addrFamily address)) Socket.close use
 where
  open address family =
    bracketOnError (Socket.socket family Socket.Stream Socket.defaultProtocol) Socket.close \socket -> do
      Socket.setSocketOption socket Socket.ReuseAddr 1
      when (family /= Socket.AF_UNIX) (Socket.setSocketOption socket Socket.NoDelay 1)
      Socket.bind socket address
      Socket.listen socket 1024
      pure socket

data Dispatch = Dispatch
  { tls :: Maybe Tls
  , report :: SomeException -> IO ()
  , http2 :: Http2Settings
  , manager :: TimeManager.Manager
  , warp :: Warp.Settings
  , internal :: Warp.InternalInfo
  , http1Accept :: Warp.Connection -> Warp.Transport -> IO ()
  , app :: Wai.Application
  , policy :: Policy
  , headerLimit :: Maybe Int
  }

dispatch :: Dispatch -> Socket.Socket -> Socket.SockAddr -> IO ()
dispatch dispatching connection peer = do
  void (try @IOException (Socket.setSocketOption connection Socket.NoDelay 1))
  case dispatching.tls of
    Nothing -> plain dispatching connection peer
    Just tls -> secured dispatching tls connection peer

made :: (Warp.Connection, Warp.Transport, Socket.SockAddr) -> (IO (Warp.Connection, Warp.Transport), Socket.SockAddr)
made (connection, transport, peer) = (pure (connection, transport), peer)

secured :: Dispatch -> Tls -> Socket.Socket -> Socket.SockAddr -> IO ()
secured dispatching@Dispatch{report, warp, http1Accept} tls connection peer =
  secure tls connection >>= \case
    Left failure -> report failure *> Socket.close connection
    Right Secured{context, protocol}
      | protocol == Just "h2" ->
          overHttp2 dispatching connection peer (transport protocol) Channel{send = TLS.sendData context . LBS.fromStrict, receive = TLS.recvData context, opening = BS.empty}
            `finally` (void (try @SomeException (TLS.bye context)) *> Socket.close connection)
      | otherwise -> do
          prepared <- Warp.socketConnection warp connection
          http1Accept
            prepared
              { Warp.connSendMany = TLS.sendData context . LBS.fromChunks
              , Warp.connSendAll = TLS.sendData context . LBS.fromStrict
              , Warp.connSendFile = \_ _ _ _ _ -> ioError (userError "files are not sent over this connection")
              , Warp.connClose = void (try @SomeException (TLS.bye context)) *> prepared.connClose
              , Warp.connRecv = TLS.recvData context
              , Warp.connRecvBuf = \_ _ -> pure False
              }
            (transport protocol)
 where
  transport protocol =
    Warp.TLS
      { Warp.tlsMajorVersion = 3
      , Warp.tlsMinorVersion = 4
      , Warp.tlsNegotiatedProtocol = protocol
      , Warp.tlsChiperID = 0
      , Warp.tlsClientCertificate = Nothing
      }

plain :: Dispatch -> Socket.Socket -> Socket.SockAddr -> IO ()
plain dispatching@Dispatch{warp, http1Accept} connection peer = do
  pool <- Recv.newBufferPool 2048 16384
  let receive = Recv.receive connection pool
  opening <- sniff receive
  if "PRI" `BS.isPrefixOf` opening
    then
      overHttp2 dispatching connection peer Warp.TCP Channel{send = SocketBS.sendAll connection, receive, opening}
        `finally` Socket.close connection
    else do
      prepared <- Warp.socketConnection warp connection
      pending <- newIORef (Just opening)
      let replay =
            atomicModifyIORef' pending (Nothing,) >>= \case
              Just bytes -> pure bytes
              Nothing -> prepared.connRecv
      http1Accept prepared{Warp.connRecv = replay} Warp.TCP

sniff :: IO BS.ByteString -> IO BS.ByteString
sniff receive = go BS.empty
 where
  go held
    | BS.length held >= 3 = pure held
    | otherwise = do
        chunk <- receive
        if BS.null chunk then pure held else go (held <> chunk)

overHttp2 :: Dispatch -> Socket.Socket -> Socket.SockAddr -> Warp.Transport -> Channel -> IO ()
overHttp2 dispatching connection peer transport channel = do
  liveness <- newLiveness
  lock <- newMVar ()
  let send bytes = withMVar lock \_ -> channel.send bytes
      receive = do
        chunk <- channel.receive
        unless (BS.null chunk) (heard liveness)
        pure chunk
      server = Warp.http2server "peculiar-rpc" dispatching.warp dispatching.internal transport peer dispatching.app
      counted request aux respond = tracking liveness (server request aux respond)
  withConfig dispatching.manager connection channel{send, receive} \config ->
    withAsync (supervise dispatching.policy liveness send *> hangUp) \_ ->
      HTTP2.run (serverConfig dispatching.headerLimit dispatching.http2) config counted
 where
  hangUp = void (try @IOException (Socket.shutdown connection Socket.ShutdownBoth))

serverConfig :: Maybe Int -> Http2Settings -> HTTP2.ServerConfig
serverConfig headerLimit settings =
  HTTP2.defaultServerConfig
    { HTTP2.connectionWindowSize = settings.connectionWindow
    , HTTP2.settings = (http2Settings settings){HTTP2.maxHeaderListSize = headerLimit}
    }
