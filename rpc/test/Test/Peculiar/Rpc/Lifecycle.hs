module Test.Peculiar.Rpc.Lifecycle (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Hedgehog (Gen, Group (..), Property, evalEither, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Service (Client (..))
import Peculiar.Rpc.Watch (FrameHeader (..), Keep (..), goawayType, watching)
import System.Directory (getTemporaryDirectory)
import System.Timeout (timeout)
import Test.Peculiar.Rpc.Echo (EchoClient (..), echo)

tests :: Int -> Group
tests port =
  Group
    "Lifecycle"
    [ ("the frame watcher passes every byte but the frames it drops", watcherKeepsBytes)
    , ("a server and client talk over a Unix socket", withTests 1 unixSocket)
    , ("a pool of connections spreads calls", withTests 1 (pooled port))
    , ("an idle connection closed by the server is replaced", withTests 1 idleReplaced)
    , ("keepalive notices a peer that has gone silent", withTests 1 silentPeer)
    , ("Connect GET carries an idempotent call", withTests 1 (connectGet port))
    , ("oversized headers are refused", withTests 1 (hugeHeaders port))
    ]

say :: EchoClient -> T.Text -> IO T.Text
say remote text = (^. #text) <$> remote.say (defMessage & #text .~ text)

frames :: Gen [(Int, BS.ByteString)]
frames = Gen.list (Range.linear 0 20) ((,) <$> Gen.element [0x0, 0x1, 0x4, 0x6, 0x7, 0x8] <*> Gen.bytes (Range.linear 0 40))

encoded :: (Int, BS.ByteString) -> BS.ByteString
encoded (kind, payload) =
  BS.pack [0, 0, fromIntegral (BS.length payload), fromIntegral kind, 0, 0, 0, 0, 1] <> payload

chunked :: [Int] -> BS.ByteString -> [BS.ByteString]
chunked sizes bytes = case sizes of
  _ | BS.null bytes -> []
  size : rest -> let (chunk, more) = BS.splitAt size bytes in chunk : chunked rest more
  [] -> [bytes]

watcherKeepsBytes :: Property
watcherKeepsBytes = property do
  given <- forAll frames
  sizes <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 1 17)))
  (passed, seen) <- liftIO do
    source <- newIORef (chunked sizes (foldMap encoded given))
    dropped <- newIORef (0 :: Int)
    let receive = do
          remaining <- readIORef source
          case remaining of
            chunk : rest -> chunk <$ modifyIORef' source (const rest)
            [] -> pure BS.empty
        decide header
          | header.kind == goawayType = Drop <$ modifyIORef' dropped (+ 1)
          | otherwise = pure Keep
    pull <- watching 0 decide receive
    let drain acc =
          pull >>= \chunk -> if BS.null chunk then pure (BS.concat (reverse acc)) else drain (chunk : acc)
    (,) <$> drain [] <*> readIORef dropped
  passed === foldMap encoded (filter ((/= fromIntegral goawayType) . fst) given)
  seen === length (filter ((== fromIntegral goawayType) . fst) given)

serving :: (Rpc.Settings -> Rpc.Settings) -> (Int -> IO a) -> IO a
serving adjust use = do
  bound <- newEmptyMVar
  stop <- newEmptyMVar
  let settings = adjust Rpc.defaultSettings{Rpc.port = 0, Rpc.onListening = putMVar bound, Rpc.calls = Rpc.defaultCalls{Rpc.report = const (pure ())}, Rpc.stopWhen = readMVar stop}
  withAsync (Rpc.serveEndpoints settings [Rpc.endpoint echo]) \_ ->
    (takeMVar bound >>= use) <* putMVar stop ()

unixSocket :: Property
unixSocket = property do
  reply <- liftIO do
    directory <- getTemporaryDirectory
    let path = directory <> "/peculiar-rpc-test.sock"
    serving (\settings -> settings{Rpc.unixSocket = Just path}) \_ ->
      Rpc.connect Rpc.defaultDialing (Rpc.Unix path) \connection ->
        say (client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc)) "over a socket file"
  reply === "over a socket file"

pooled :: Int -> Property
pooled port = property do
  replies <- liftIO $ Rpc.connect Rpc.defaultDialing{Rpc.pool = 3} (Rpc.Tcp "127.0.0.1" port) \connection ->
    traverse (say (client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc)) . T.pack . show) [1 .. 6 :: Int]
  replies === fmap (T.pack . show) [1 .. 6 :: Int]

idleReplaced :: Property
idleReplaced = property do
  replies <- liftIO $ serving (\settings -> settings{Rpc.idleTimeout = Just (Rpc.milliseconds 200)}) \port ->
    Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> do
      let remote = client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc)
      first <- say remote "before"
      threadDelay 700_000
      second <- say remote "after"
      pure [first, second]
  replies === ["before", "after"]

silentPeer :: Property
silentPeer = property do
  outcome <- liftIO $ bracket listening Socket.close \listener -> do
    port <- fromIntegral <$> Socket.socketPort listener
    withAsync (forever (Socket.accept listener >>= \(peer, _) -> void (withAsync (forever (SocketBS.recv peer 4096)) \_ -> threadDelay 10_000_000))) \_ ->
      Rpc.connect Rpc.defaultDialing{Rpc.pinging = Just Rpc.Keepalive{Rpc.every = Rpc.milliseconds 100, Rpc.within = Rpc.milliseconds 200}} (Rpc.Tcp "127.0.0.1" port) \connection ->
        timeout 5_000_000 (try @RpcError (say (client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc)) "anyone there"))
  case outcome of
    Just (Left failed) -> failed.status.code === Unavailable
    Just (Right _) -> evalEither (Left @T.Text "a silent peer answered")
    Nothing -> evalEither (Left @T.Text "keepalive did not notice the silence")
 where
  listening = do
    socket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen socket 8
    pure socket

connectGet :: Int -> Property
connectGet port = property do
  replies <- liftIO do
    manager <- Http.newManager Http.defaultManagerSettings
    target <- either (ioError . userError . T.unpack) pure (Rpc.web manager Rpc.Connect (T.pack ("http://127.0.0.1:" <> show port)))
    let options = Rpc.defaultOptions{Rpc.httpGet = True}
    overHttp1 <- say (client options (Rpc.Http1 target)) "cacheable"
    overHttp2 <- Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
      say (client options (Rpc.Http2 connection Rpc.Connect)) "cacheable"
    pure [overHttp1, overHttp2]
  replies === ["cacheable", "cacheable"]

hugeHeaders :: Int -> Property
hugeHeaders port = property do
  outcome <- liftIO $ Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
    try @RpcError (say (client Rpc.defaultOptions{Rpc.requestMetadata = bulk} (Rpc.Http2 connection Rpc.Grpc)) "big")
  case outcome of
    Left failed -> failed.status.code === ResourceExhausted
    Right _ -> evalEither (Left @T.Text "oversized headers were accepted")
 where
  bulk = foldMap (\n -> Rpc.header ("x-bulk-" <> BC.pack (show n)) (BS.replicate 3000 0x61)) [0 .. 7 :: Int]
