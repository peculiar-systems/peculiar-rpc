{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Compression (compress, decompress)
import Peculiar.Rpc.Core.Base64 (encodeText, feedText, textDecoder)
import Peculiar.Rpc.Core.Frame (Frame (..), decodeAll, encodeStrict, message)
import Proto.Peculiar.Test.V1.Echo
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf, nfIO, whnfIO)

Rpc.deriveService ''Echo

echo :: EchoServer
echo = EchoServer{chat, count, say, sum = total}
 where
  say _ request = pure (defMessage & #text .~ request ^. #text)
  count _ request send = for_ [1 .. request ^. #upto] \n -> send (defMessage & #n .~ n)
  total _ receive =
    let go acc = receive >>= maybe (pure (defMessage & #total .~ acc)) (\request -> go (acc + request ^. #n))
     in go 0
  chat _ receive send =
    let go = receive >>= maybe (pure ()) (\request -> send (defMessage & #text .~ request ^. #text) *> go)
     in go

main :: IO ()
main = do
  bound <- newEmptyMVar
  stop <- newEmptyMVar
  let settings = Rpc.defaultSettings{Rpc.port = 0, Rpc.onListening = putMVar bound, Rpc.calls = Rpc.defaultCalls{Rpc.report = const (pure ())}, Rpc.stopWhen = takeMVar stop}
  withAsync (Rpc.serveEndpoints settings [Rpc.endpoint echo]) \_ -> do
    port <- takeMVar bound
    manager <- Http.newManager Http.defaultManagerSettings
    let url = T.pack ("http://127.0.0.1:" <> show port)
        overHttp1 protocol = either (ioError . userError . T.unpack) (pure . Rpc.Http1) (Rpc.web manager protocol url)
    connect <- overHttp1 Rpc.Connect
    grpcWeb <- overHttp1 Rpc.GrpcWeb
    Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> do
      let native = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: EchoClient
          connect2 = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Connect) :: EchoClient
      defaultMain [wire, calls native connect2 (Rpc.client Rpc.defaultOptions connect) (Rpc.client Rpc.defaultOptions grpcWeb)]
    putMVar stop ()

payload :: BS.ByteString
payload = BS.pack (take 65536 (cycle [0 .. 250]))

frames :: BS.ByteString
frames = foldMap (encodeStrict . message) (replicate 1000 (BS.take 1024 payload))

wire :: Benchmark
wire =
  bgroup
    "wire"
    [ bench "decode 1000 frames of 1 KiB" (nf (either (const 0) (sum . fmap (\frame -> BS.length frame.payload)) . decodeAll Nothing) frames)
    , bench "encode 1000 frames of 1 KiB" (nf (foldMap (encodeStrict . message)) (replicate 1000 (BS.take 1024 payload)))
    , bench "grpc-web-text decode 64 KiB" (nf (fmap fst . feedText textDecoder) (encodeText payload))
    , bgroup "compress 64 KiB" [bench (show codec) (nf (compress codec) payload) | codec <- [minBound ..]]
    , bgroup "decompress 64 KiB" [bench (show codec) (nfIO (decompress codec Nothing (compress codec payload))) | codec <- [minBound ..]]
    ]

calls :: EchoClient -> EchoClient -> EchoClient -> EchoClient -> Benchmark
calls native connect2 connect1 grpcWeb1 =
  bgroup
    "round trips"
    [ bench "unary, gRPC over HTTP/2" (whnfIO (native.say request))
    , bench "unary, Connect over HTTP/2" (whnfIO (connect2.say request))
    , bench "unary, Connect over HTTP/1.1" (whnfIO (connect1.say request))
    , bench "unary, gRPC-Web over HTTP/1.1" (whnfIO (grpcWeb1.say request))
    , bench "1 MiB unary, gRPC over HTTP/2" (whnfIO (native.say large))
    , bench "1000-message server stream, gRPC over HTTP/2" (nfIO (streamed native))
    , bench "1000-message client stream, gRPC over HTTP/2" (nfIO (summed native))
    , bench "1000-message bidi stream, gRPC over HTTP/2" (nfIO (chatted native))
    ]
 where
  request = defMessage & #text .~ "benchmark"
  large = defMessage & #text .~ T.replicate 1048576 "x"

streamed :: EchoClient -> IO Int
streamed remote = do
  seen <- newIORef 0
  remote.count (defMessage & #upto .~ 1000) \_ -> modifyIORef' seen (+ 1)
  readIORef seen

summed :: EchoClient -> IO Int
summed remote = do
  reply <- remote.sum \send -> for_ [1 .. 1000] \n -> send (defMessage & #n .~ n)
  pure (fromIntegral (reply ^. #total))

chatted :: EchoClient -> IO Int
chatted remote = do
  seen <- newIORef 0
  remote.chat (\send -> for_ [1 .. 1000 :: Int] \_ -> send (defMessage & #text .~ "x")) \_ -> modifyIORef' seen (+ 1)
  readIORef seen
