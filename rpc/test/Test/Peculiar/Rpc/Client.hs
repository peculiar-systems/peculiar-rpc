{-# LANGUAGE RankNTypes #-}

module Test.Peculiar.Rpc.Client (tests) where

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.ProtoLens (Message, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.String (fromString)
import Data.Text qualified as T
import Data.Traversable (for)
import GHC.Clock (getMonotonicTimeNSec)
import Hedgehog (Group (..), Property, PropertyName, assert, evalEither, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Network.HTTP.Client qualified as Http
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Json qualified as Json
import Peculiar.Rpc.Service (Client (..))
import Proto.Peculiar.Test.V1.Echo (CountRequest, SayRequest)
import Test.Peculiar.Rpc.Echo (EchoClient (..))

data Way = Way T.Text Rpc.Codec (forall a. Int -> (Rpc.Transport -> IO a) -> IO a)

ways :: [Way]
ways =
  [ Way (label <> codecLabel codec) codec open
  | codec <- [Rpc.Proto, Rpc.Json]
  , (label, Opening open) <-
      [ ("HTTP/1.1 Connect", Opening (overHttp1 Rpc.Connect))
      , ("HTTP/1.1 gRPC-Web", Opening (overHttp1 Rpc.GrpcWeb))
      , ("HTTP/2 gRPC", Opening (overHttp2 Rpc.Grpc))
      , ("HTTP/2 Connect", Opening (overHttp2 Rpc.Connect))
      , ("HTTP/2 gRPC-Web", Opening (overHttp2 Rpc.GrpcWeb))
      ]
  ]
 where
  codecLabel = \case
    Rpc.Proto -> ""
    Rpc.Json -> " JSON"

newtype Opening = Opening (forall a. Int -> (Rpc.Transport -> IO a) -> IO a)

optionsFor :: Way -> Rpc.Options
optionsFor (Way _ codec _) = Rpc.defaultOptions{Rpc.codec = codec}

overHttp1 :: Rpc.Protocol -> Int -> (Rpc.Transport -> IO a) -> IO a
overHttp1 protocol port use = do
  manager <- Http.newManager Http.defaultManagerSettings
  target <- either (ioError . userError . T.unpack) pure (Rpc.web manager protocol (T.pack ("http://127.0.0.1:" <> show port)))
  use (Rpc.Http1 target)

overHttp2 :: Rpc.Protocol -> Int -> (Rpc.Transport -> IO a) -> IO a
overHttp2 protocol port use = Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection -> use (Rpc.Http2 connection protocol)

tests :: Int -> Group
tests port = Group "Client" (foldMap (cases port) ways <> policies port)

cases :: Int -> Way -> [(PropertyName, Property)]
cases port way =
  [ (named "unary", withTests 1 (unary port way))
  , (named "server streaming", withTests 1 (serverStreaming port way))
  , (named "client streaming", withTests 1 (clientStreaming port way))
  , (named "bidi", withTests 1 (bidi port way))
  , (named "a typed error", withTests 1 (failure port way))
  , (named "gzip both ways", withTests 1 (compressed Rpc.Gzip port way))
  , (named "zstd streams", withTests 1 (compressedStream Rpc.Zstd port way))
  , (named "a unary handler sees its path and request bytes", withTests 1 (unaryRequest port way))
  , (named "a server stream sees its path and request bytes", withTests 1 (streamRequest port way))
  , (named "a bidi stream has no request bytes", withTests 1 (bidiRequest port way))
  ]
 where
  named what = fromString (T.unpack (labelOf way <> ", " <> what))

policies :: Int -> [(PropertyName, Property)]
policies port =
  [ ("snappy, deflate and brotli requests", withTests 1 (everyCodec port))
  , ("retries repeat an unavailable call and give up", withTests 1 (retries port))
  , ("retries leave other failures alone", withTests 1 (noRetry port))
  , ("a server's pushback delays the retry", withTests 1 (pushedBack port))
  , ("a server can refuse retries", withTests 1 (refusedRetry port))
  , ("a client interceptor rewrites the options", withTests 1 (clientInterceptor port))
  , ("a server interceptor can refuse a call", withTests 1 (serverInterceptor port))
  ]

labelOf :: Way -> T.Text
labelOf (Way label _ _) = label

through :: Way -> Int -> (Rpc.Transport -> IO a) -> IO a
through (Way _ _ open) = open

remote :: Rpc.Options -> Rpc.Transport -> EchoClient
remote = client

collecting :: ((a -> IO ()) -> IO ()) -> IO [a]
collecting run = do
  seen <- newIORef []
  run \item -> modifyIORef' seen (item :)
  reverse <$> readIORef seen

unary :: Int -> Way -> Property
unary port way = property do
  reply <- liftIO $ through way port \transport -> (remote (optionsFor way) transport).say (defMessage & #text .~ "hello")
  reply ^. #text === "hello"

serverStreaming :: Int -> Way -> Property
serverStreaming port way = property do
  numbers <- liftIO $ through way port \transport -> collecting ((remote (optionsFor way) transport).count (defMessage & #upto .~ 3))
  fmap (^. #n) numbers === [1, 2, 3]

clientStreaming :: Int -> Way -> Property
clientStreaming port way = property do
  reply <- liftIO $ through way port \transport ->
    (remote (optionsFor way) transport).sum \send -> for_ [1 .. 4] \n -> send (defMessage & #n .~ n)
  reply ^. #total === 10

bidi :: Int -> Way -> Property
bidi port way = property do
  replies <- liftIO $ through way port \transport ->
    collecting ((remote (optionsFor way) transport).chat \send -> for_ ["x", "y"] \text -> send (defMessage & #text .~ text))
  fmap (^. #text) replies === ["X", "Y"]

failure :: Int -> Way -> Property
failure port way = property do
  outcome <- liftIO $ through way port \transport ->
    try @RpcError ((remote (optionsFor way) transport).say (defMessage & #failCode .~ 7 & #failMessage .~ "no"))
  case outcome of
    Left failed -> (failed.status.code, failed.status.message) === (PermissionDenied, "no")
    Right _ -> evalEither (Left @T.Text "the call succeeded")

compressed :: Rpc.Compression -> Int -> Way -> Property
compressed compression port way = property do
  let long = T.replicate 60 "compressible "
  reply <- liftIO $ through way port \transport ->
    (remote (optionsFor way){Rpc.compression = Just compression, Rpc.compressAbove = 0} transport).say (defMessage & #text .~ long)
  reply ^. #text === long

compressedStream :: Rpc.Compression -> Int -> Way -> Property
compressedStream compression port way = property do
  reply <- liftIO $ through way port \transport ->
    (remote (optionsFor way){Rpc.compression = Just compression, Rpc.compressAbove = 0} transport).sum \send -> for_ [1 .. 20] \n -> send (defMessage & #n .~ n)
  reply ^. #total === 210

echoed :: Way -> (Rpc.Options -> Rpc.Transport -> IO ()) -> Int -> IO Rpc.Metadata
echoed way call port = do
  seen <- newIORef mempty
  through way port (call (optionsFor way){Rpc.requestMetadata = Rpc.header "x-echo-request" "yes", Rpc.onHeaders = writeIORef seen})
  readIORef seen

sent :: (Message m) => Way -> m -> BS.ByteString
sent (Way _ codec _) request = case codec of
  Rpc.Proto -> encodeMessage request
  Rpc.Json -> fromRight BS.empty ((Json.codec (Json.defaultParsing Json.wellKnown)).encoded request)

unaryRequest :: Int -> Way -> Property
unaryRequest port way = property do
  let request = defMessage & #text .~ "signed" :: SayRequest
  heard <- liftIO $ echoed way (\options transport -> void ((remote options transport).say request)) port
  Rpc.lookupHeader "x-path" heard === Just "/peculiar.test.v1.Echo/Say"
  Rpc.lookupHeader "x-request-bin" heard === Just (sent way request)

streamRequest :: Int -> Way -> Property
streamRequest port way = property do
  let request = defMessage & #upto .~ 2 :: CountRequest
  heard <- liftIO $ echoed way (\options transport -> (remote options transport).count request (const (pure ()))) port
  Rpc.lookupHeader "x-path" heard === Just "/peculiar.test.v1.Echo/Count"
  Rpc.lookupHeader "x-request-bin" heard === Just (sent way request)

bidiRequest :: Int -> Way -> Property
bidiRequest port way = property do
  heard <- liftIO $ echoed way (\options transport -> (remote options transport).chat (\send -> send (defMessage & #text .~ "x")) (const (pure ()))) port
  Rpc.lookupHeader "x-path" heard === Just "/peculiar.test.v1.Echo/Chat"
  Rpc.lookupHeader "x-request-bin" heard === Nothing

pushedBack :: Int -> Property
pushedBack port = property do
  for_ [Rpc.Grpc, Rpc.Connect, Rpc.GrpcWeb] \protocol -> do
    (outcome, attempts, waited) <- liftIO $ overHttp2 protocol port \transport -> do
      counter <- newIORef (0 :: Int)
      let options =
            Rpc.defaultOptions
              { Rpc.retry = Just Rpc.defaultRetry{Rpc.attempts = 2, Rpc.initialBackoff = Rpc.milliseconds 1}
              , Rpc.onTrailers = \_ -> atomicModifyIORef' counter \n -> (n + 1, ())
              }
      started <- getMonotonicTimeNSec
      outcome <- try @RpcError ((remote options transport).say (defMessage & #text .~ "push back"))
      finished <- getMonotonicTimeNSec
      attempts <- readIORef counter
      pure (outcome, attempts, finished - started)
    attempts === 2
    assert (waited >= 150_000_000)
    either (\failed -> Rpc.lookupHeader "grpc-retry-pushback-ms" failed.metadata === Just "150") (const (evalEither (Left @T.Text "the call succeeded"))) outcome

refusedRetry :: Int -> Property
refusedRetry port = property do
  attempts <- liftIO $ overHttp2 Rpc.Grpc port \transport -> do
    counter <- newIORef (0 :: Int)
    let options =
          Rpc.defaultOptions
            { Rpc.retry = Just Rpc.defaultRetry{Rpc.initialBackoff = Rpc.milliseconds 1}
            , Rpc.onTrailers = \_ -> atomicModifyIORef' counter \n -> (n + 1, ())
            }
    _ <- try @RpcError ((remote options transport).say (defMessage & #text .~ "do not retry"))
    readIORef counter
  attempts === 1

everyCodec :: Int -> Property
everyCodec port = property do
  replies <- liftIO $ overHttp2 Rpc.Grpc port \transport ->
    for [Rpc.Snappy, Rpc.Deflate, Rpc.Brotli] \compression ->
      (remote Rpc.defaultOptions{Rpc.compression = Just compression, Rpc.compressAbove = 0} transport).say (defMessage & #text .~ T.replicate 50 "abc")
  fmap (^. #text) replies === replicate 3 (T.replicate 50 "abc")

retries :: Int -> Property
retries port = property do
  (outcome, attempts) <- liftIO $ overHttp2 Rpc.Grpc port \transport -> do
    counter <- newIORef (0 :: Int)
    let options =
          Rpc.defaultOptions
            { Rpc.retry = Just Rpc.defaultRetry{Rpc.attempts = 3, Rpc.initialBackoff = Rpc.milliseconds 1}
            , Rpc.onTrailers = \_ -> atomicModifyIORef' counter \n -> (n + 1, ())
            }
    outcome <- try @RpcError ((remote options transport).say (defMessage & #failCode .~ 14 & #failMessage .~ "down"))
    (outcome,) <$> readIORef counter
  attempts === 3
  either (\failed -> failed.status.code === Unavailable) (const (evalEither (Left @T.Text "the call succeeded"))) outcome

noRetry :: Int -> Property
noRetry port = property do
  attempts <- liftIO $ overHttp2 Rpc.Grpc port \transport -> do
    counter <- newIORef (0 :: Int)
    let options =
          Rpc.defaultOptions
            { Rpc.retry = Just Rpc.defaultRetry{Rpc.initialBackoff = Rpc.milliseconds 1}
            , Rpc.onTrailers = \_ -> atomicModifyIORef' counter \n -> (n + 1, ())
            }
    _ <- try @RpcError ((remote options transport).say (defMessage & #failCode .~ 5 & #failMessage .~ "gone"))
    readIORef counter
  attempts === 1

clientInterceptor :: Int -> Property
clientInterceptor port = property do
  reply <- liftIO $ overHttp2 Rpc.Grpc port \transport -> do
    let stamping = Rpc.ClientInterceptor \route options continue ->
          continue options{Rpc.requestMetadata = options.requestMetadata <> Rpc.header "authorization" (fromString (T.unpack route.method))}
    (remote Rpc.defaultOptions{Rpc.clientInterceptor = stamping} transport).say (defMessage & #text .~ "whoami")
  reply ^. #text === "Say"

serverInterceptor :: Int -> Property
serverInterceptor port = property do
  outcome <- liftIO $ overHttp2 Rpc.Connect port \transport ->
    try @RpcError ((remote Rpc.defaultOptions{Rpc.requestMetadata = Rpc.header "x-refuse" "yes"} transport).say (defMessage & #text .~ "hi"))
  case outcome of
    Left failed -> (failed.status.code, failed.status.message) === (PermissionDenied, "refused before peculiar.test.v1.Echo/Say")
    Right _ -> evalEither (Left @T.Text "the call succeeded")
