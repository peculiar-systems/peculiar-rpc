{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Calling
  ( Calling
  , CallingOf
  , Invoking (..)
  , calling
  , Options (..)
  , defaultOptions
  , Transport (..)
  , ClientInterceptor (..)
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.ProtoLens.Service.Types (MethodInput, MethodOutput, MethodStreamingType, StreamingType (..))
import Data.Proxy (Proxy (..))
import Peculiar.Rpc.Client.Call qualified as Call
import Peculiar.Rpc.Client.Http1 (Target (..), http1)
import Peculiar.Rpc.Client.Http2 (Connection, connectionDeadlines, http2)
import Peculiar.Rpc.Compression (Compression)
import Peculiar.Rpc.Core.Client (Protocol)
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Timeout (Timeout (..))
import Peculiar.Rpc.Json (Parsing (..), Registry, wellKnown)
import Peculiar.Rpc.Metadata (Metadata)
import Peculiar.Rpc.Retry (Retry, retrying)
import Peculiar.Rpc.Route (Route, routeOf)
import Peculiar.Rpc.Rpc (Method, idempotent, methodPath)

type family CallingOf (kind :: StreamingType) (input :: Type) (output :: Type) :: Type where
  CallingOf 'NonStreaming input output = input -> IO output
  CallingOf 'ServerStreaming input output = input -> (output -> IO ()) -> IO ()
  CallingOf 'ClientStreaming input output = ((input -> IO ()) -> IO ()) -> IO output
  CallingOf 'BiDiStreaming input output = ((input -> IO ()) -> IO ()) -> (output -> IO ()) -> IO ()

type Calling service method =
  CallingOf (MethodStreamingType service method) (MethodInput service method) (MethodOutput service method)

data Transport
  = Http2 Connection Protocol
  | Http1 Target

data Options = Options
  { deadline :: Maybe Timeout
  , requestMetadata :: Metadata
  , onHeaders :: Metadata -> IO ()
  , onTrailers :: Metadata -> IO ()
  , compression :: Maybe Compression
  , compressAbove :: Int
  , retry :: Maybe Retry
  , clientInterceptor :: ClientInterceptor
  , httpGet :: Bool
  , codec :: Codec
  , registry :: Registry
  }

newtype ClientInterceptor = ClientInterceptor (forall a. Route -> Options -> (Options -> IO a) -> IO a)

instance Semigroup ClientInterceptor where
  ClientInterceptor outer <> ClientInterceptor inner =
    ClientInterceptor \route options continue -> outer route options \options' -> inner route options' continue

instance Monoid ClientInterceptor where
  mempty = ClientInterceptor \_ options continue -> continue options

defaultOptions :: Options
defaultOptions =
  Options
    { deadline = Nothing
    , requestMetadata = mempty
    , onHeaders = const (pure ())
    , onTrailers = const (pure ())
    , compression = Nothing
    , compressAbove = 1024
    , retry = Nothing
    , clientInterceptor = mempty
    , httpGet = False
    , codec = Proto
    , registry = wellKnown
    }

class Invoking (kind :: StreamingType) where
  invoking
    :: (Method service method, kind ~ MethodStreamingType service method)
    => Proxy service
    -> Proxy method
    -> Options
    -> Transport
    -> CallingOf kind (MethodInput service method) (MethodOutput service method)

instance Invoking 'NonStreaming where
  invoking (_ :: Proxy service) (_ :: Proxy method) options transport input =
    intercepted @service @method options \options' ->
      retried options' (pure False) \left -> Call.unary (callOf @service @method options' transport left) input

instance Invoking 'ServerStreaming where
  invoking (_ :: Proxy service) (_ :: Proxy method) options transport input receive =
    intercepted @service @method options \options' -> do
      delivered <- newIORef False
      retried options' (readIORef delivered) \left ->
        Call.serverStream (callOf @service @method options' transport left) input \output ->
          writeIORef delivered True *> receive output

instance Invoking 'ClientStreaming where
  invoking (_ :: Proxy service) (_ :: Proxy method) options transport produce =
    intercepted @service @method options \options' ->
      Call.clientStream (callOf @service @method options' transport options'.deadline) produce

instance Invoking 'BiDiStreaming where
  invoking (_ :: Proxy service) (_ :: Proxy method) options transport produce receive =
    intercepted @service @method options \options' ->
      Call.bidiStream (callOf @service @method options' transport options'.deadline) produce receive

intercepted :: forall service method a. (Method service method) => Options -> (Options -> IO a) -> IO a
intercepted options = case options.clientInterceptor of
  ClientInterceptor around -> around (routeOf (methodPath @service @method)) options

retried :: Options -> IO Bool -> (Maybe Timeout -> IO a) -> IO a
retried options committed attempt = case options.retry of
  Nothing -> attempt options.deadline
  Just policy -> retrying policy options.deadline committed attempt

callOf :: forall service method. (Method service method) => Options -> Transport -> Maybe Timeout -> Call.Call
callOf options transport timeout =
  Call.Call
    { exchanger
    , protocol
    , path = methodPath @service @method
    , timeout
    , codec = options.codec
    , parsing = Parsing{registry = options.registry, ignoreUnknown = True}
    , deadlines = case transport of
        Http2 connection _ -> Just (connectionDeadlines connection)
        Http1 _ -> Nothing
    , metadata = options.requestMetadata
    , compression = options.compression
    , compressAbove = options.compressAbove
    , cacheable = options.httpGet && idempotent @service @method
    , heard = options.onHeaders
    , trailed = options.onTrailers
    }
 where
  (exchanger, protocol) = case transport of
    Http2 connection over -> (http2 connection, over)
    Http1 target -> (http1 target, target.protocol)

calling
  :: forall service method
   . (Method service method, Invoking (MethodStreamingType service method))
  => Proxy service
  -> Proxy method
  -> Options
  -> Transport
  -> Calling service method
calling = invoking
