{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Handler
  ( Handler
  , HandlerOf
  , Calling
  , CallingOf
  , Invoking (..)
  , Kind (..)
  , Served (..)
  , Direct
  , Streaming
  , Received (..)
  , unchanged
  , handler
  , calling
  , Options (..)
  , defaultOptions
  , Transport (..)
  , ClientInterceptor (..)
  ) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.ProtoLens (Message, decodeMessage)
import Data.ProtoLens.Service.Types (MethodInput, MethodOutput, MethodStreamingType, StreamingType (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Peculiar.Rpc.Calling (Calling, CallingOf, ClientInterceptor (..), Invoking (..), Options (..), Transport (..), calling, defaultOptions)
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Encoding (encoded)
import Peculiar.Rpc.Error (rpcError)
import Peculiar.Rpc.Intercept (Interceptor (..), routeOf)
import Peculiar.Rpc.Json (Parsing (..))
import Peculiar.Rpc.Received (Received (..), unchanged)
import Peculiar.Rpc.Rpc (Method, methodPath)
import Peculiar.Rpc.Translate (Translation, translationOf)
import Peculiar.Rpc.Validate (Validator, validatorOf, violations)
import Peculiar.Rpc.Violations (invalidRequest, unvalidated)

type family HandlerOf (kind :: StreamingType) (input :: Type) (output :: Type) :: Type where
  HandlerOf 'NonStreaming input output = Context -> input -> IO output
  HandlerOf 'ServerStreaming input output = Context -> input -> (output -> IO ()) -> IO ()
  HandlerOf 'ClientStreaming input output = Context -> IO (Maybe input) -> IO output
  HandlerOf 'BiDiStreaming input output = Context -> IO (Maybe input) -> (output -> IO ()) -> IO ()

type Handler service method =
  HandlerOf (MethodStreamingType service method) (MethodInput service method) (MethodOutput service method)

type Direct = Context -> BS.ByteString -> IO BS.ByteString

type Streaming = Context -> IO (Maybe Received) -> (BS.ByteString -> IO ()) -> IO ()

type Serving = Context -> IO (Maybe BS.ByteString) -> (BS.ByteString -> IO ()) -> IO ()

data Served = Served
  { path :: BS.ByteString
  , streaming :: Interceptor -> Streaming
  , direct :: Maybe (Interceptor -> Direct)
  , translation :: Parsing -> Translation
  }

class Kind (kind :: StreamingType) where
  directly
    :: (Method service method, kind ~ MethodStreamingType service method)
    => Proxy service
    -> Proxy method
    -> Validator (MethodInput service method)
    -> HandlerOf kind (MethodInput service method) (MethodOutput service method)
    -> Maybe Direct
  directly _ _ _ _ = Nothing
  serving
    :: (Method service method, kind ~ MethodStreamingType service method)
    => Proxy service
    -> Proxy method
    -> Validator (MethodInput service method)
    -> HandlerOf kind (MethodInput service method) (MethodOutput service method)
    -> Serving
  oneRequest :: Proxy kind -> Bool
  oneRequest _ = False

instance Kind 'NonStreaming where
  oneRequest _ = True
  directly _ _ validator run = Just \context bytes -> encoded <$> (run context =<< accepted validator bytes)
  serving _ _ validator run context next send = receiveOne validator next >>= run context >>= send . encoded

instance Kind 'ServerStreaming where
  oneRequest _ = True
  serving _ _ validator run context next send = receiveOne validator next >>= \input -> run context input (send . encoded)

instance Kind 'ClientStreaming where
  serving _ _ validator run context next send = run context (receiveNext validator next) >>= send . encoded

instance Kind 'BiDiStreaming where
  serving _ _ validator run context next send = run context (receiveNext validator next) (send . encoded)

handler
  :: forall service method
   . (Method service method, Kind (MethodStreamingType service method))
  => Proxy service
  -> Proxy method
  -> Handler service method
  -> Served
handler service method run =
  Served
    { path
    , streaming = \(Interceptor around) context next send ->
        if oneRequest (Proxy @(MethodStreamingType service method))
          then do
            first <- next
            replayed <- replaying first next
            around route context{requestBytes = (.raw) <$> first} \context' -> serving service method validator run context' (fmap (.binary) <$> replayed) send
          else around route context \context' -> serving service method validator run context' (fmap (.binary) <$> next) send
    , direct = fmap (\unary (Interceptor around) context bytes -> around route context (`unary` bytes)) (directly service method validator run)
    , translation = translationOf (Proxy @(MethodInput service method)) (Proxy @(MethodOutput service method))
    }
 where
  path = methodPath @service @method
  route = routeOf path
  validator = validatorOf @(MethodInput service method)

replaying :: Maybe Received -> IO (Maybe Received) -> IO (IO (Maybe Received))
replaying first next = do
  given <- newIORef False
  pure do
    done <- readIORef given
    writeIORef given True
    case (done, first) of
      (False, _) -> pure first
      (True, Nothing) -> pure Nothing
      (True, Just _) -> next

decoded :: (Message a) => BS.ByteString -> IO a
decoded = either (throwIO . rpcError Internal . T.pack) pure . decodeMessage

accepted :: (Message a) => Validator a -> BS.ByteString -> IO a
accepted validator bytes = do
  input <- decoded bytes
  now <- nanosecondsNow
  case violations validator now bytes of
    Right [] -> pure input
    Right found -> throwIO (invalidRequest found)
    Left failure -> throwIO (unvalidated failure)

nanosecondsNow :: IO Integer
nanosecondsNow = (\elapsed -> floor (toRational (nominalDiffTimeToSeconds elapsed) * 1000000000)) <$> getPOSIXTime

receiveOne :: (Message a) => Validator a -> IO (Maybe BS.ByteString) -> IO a
receiveOne validator next =
  next >>= \case
    Just bytes ->
      next >>= \case
        Nothing -> accepted validator bytes
        Just _ -> throwIO (rpcError Unimplemented "a unary request carried more than one message")
    Nothing -> throwIO (rpcError Unimplemented "a unary request carried no message")

receiveNext :: (Message a) => Validator a -> IO (Maybe BS.ByteString) -> IO (Maybe a)
receiveNext validator next = next >>= traverse (accepted validator)
