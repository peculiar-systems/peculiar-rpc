{-# LANGUAGE TemplateHaskell #-}

module Test.Peculiar.Rpc.Echo
  ( EchoServer (..)
  , EchoClient (..)
  , echo
  ) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code (codeFromNumber)
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Derive (deriveService)
import Peculiar.Rpc.Error (RpcError (..))
import Proto.Peculiar.Test.V1.Echo

deriveService ''Echo

echo :: EchoServer
echo = EchoServer{chat, count, say, sum = total}

say :: Context -> SayRequest -> IO SayResponse
say context request
  | request ^. #text == "whoami" = pure (defMessage & #text .~ maybe "anonymous" TE.decodeUtf8Lenient (Rpc.lookupHeader "authorization" context.metadata))
  | request ^. #text == "whereami" = pure (defMessage & #text .~ T.pack (show context.peer))
  | request ^. #text == "push back" = throwIO (Rpc.withMetadata (Rpc.pushback (Rpc.milliseconds 150)) (Rpc.rpcError Rpc.Unavailable "busy"))
  | request ^. #text == "do not retry" = throwIO (Rpc.withMetadata Rpc.noRetry (Rpc.rpcError Rpc.Unavailable "gone"))
  | otherwise = case codeFromNumber (fromIntegral (request ^. #failCode)) of
      Just code | request ^. #failCode /= 0 -> throwIO RpcError{status = Status{code, message = request ^. #failMessage, details = []}, metadata = mempty}
      _ -> pure (defMessage & #text .~ request ^. #text)

count :: Context -> CountRequest -> (CountResponse -> IO ()) -> IO ()
count _ request send = for_ [1 .. request ^. #upto] \n -> send (defMessage & #n .~ n)

total :: Context -> IO (Maybe SumRequest) -> IO SumResponse
total _ receive = go 0
 where
  go acc =
    receive >>= \case
      Just request -> go (acc + request ^. #n)
      Nothing -> pure (defMessage & #total .~ acc)

chat :: Context -> IO (Maybe ChatRequest) -> (ChatResponse -> IO ()) -> IO ()
chat context receive send =
  receive >>= \case
    Just request
      | request ^. #text == "whereami" -> send (defMessage & #text .~ T.pack (show context.peer)) *> chat context receive send
      | otherwise -> send (defMessage & #text .~ T.toUpper (request ^. #text)) *> chat context receive send
    Nothing -> pure ()
