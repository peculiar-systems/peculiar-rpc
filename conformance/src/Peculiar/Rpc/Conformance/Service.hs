{-# LANGUAGE TemplateHaskell #-}

module Peculiar.Rpc.Conformance.Service
  ( ConformanceServiceServer (..)
  , conformance
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Any qualified as Any
import Data.ProtoLens.Labels ()
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Context (Context (..))
import Peculiar.Rpc.Core.Code qualified as Code
import Peculiar.Rpc.Core.Status qualified as Core
import Peculiar.Rpc.Core.Timeout (timeoutMillis)
import Peculiar.Rpc.Derive (deriveServer)
import Peculiar.Rpc.Error (RpcError (..), rpcError)
import Peculiar.Rpc.Metadata (Entry (..), Metadata (..), binary)
import Peculiar.Rpc.Metadata qualified as Metadata
import Proto.Connectrpc.Conformance.V1.Service
import Proto.Google.Protobuf.Any (Any)

deriveServer ''ConformanceService

conformance :: ConformanceServiceServer
conformance =
  ConformanceServiceServer
    { bidiStream
    , clientStream
    , idempotentUnary
    , serverStream
    , unary
    , unimplemented
    }

unary :: Context -> UnaryRequest -> IO UnaryResponse
unary context request =
  answerOnce context (request ^. #maybe'responseDefinition) [Any.pack request] \payload ->
    defMessage & #payload .~ payload

idempotentUnary :: Context -> IdempotentUnaryRequest -> IO IdempotentUnaryResponse
idempotentUnary context request =
  answerOnce context (request ^. #maybe'responseDefinition) [Any.pack request] \payload ->
    defMessage & #payload .~ payload

clientStream :: Context -> IO (Maybe ClientStreamRequest) -> IO ClientStreamResponse
clientStream context receive = do
  requests <- receiveAll receive
  let definition = case requests of
        first : _ -> first ^. #maybe'responseDefinition
        [] -> Nothing
  answerOnce context definition (fmap Any.pack requests) \payload ->
    defMessage & #payload .~ payload

serverStream :: Context -> ServerStreamRequest -> (ServerStreamResponse -> IO ()) -> IO ()
serverStream context request send =
  answerStream context (request ^. #maybe'responseDefinition) (requestInfo context [Any.pack request]) \payload ->
    send (defMessage & #payload .~ payload)

bidiStream :: Context -> IO (Maybe BidiStreamRequest) -> (BidiStreamResponse -> IO ()) -> IO ()
bidiStream context receive send =
  receive >>= \case
    Nothing -> pure ()
    Just first
      | first ^. #fullDuplex -> fullDuplex context first receive send
      | otherwise -> do
          rest <- receiveAll receive
          answerStream context (first ^. #maybe'responseDefinition) (requestInfo context (fmap Any.pack (first : rest))) \payload ->
            send (defMessage & #payload .~ payload)

fullDuplex :: Context -> BidiStreamRequest -> IO (Maybe BidiStreamRequest) -> (BidiStreamResponse -> IO ()) -> IO ()
fullDuplex context first receive send = do
  let definition = fromMaybe defMessage (first ^. #maybe'responseDefinition)
      responses = definition ^. #responseData
  case first ^. #maybe'responseDefinition of
    Just _ -> do
      context.respondWith (metadataOf (definition ^. #responseHeaders))
      context.trailWith (metadataOf (definition ^. #responseTrailers))
      context.flush
    Nothing -> pure ()
  let loop index request
        | index >= length responses = pure index
        | otherwise = do
            let info =
                  if index == 0
                    then requestInfo context [Any.pack request]
                    else defMessage & #requests .~ [Any.pack request]
            pause (definition ^. #responseDelayMs)
            send (defMessage & #payload .~ (defMessage & #data' .~ (responses !!? index) & #requestInfo .~ info))
            receive >>= \case
              Just next -> loop (index + 1) next
              Nothing -> pure (index + 1)
  sent <- loop (0 :: Int) first
  finish definition sent (requestInfo context [Any.pack first])

unimplemented :: Context -> UnimplementedRequest -> IO UnimplementedResponse
unimplemented _ _ = throwIO (rpcError Code.Unimplemented "")

receiveAll :: IO (Maybe a) -> IO [a]
receiveAll receive =
  receive >>= \case
    Just item -> (item :) <$> receiveAll receive
    Nothing -> pure []

answerOnce :: Context -> Maybe UnaryResponseDefinition -> [Any] -> (ConformancePayload -> response) -> IO response
answerOnce context definition requests wrap = do
  let info = requestInfo context requests
      chosen = fromMaybe defMessage definition
  context.respondWith (metadataOf (chosen ^. #responseHeaders))
  context.trailWith (metadataOf (chosen ^. #responseTrailers))
  pause (chosen ^. #responseDelayMs)
  case chosen ^. #maybe'response of
    Just (UnaryResponseDefinition'Error failure) -> do
      context.flush
      throwIO (failed failure (Just info))
    Just (UnaryResponseDefinition'ResponseData bytes) ->
      pure (wrap (defMessage & #data' .~ bytes & #requestInfo .~ info))
    Nothing ->
      pure (wrap (defMessage & #requestInfo .~ info))

answerStream :: Context -> Maybe StreamResponseDefinition -> ConformancePayload'RequestInfo -> (ConformancePayload -> IO ()) -> IO ()
answerStream context definition info send = case definition of
  Nothing -> pure ()
  Just chosen -> do
    context.respondWith (metadataOf (chosen ^. #responseHeaders))
    context.trailWith (metadataOf (chosen ^. #responseTrailers))
    context.flush
    let responses = chosen ^. #responseData
    sequence_
      [ pause (chosen ^. #responseDelayMs) *> send (withInfo first (defMessage & #data' .~ bytes))
      | (first, bytes) <- zip (True : repeat False) responses
      ]
    finish chosen (length responses) info
 where
  withInfo first payload
    | first = payload & #requestInfo .~ info
    | otherwise = payload

finish :: StreamResponseDefinition -> Int -> ConformancePayload'RequestInfo -> IO ()
finish definition sent info = case definition ^. #maybe'error of
  Just failure -> throwIO (failed failure (if sent == 0 then Just info else Nothing))
  Nothing -> pure ()

failed :: Error -> Maybe ConformancePayload'RequestInfo -> RpcError
failed failure info =
  RpcError
    { status =
        Core.Status
          { code = fromMaybe Code.Unknown (Code.codeFromNumber (fromEnum (failure ^. #code)))
          , message = fromMaybe "" (failure ^. #maybe'message)
          , details = fmap detail (failure ^. #details <> foldMap (pure . Any.pack) info)
          }
    , metadata = mempty
    }
 where
  detail any' = Core.Detail{typeUrl = any' ^. #typeUrl, value = any' ^. #value}

requestInfo :: Context -> [Any] -> ConformancePayload'RequestInfo
requestInfo context requests =
  defMessage
    & #requestHeaders .~ grouped
    & #maybe'timeoutMs .~ fmap (fromIntegral . timeoutMillis) context.timeout
    & #requests .~ requests
 where
  Metadata received = context.metadata
  grouped =
    [ defMessage & #name .~ name & #value .~ values
    | (name, values) <- Map.toList (Map.fromListWith (flip (<>)) (fmap entry received))
    ]
  entry metadata = (TE.decodeUtf8Lenient metadata.name, [TE.decodeUtf8Lenient (if binary metadata then B64.encode metadata.value else metadata.value)])

metadataOf :: [Header] -> Metadata
metadataOf headers =
  mconcat
    [ Metadata.header name (encodedValue name value)
    | given <- headers
    , let name = BC.map toLower (TE.encodeUtf8 (given ^. #name))
    , value <- given ^. #value
    ]
 where
  encodedValue name value
    | "-bin" `BS.isSuffixOf` name = fromRight (TE.encodeUtf8 value) (B64.decode (TE.encodeUtf8 value))
    | otherwise = TE.encodeUtf8 value

pause :: Word32 -> IO ()
pause ms = when (ms > 0) (threadDelay (fromIntegral ms * 1000))

(!!?) :: [BS.ByteString] -> Int -> BS.ByteString
list !!? index = case drop index list of
  item : _ -> item
  [] -> BS.empty
