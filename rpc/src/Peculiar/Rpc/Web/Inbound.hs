module Peculiar.Rpc.Web.Inbound
  ( Call (..)
  , Arriving (..)
  , Pull
  , Messages
  , newCall
  , supported
  , messagesOf
  , framedFrom
  , fromText
  , whole
  , once
  , settle
  , bounded
  , failed
  , tooLarge
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Bits (shift)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Functor (($>), (<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (SockAddr)
import Peculiar.Rpc.Compression (compressionNamed, decompress)
import Peculiar.Rpc.Core.Base64 (feedText, textDecoder)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Codec (Codec)
import Peculiar.Rpc.Core.Frame (Frame (..), FrameError (..), Kind (..), decoder, encodeStrict, feed)
import Peculiar.Rpc.Core.Frame qualified as Frame
import Peculiar.Rpc.Core.Headers (lookupHeader, statusHeaders)
import Peculiar.Rpc.Core.Status (Status, failing)
import Peculiar.Rpc.Core.Timeout (Timeout (..), parseGrpcTimeout)
import Peculiar.Rpc.Core.Trailers (Header)
import Peculiar.Rpc.Deadline (Deadlines, within)

type Pull = IO (BS.ByteString, Bool)

type Messages = IO (Maybe BS.ByteString)

data Call = Call
  { deadlines :: Deadlines
  , peer :: Maybe SockAddr
  , path :: BS.ByteString
  , codec :: Codec
  , limit :: Maybe Int
  , encoding :: Maybe BS.ByteString
  , deadline :: Maybe Word64
  , override :: IORef (Maybe Status)
  }

data Arriving = Arriving
  { peer :: Maybe SockAddr
  , path :: BS.ByteString
  , codec :: Codec
  , limit :: Maybe Int
  }

newCall :: Deadlines -> Arriving -> [Header] -> IO Call
newCall deadlines Arriving{peer, path, codec, limit} headers = do
  override <- newIORef Nothing
  now <- getMonotonicTimeNSec
  let deadline = do
        raw <- lookupHeader "grpc-timeout" headers
        Timeout nanos <- parseGrpcTimeout raw
        pure (now + fromIntegral (nanos - transit nanos))
  pure Call{deadlines, peer, path, codec, limit, encoding = BC.map toLower <$> lookupHeader "grpc-encoding" headers, deadline, override}

transit :: Integer -> Integer
transit nanos = min 10_000_000 (shift nanos (-4))

supported :: Maybe BS.ByteString -> Bool
supported = \case
  Nothing -> True
  Just name -> BC.map toLower name == "identity" || isJust (compressionNamed name)

cut :: Call -> Status -> IO ()
cut call status = modifyIORef' call.override (<|> Just status)

tooLarge :: Status
tooLarge = failing ResourceExhausted "message is larger than this server accepts"

admit :: Call -> Frame -> IO (Either Status Frame)
admit call frame
  | not frame.compressed = pure (allowed frame.payload)
  | otherwise = case compressionNamed =<< call.encoding of
      Nothing -> pure (Left undeclared)
      Just compression ->
        decompress compression call.limit frame.payload <&> \case
          Left reason -> Left (failing Internal ("the message could not be decompressed: " <> reason))
          Right payload -> allowed payload
 where
  allowed payload
    | any (BS.length payload >) call.limit = Left tooLarge
    | otherwise = Right frame{compressed = False, payload}

  undeclared = failing Internal "a compressed message arrived without a declared compression"

messagesOf :: Call -> Pull -> IO Messages
messagesOf call next = do
  decoding <- newIORef (Just (decoder call.limit))
  pending <- newIORef []
  let pull =
        readIORef pending >>= \case
          payload : rest -> writeIORef pending rest $> Just payload
          [] ->
            readIORef decoding >>= \case
              Nothing -> pure Nothing
              Just current -> do
                (chunk, _) <- next
                if BS.null chunk
                  then do
                    writeIORef decoding Nothing
                    case Frame.finish current of
                      Left _ -> cut call (failing Internal "the request ended inside a message")
                      Right () -> pure ()
                    pure Nothing
                  else case feed current chunk of
                    Left (TooLarge _) -> stop tooLarge
                    Left malformed -> stop (failing Internal (T.pack (show malformed)))
                    Right (frames, current') -> do
                      admitted <- traverse (admit call) [frame | frame <- frames, frame.kind == Message]
                      case sequence admitted of
                        Left status -> stop status
                        Right ready -> do
                          writeIORef decoding (Just current')
                          writeIORef pending [frame.payload | frame <- ready]
                          pull
      stop status = do
        cut call status
        writeIORef decoding Nothing
        writeIORef pending []
        void (forkIO (discarding next))
        pure Nothing
  pure pull

framedFrom :: Messages -> Pull
framedFrom next = maybe (BS.empty, True) (\payload -> (encodeStrict (Frame.message payload), False)) <$> next

discarding :: Pull -> IO ()
discarding = void . try @SomeException . discard

discard :: Pull -> IO ()
discard next = go (0 :: Int)
 where
  go seen = do
    (chunk, _) <- next
    let seen' = seen + BS.length chunk
    if BS.null chunk || seen' > 67108864 then pure () else go seen'

fromText :: Call -> Pull -> IO Pull
fromText call next = do
  state <- newIORef (Just textDecoder)
  pure (pull state)
 where
  pull state =
    readIORef state >>= \case
      Nothing -> pure (BS.empty, True)
      Just current -> do
        (chunk, _) <- next
        if BS.null chunk
          then writeIORef state Nothing $> (BS.empty, True)
          else case feedText current chunk of
            Left reason -> do
              cut call (failing InvalidArgument reason)
              writeIORef state Nothing
              void (forkIO (discarding next))
              pure (BS.empty, True)
            Right (decoded, current') -> do
              writeIORef state (Just current')
              if BS.null decoded then pull state else pure (decoded, False)

whole :: Call -> Bool -> Pull -> IO (Either Status Frame)
whole call compressed next = go [] 0
 where
  cap = fmap (if compressed then Frame.allowance else id) call.limit
  go chunks size = do
    (chunk, _) <- next
    let size' = size + BS.length chunk
    if
      | BS.null chunk -> admit call (Frame Frame.Message compressed (BS.concat (reverse chunks)))
      | any (size' >) cap -> discard next $> Left tooLarge
      | otherwise -> go (chunk : chunks) size'

once :: BS.ByteString -> IO Pull
once bytes = do
  given <- newIORef False
  pure do
    done <- readIORef given
    writeIORef given True
    pure (if done then (BS.empty, True) else (bytes, True))

bounded :: Call -> IO () -> IO ()
bounded call action = case call.deadline of
  Nothing -> action
  Just at -> within call.deadlines at action >>= maybe (cut call expiredStatus) pure

expiredStatus :: Status
expiredStatus = failing DeadlineExceeded "the deadline passed before the call finished"

failed :: Call -> IO Bool
failed call = isJust <$> readIORef call.override

settle :: Call -> [Header] -> IO [Header]
settle call trailers = do
  replaced <- readIORef call.override
  case replaced of
    Just status -> pure (statusHeaders status <> filter (not . isStatus . fst) trailers)
    Nothing
      | isJust (lookup "grpc-status" trailers) -> pure trailers
      | otherwise -> do
          now <- getMonotonicTimeNSec
          let expired = any (now >=) call.deadline
              status = if expired then expiredStatus else failing Unknown "the call ended without a status"
          pure (statusHeaders status <> trailers)
 where
  isStatus name = name `elem` ["grpc-status", "grpc-message", "grpc-status-details-bin"]
