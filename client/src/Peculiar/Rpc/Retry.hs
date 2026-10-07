module Peculiar.Rpc.Retry
  ( Retry (..)
  , defaultRetry
  , retrying
  , pushback
  , noRetry
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Data.Bits ((.&.))
import Data.ByteString.Char8 qualified as BC
import GHC.Clock (getMonotonicTimeNSec)
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status (..))
import Peculiar.Rpc.Core.Timeout (Timeout (..), milliseconds, seconds, timeoutMicros, timeoutMillis)
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Metadata (Metadata, header, lookupHeader)

data Retry = Retry
  { attempts :: Int
  , initialBackoff :: Timeout
  , maxBackoff :: Timeout
  , multiplier :: Double
  , retryOn :: [Code]
  }

defaultRetry :: Retry
defaultRetry =
  Retry
    { attempts = 4
    , initialBackoff = milliseconds 100
    , maxBackoff = seconds 5
    , multiplier = 2
    , retryOn = [Unavailable]
    }

data Pause
  = Backoff
  | Pushback Integer
  | Stop

retrying :: Retry -> Maybe Timeout -> IO Bool -> (Maybe Timeout -> IO a) -> IO a
retrying retry deadline committed attempt = do
  started <- getMonotonicTimeNSec
  let remaining = do
        now <- getMonotonicTimeNSec
        pure (fmap (\limit -> Timeout (max 0 (limit.nanos - toInteger (now - started)))) deadline)
      go number backoff = do
        left <- remaining
        try @RpcError (attempt left) >>= \case
          Right result -> pure result
          Left failure -> do
            sent <- committed
            let pause = pauseFor failure
            if sent || number >= retry.attempts || failure.status.code `notElem` retry.retryOn
              then throwIO failure
              else case pause of
                Stop -> throwIO failure
                _ -> do
                  jitter <- (\nanos -> 0.5 + fromIntegral (nanos .&. 1023) / 2046) <$> getMonotonicTimeNSec
                  let wait = case pause of
                        Pushback nanos -> nanos
                        _ -> round (fromIntegral backoff * (jitter :: Double))
                  after <- remaining
                  if any (\budget -> budget.nanos <= wait) after
                    then throwIO failure
                    else do
                      threadDelay (fromIntegral (timeoutMicros (Timeout wait)))
                      go (number + 1) (min retry.maxBackoff.nanos (round (fromIntegral backoff * retry.multiplier)))
  go 1 retry.initialBackoff.nanos

pauseFor :: RpcError -> Pause
pauseFor failure = case lookupHeader "grpc-retry-pushback-ms" failure.metadata of
  Nothing -> Backoff
  Just raw -> case BC.readInteger raw of
    Just (millis, rest) | BC.null rest, millis >= 0 -> Pushback (millis * 1_000_000)
    _ -> Stop

pushback :: Timeout -> Metadata
pushback wait = header "grpc-retry-pushback-ms" (BC.pack (show (timeoutMillis wait)))

noRetry :: Metadata
noRetry = header "grpc-retry-pushback-ms" "-1"
