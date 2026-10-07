module Peculiar.Rpc.Deadline
  ( Deadlines
  , withDeadlines
  , within
  ) where

import Control.Concurrent (ThreadId, getNumCapabilities, myThreadId, threadCapability, threadDelay, throwTo)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (Exception, bracket, handleJust)
import Control.Monad (filterM, forever, replicateM, unless, void)
import Data.Array (Array, elems, listArray, (!))
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Event (TimeoutKey, getSystemTimerManager, registerTimeout, unregisterTimeout)
import GHC.IORef (atomicModifyIORef'_)
import Peculiar.Rpc.Core.Timeout (Timeout (..), timeoutMicros)

data Deadlines = Deadlines
  { shards :: Array Int (IORef [Entry])
  , count :: Int
  }

data State
  = Waiting
  | Armed TimeoutKey
  | Over

data Entry = Entry
  { due :: Word64
  , target :: ThreadId
  , state :: MVar State
  }

newtype Expired = Expired (MVar State)
  deriving stock (Eq)
  deriving anyclass (Exception)

instance Show Expired where
  show _ = "the deadline passed"

period :: Word64
period = 50_000_000

withDeadlines :: (Deadlines -> IO a) -> IO a
withDeadlines use = do
  count <- getNumCapabilities
  shards <- listArray (0, count - 1) <$> replicateM count (newIORef [])
  let deadlines = Deadlines{shards, count}
  withAsync (sweeping deadlines) (const (use deadlines))

within :: Deadlines -> Word64 -> IO a -> IO (Maybe a)
within deadlines due action = do
  target <- myThreadId
  state <- newMVar Waiting
  handleJust
    (\(Expired caught) -> if caught == state then Just () else Nothing)
    (const (pure Nothing))
    (bracket (enter deadlines Entry{due, target, state}) leave (const (Just <$> action)))

enter :: Deadlines -> Entry -> IO Entry
enter deadlines entry = do
  let due = entry.due
  now <- getMonotonicTimeNSec
  if due <= now + 2 * period
    then arm now entry
    else do
      (capability, _) <- threadCapability entry.target
      void (atomicModifyIORef'_ (deadlines.shards ! min (deadlines.count - 1) capability) (entry :))
  pure entry

leave :: Entry -> IO ()
leave entry =
  modifyMVar_ entry.state \case
    Armed key -> (getSystemTimerManager >>= (`unregisterTimeout` key)) $> Over
    _ -> pure Over

arm :: Word64 -> Entry -> IO ()
arm now entry
  | entry.due <= now = expire entry
  | otherwise =
      modifyMVar_ entry.state \case
        Waiting -> do
          manager <- getSystemTimerManager
          Armed <$> registerTimeout manager (fromIntegral (timeoutMicros (Timeout (toInteger (entry.due - now))))) (expire entry)
        other -> pure other

expire :: Entry -> IO ()
expire entry =
  modifyMVar_ entry.state \case
    Over -> pure Over
    _ -> throwTo entry.target (Expired entry.state) $> Over

sweeping :: Deadlines -> IO ()
sweeping deadlines = forever do
  threadDelay (fromIntegral (timeoutMicros (Timeout (toInteger period))))
  now <- getMonotonicTimeNSec
  for_ (elems deadlines.shards) \shard -> do
    taken <- atomicModifyIORef' shard ([],)
    kept <- filterM (pending now) taken
    unless (null kept) (atomicModifyIORef' shard \fresh -> (fresh <> kept, ()))
 where
  pending now entry =
    readMVar entry.state >>= \case
      Waiting
        | entry.due <= now + 2 * period -> arm now entry $> False
        | otherwise -> pure True
      _ -> pure False
