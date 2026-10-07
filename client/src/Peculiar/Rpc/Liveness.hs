module Peculiar.Rpc.Liveness
  ( Keepalive (..)
  , Liveness
  , Policy (..)
  , newLiveness
  , heard
  , tracking
  , supervise
  , activeCalls
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Peculiar.Rpc.Core.Timeout (Timeout (..))
import Peculiar.Rpc.Watch (goawayFrame, pingFrame)

data Keepalive = Keepalive
  { every :: Timeout
  , within :: Timeout
  }

data Liveness = Liveness
  { lastHeard :: IORef Word64
  , active :: TVar Int
  , quietSince :: TVar Word64
  , opened :: Word64
  }

data Policy = Policy
  { keepalive :: Maybe Keepalive
  , idle :: Maybe Timeout
  , maxAge :: Maybe Timeout
  , closing :: STM Bool
  , announce :: Bool
  , grace :: Maybe Timeout
  }

newLiveness :: IO Liveness
newLiveness = do
  now <- getMonotonicTimeNSec
  Liveness <$> newIORef now <*> newTVarIO 0 <*> newTVarIO now <*> pure now

heard :: Liveness -> IO ()
heard liveness = getMonotonicTimeNSec >>= writeIORef liveness.lastHeard

activeCalls :: Liveness -> IO Int
activeCalls liveness = readTVarIO liveness.active

tracking :: Liveness -> IO a -> IO a
tracking liveness action = do
  atomically (modifyTVar' liveness.active (+ 1))
  action `finally` do
    now <- getMonotonicTimeNSec
    atomically do
      modifyTVar' liveness.active (subtract 1)
      remaining <- readTVar liveness.active
      when (remaining == 0) (writeTVar liveness.quietSince now)

data Watch = Watch
  { pinged :: Maybe Word64
  , goneAway :: Maybe Word64
  }

supervise :: Policy -> Liveness -> (BS.ByteString -> IO ()) -> IO ()
supervise policy liveness inject = go Watch{pinged = Nothing, goneAway = Nothing}
 where
  tick = 100_000
  since now start = toInteger (now - start)
  go watch = do
    threadDelay tick
    now <- getMonotonicTimeNSec
    lastHeard <- readIORef liveness.lastHeard
    (calls, quiet, stopping) <- atomically ((,,) <$> readTVar liveness.active <*> readTVar liveness.quietSince <*> policy.closing)
    let answered = all (lastHeard >=) watch.pinged
        dead = case (policy.keepalive, watch.pinged) of
          (Just alive, Just at) -> not answered && since now at >= alive.within.nanos
          _ -> False
        dueForPing = case policy.keepalive of
          Just alive -> answered && since now lastHeard >= alive.every.nanos
          Nothing -> False
        idleTooLong = calls == 0 && any (\limit -> since now quiet >= limit.nanos) policy.idle
        tooOld = any (\limit -> since now liveness.opened >= limit.nanos) policy.maxAge
        leaving = stopping || idleTooLong || tooOld
        overdue = case (watch.goneAway, policy.grace) of
          (Just at, Just limit) -> since now at >= limit.nanos
          _ -> False
    if
      | dead -> pure ()
      | Just _ <- watch.goneAway, calls == 0 || overdue -> threadDelay tick
      | leaving
      , Nothing <- watch.goneAway -> do
          when policy.announce (inject goawayFrame)
          go watch{goneAway = Just now}
      | dueForPing -> do
          inject pingFrame
          go watch{pinged = Just now}
      | otherwise -> go watch
