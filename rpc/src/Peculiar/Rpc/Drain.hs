module Peculiar.Rpc.Drain
  ( Gate
  , newGate
  , closingOf
  , admitted
  , tracked
  , drain
  , terminated
  ) where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, finally, mask_, try)
import Control.Monad (void, when)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (parseQuery, status503)
import Network.Wai qualified as Wai
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Status, failing)
import Peculiar.Rpc.Core.Timeout (Timeout, timeoutMicros)
import Peculiar.Rpc.Core.Wire (Classified (..), Incoming (..), Wire (..), classify)
import Peculiar.Rpc.Wai (incomingOf, reply)
import Peculiar.Rpc.Web (declined)
import System.Posix.Signals qualified as Signals
import System.Timeout (timeout)

data Gate = Gate
  { open :: TVar Bool
  , active :: TVar Int
  }

newGate :: IO Gate
newGate = Gate <$> newTVarIO True <*> newTVarIO 0

closingOf :: Gate -> STM Bool
closingOf gate = not <$> readTVar gate.open

admitted :: Gate -> Wai.Middleware
admitted gate app request respond = do
  entered <- atomically do
    opened <- readTVar gate.open
    when opened (modifyTVar' gate.active (+ 1))
    pure opened
  if entered
    then app request respond `finally` atomically (modifyTVar' gate.active (subtract 1))
    else refused request respond

refused :: Wai.Application
refused request respond
  | rpcShaped incoming = reply request respond (declined shuttingDown incoming)
  | otherwise = respond (Wai.responseLBS status503 [("content-type", "text/plain")] "the server is shutting down")
 where
  incoming = incomingOf request

rpcShaped :: Incoming -> Bool
rpcShaped incoming = case classify incoming of
  Native _ -> True
  Translated ConnectGet _ -> any ((== "encoding") . fst) (parseQuery (BC.dropWhile (/= '?') incoming.path))
  Translated _ _ -> True
  Rejected _ -> False

shuttingDown :: Status
shuttingDown = failing Unavailable "the server is shutting down"

tracked :: TVar (Map.Map ThreadId ()) -> IO () -> IO ()
tracked live action = mask_ do
  thread <-
    forkIOWithUnmask \unmask ->
      void (try @SomeException (unmask action))
        `finally` (myThreadId >>= \self -> atomically (modifyTVar' live (Map.delete self)))
  atomically (modifyTVar' live (Map.insert thread ()))

drain :: Gate -> TVar (Map.Map ThreadId ()) -> Timeout -> IO ()
drain gate live grace = do
  atomically (writeTVar gate.open False)
  void (timeout (fromIntegral (timeoutMicros grace)) (atomically (readTVar gate.active >>= check . (== 0))))
  threadDelay (fromIntegral (min 250_000 (timeoutMicros grace)))
  readTVarIO live >>= mapM_ (forkIO . killThread) . Map.keys
  void (timeout 1_000_000 (atomically (readTVar live >>= check . Map.null)))

terminated :: IO ()
terminated = do
  signalled <- newEmptyMVar
  for_ [Signals.sigTERM, Signals.sigINT] \signal ->
    Signals.installHandler signal (Signals.CatchOnce (void (tryPutMVar signalled ()))) Nothing
  takeMVar signalled
