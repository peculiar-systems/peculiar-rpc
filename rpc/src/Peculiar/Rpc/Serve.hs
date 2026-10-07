module Peculiar.Rpc.Serve
  ( Settings (..)
  , defaultSettings
  , serve
  , serveEndpoints
  , terminated
  ) where

import Control.Concurrent.Async (cancel, race_, wait, withAsync)
import Control.Concurrent.STM (atomically, check, newTQueueIO, newTVarIO, readTQueue, readTVar, writeTQueue)
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Peculiar.Rpc.Core.Timeout (Timeout, seconds)
import Peculiar.Rpc.Drain (admitted, closingOf, drain, newGate, terminated, tracked)
import Peculiar.Rpc.Listen (Dispatch (..), Http2Settings, Listening (..), defaultHttp2, dispatch, made, withListening)
import Peculiar.Rpc.Liveness (Keepalive (..), Policy (..))
import Peculiar.Rpc.Mount (Calls (..), application, defaultCalls, middleware, withServer)
import Peculiar.Rpc.Service (Endpoint)
import Peculiar.Rpc.Tls (Tls)
import System.TimeManager qualified as TimeManager

data Settings = Settings
  { host :: T.Text
  , port :: Int
  , http2 :: Http2Settings
  , onListening :: Int -> IO ()
  , tls :: Maybe Tls
  , stopWhen :: IO ()
  , grace :: Timeout
  , unixSocket :: Maybe FilePath
  , keepalive :: Maybe Keepalive
  , idleTimeout :: Maybe Timeout
  , maxAge :: Maybe Timeout
  , maxConnections :: Maybe Int
  , calls :: Calls
  , beside :: Maybe Wai.Application
  }

defaultSettings :: Settings
defaultSettings =
  Settings
    { host = "127.0.0.1"
    , port = 50051
    , http2 = defaultHttp2
    , onListening = const (pure ())
    , tls = Nothing
    , stopWhen = terminated
    , grace = seconds 15
    , unixSocket = Nothing
    , keepalive = Just Keepalive{every = seconds 7200, within = seconds 20}
    , idleTimeout = Nothing
    , maxAge = Nothing
    , maxConnections = Nothing
    , calls = defaultCalls
    , beside = Nothing
    }

serveEndpoints :: Settings -> [Endpoint] -> IO ()
serveEndpoints settings endpoints =
  withServer settings.calls endpoints \server ->
    serve settings (maybe (application server) (middleware server) settings.beside)

serve :: Settings -> Wai.Application -> IO ()
serve settings app = do
  gate <- newGate
  live <- newTVarIO Map.empty
  withListening (maybe (OnTcp settings.host settings.port) OnUnix settings.unixSocket) \listening -> do
    bound <- case settings.unixSocket of
      Nothing -> fromIntegral <$> Socket.socketPort listening
      Just _ -> pure 0
    TimeManager.withManager 0 \manager -> do
      http1 <- newTQueueIO
      let warp =
            Warp.setOnException (\_ failure -> when (Warp.defaultShouldDisplayException failure) (settings.calls.report failure)) $
              Warp.setHTTP2Disabled Warp.defaultSettings
          accepted = atomically (readTQueue http1)
          admitting = admitted gate app
          policy =
            Policy
              { keepalive = settings.keepalive
              , idle = settings.idleTimeout
              , maxAge = settings.maxAge
              , closing = closingOf gate
              , announce = True
              , grace = Just settings.grace
              }
          room = for_ settings.maxConnections \limit -> atomically (readTVar live >>= check . (< limit) . Map.size)
      Warp.withII warp \internal -> do
        let accepting = forever do
              room
              (connection, peer) <- Socket.accept listening
              let http1Accept prepared transport = atomically (writeTQueue http1 (prepared, transport, peer))
              tracked live (dispatch Dispatch{tls = settings.tls, report = settings.calls.report, http2 = settings.http2, manager, warp, internal, http1Accept, app = admitting, policy, headerLimit = settings.calls.headerLimit} connection peer)
        withAsync (Warp.runSettingsConnectionMakerSecure warp (made <$> accepted) admitting) \_ ->
          withAsync accepting \acceptor -> do
            settings.onListening bound
            race_ settings.stopWhen (wait acceptor)
            cancel acceptor
            drain gate live settings.grace
