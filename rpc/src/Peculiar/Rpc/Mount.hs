module Peculiar.Rpc.Mount
  ( Calls (..)
  , defaultCalls
  , Server
  , withServer
  , application
  , middleware
  ) where

import Control.Exception (SomeException, displayException)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Network.HTTP.Semantics.Server qualified as Http
import Network.Wai qualified as Wai
import Peculiar.Rpc.Core.Cors (Cors, isPreflight)
import Peculiar.Rpc.Core.Wire (Incoming (..))
import Peculiar.Rpc.Deadline (withDeadlines)
import Peculiar.Rpc.Intercept (Interceptor)
import Peculiar.Rpc.Native (native)
import Peculiar.Rpc.OpenApi (Info)
import Peculiar.Rpc.Rest (Lookup (..), Routes (..), lookupBinding)
import Peculiar.Rpc.Routing (Choices (..), Routing (..), routing)
import Peculiar.Rpc.Service (Endpoint (..))
import Peculiar.Rpc.Wai (bridge, incomingOf)
import Peculiar.Rpc.Web (WebSettings (..), webWith)
import Peculiar.Rpc.Web.Direct (Directs (..))
import System.IO (hPutStrLn, stderr)
import System.TimeManager qualified as TimeManager

data Calls = Calls
  { interceptor :: Interceptor
  , revealErrors :: Bool
  , report :: SomeException -> IO ()
  , cors :: Maybe Cors
  , receiveLimit :: Maybe Int
  , headerLimit :: Maybe Int
  , getAllowed :: BS.ByteString -> Bool
  , openApi :: Maybe Info
  }

defaultCalls :: Calls
defaultCalls =
  Calls
    { interceptor = mempty
    , revealErrors = True
    , report = hPutStrLn stderr . displayException
    , cors = Nothing
    , receiveLimit = Nothing
    , headerLimit = Just 16384
    , getAllowed = const False
    , openApi = Nothing
    }

data Server = Server
  { stack :: Http.Server
  , owns :: Incoming -> Bool
  , manager :: TimeManager.Manager
  }

withServer :: Calls -> [Endpoint] -> (Server -> IO a) -> IO a
withServer calls endpoints use =
  withDeadlines \deadlines -> TimeManager.withManager 0 \manager -> do
    let routed = routing Choices{interceptor = calls.interceptor, report = calls.report, reveal = calls.revealErrors, openApi = calls.openApi} deadlines endpoints
        paths = Set.fromList (foldMap (.paths) endpoints)
        idempotent = Set.fromList (foldMap (.idempotent) endpoints)
        web =
          WebSettings
            { cors = calls.cors
            , receiveLimit = calls.receiveLimit
            , getAllowed = \path -> calls.getAllowed path || Set.member path idempotent
            , routes = Just (`Set.member` paths)
            , headerLimit = calls.headerLimit
            }
    use
      Server
        { stack = webWith routed.directs web (native routed.natives)
        , owns = owning (isJust calls.cors) routed.directs.routes paths
        , manager
        }

owning :: Bool -> Routes -> Set.Set BS.ByteString -> Incoming -> Bool
owning cors routes paths incoming =
  Set.member bare paths
    || incoming.method == "GET" && bare == "/openapi.json" && isJust routes.document
    || case lookupBinding routes.bindings incoming.method incoming.path of
      Routed{} -> True
      WrongVerb _ -> cors && isPreflight incoming.method incoming.headers
      NoRoute -> False
 where
  bare = BC.takeWhile (/= '?') incoming.path

application :: Server -> Wai.Application
application server = bridge server.manager server.stack

middleware :: Server -> Wai.Middleware
middleware server app request
  | server.owns (incomingOf request) = application server request
  | otherwise = app request
