module Peculiar.Rpc.Routing
  ( Routing (..)
  , Choices (..)
  , routing
  ) where

import Control.Exception (SomeException)
import Data.Map.Strict qualified as Map
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Deadline (Deadlines)
import Peculiar.Rpc.Details (errorDetails)
import Peculiar.Rpc.Handler (Served (..))
import Peculiar.Rpc.Intercept (Interceptor)
import Peculiar.Rpc.Json (Parsing (..), wellKnown)
import Peculiar.Rpc.Native (Natives (..), answer)
import Peculiar.Rpc.OpenApi (Info)
import Peculiar.Rpc.Routes (routesOf)
import Peculiar.Rpc.Service (Endpoint (..))
import Peculiar.Rpc.Translate (jsonDirect, jsonStreaming)
import Peculiar.Rpc.Web.Direct (Directs (..))

data Choices = Choices
  { interceptor :: Interceptor
  , report :: SomeException -> IO ()
  , reveal :: Bool
  , openApi :: Maybe Info
  }

data Routing = Routing
  { directs :: Directs
  , natives :: Natives
  }

routing :: Choices -> Deadlines -> [Endpoint] -> Routing
routing choices deadlines endpoints =
  Routing
    { directs =
        Directs
          { find = \case
              Proto -> (`Map.lookup` unary)
              Json -> (`Map.lookup` jsonUnary)
          , streaming = \codec path -> answer natives <$> Map.lookup path (case codec of Proto -> streams; Json -> jsonStreams)
          , deadlines
          , routes = routesOf choices.openApi registry (foldMap (.descriptors) endpoints) (foldMap (.services) endpoints)
          , report = choices.report
          , reveal = choices.reveal
          }
    , natives
    }
 where
  handlers = foldMap (.handlers) endpoints
  registry = wellKnown <> errorDetails <> foldMap (.registry) endpoints
  parsing = Parsing{registry, ignoreUnknown = False}
  translated = Map.fromList [(served.path, served.translation parsing) | served <- handlers]
  streams = Map.fromList [(served.path, served.streaming choices.interceptor) | served <- handlers]
  unary = Map.fromList [(served.path, entry choices.interceptor) | served@Served{direct = Just entry} <- handlers]
  jsonStreams = Map.intersectionWith jsonStreaming translated streams
  jsonUnary = Map.intersectionWith jsonDirect translated unary
  natives = Natives{find = (`Map.lookup` streams), report = choices.report, reveal = choices.reveal}
