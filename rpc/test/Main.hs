module Main (main) where

import Control.Monad (unless)
import Hedgehog (checkSequential)
import System.Exit (exitFailure)
import Test.Peculiar.Rpc.Cel qualified as Cel
import Test.Peculiar.Rpc.Client qualified as Client
import Test.Peculiar.Rpc.Harness (withEcho)
import Test.Peculiar.Rpc.Health qualified as Health
import Test.Peculiar.Rpc.Library (library, withLibrary)
import Test.Peculiar.Rpc.Lifecycle qualified as Lifecycle
import Test.Peculiar.Rpc.Middleware qualified as Middleware
import Test.Peculiar.Rpc.Mount qualified as Mount
import Test.Peculiar.Rpc.Native qualified as Native
import Test.Peculiar.Rpc.Peer qualified as Peer
import Test.Peculiar.Rpc.Reflection qualified as Reflection
import Test.Peculiar.Rpc.Rest qualified as Rest
import Test.Peculiar.Rpc.Shutdown qualified as Shutdown
import Test.Peculiar.Rpc.Validation qualified as Validation
import Test.Peculiar.Rpc.Web qualified as Web

main :: IO ()
main = withEcho \port statuses -> do
  results <- traverse checkSequential [Cel.tests, Web.tests port, Native.tests port, Peer.tests port, Client.tests port, Health.tests port statuses, Reflection.tests port, Shutdown.tests, Lifecycle.tests port, Mount.tests, Middleware.tests]
  served <- withLibrary library \libraryPort -> traverse checkSequential [Rest.tests libraryPort, Validation.tests libraryPort]
  unless (and results && and served) exitFailure
