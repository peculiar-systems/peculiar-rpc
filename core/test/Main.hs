module Main (main) where

import Control.Monad (unless)
import Hedgehog (checkParallel)
import System.Exit (exitFailure)
import Test.Peculiar.Rpc.Core.Base64 qualified as Base64
import Test.Peculiar.Rpc.Core.Connect qualified as Connect
import Test.Peculiar.Rpc.Core.Frame qualified as Frame
import Test.Peculiar.Rpc.Core.Response qualified as Response
import Test.Peculiar.Rpc.Core.Status qualified as Status
import Test.Peculiar.Rpc.Core.Timeout qualified as Timeout
import Test.Peculiar.Rpc.Core.Wire qualified as Wire

main :: IO ()
main = do
  results <-
    traverse
      checkParallel
      [ Base64.tests
      , Connect.tests
      , Frame.tests
      , Response.tests
      , Status.tests
      , Timeout.tests
      , Wire.tests
      ]
  unless (and results) exitFailure
