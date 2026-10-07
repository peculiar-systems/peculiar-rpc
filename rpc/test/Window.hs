module Main (main) where

import Control.Monad (unless)
import Hedgehog (checkSequential)
import System.Exit (exitFailure)
import Test.Peculiar.Rpc.Harness (withEcho)
import Test.Peculiar.Rpc.Window qualified as Window

main :: IO ()
main = withEcho \port _ -> checkSequential (Window.tests port) >>= flip unless exitFailure
