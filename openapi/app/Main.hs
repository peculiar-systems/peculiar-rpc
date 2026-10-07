module Main (main) where

import Data.Text qualified as T
import Peculiar.Rpc.OpenApi.Command qualified as Command
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main = getArgs >>= Command.main . fmap T.pack >>= exitWith
