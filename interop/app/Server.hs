module Main (main) where

import Data.Text qualified as T
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Interop.Server (serveInterop)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [port] | Just number <- readMaybe port -> serveInterop Rpc.defaultSettings{Rpc.port = number}
    [port, certificate, key] | Just number <- readMaybe port -> do
      tls <- Rpc.tlsFromFiles certificate key >>= either (die . T.unpack) pure
      serveInterop Rpc.defaultSettings{Rpc.port = number, Rpc.tls = Just tls}
    _ -> die "usage: peculiar-rpc-interop-server PORT [CERTIFICATE KEY]"
