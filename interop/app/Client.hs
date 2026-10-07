module Main (main) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Interop.Client (Server (..), allCases, runCases)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= \case
    [host, port] | Just number <- readMaybe port -> run Rpc.Plaintext host number
    [host, port, authority] | Just number <- readMaybe port -> do
      pem <- BS.readFile authority
      run (Rpc.Secure Rpc.ClientTls{Rpc.trust = Rpc.TrustPem pem, Rpc.identity = Nothing}) host number
    _ -> die "usage: peculiar-rpc-interop-client HOST PORT [CA-CERTIFICATE]"
 where
  run security host number = do
    passed <- runCases Server{security, host = T.pack host, port = number} allCases
    unless passed exitFailure
