{-# LANGUAGE TemplateHaskell #-}

module Main (main, EchoClient (..)) where

import Control.Exception (try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens (defMessage)
import Hedgehog (Group (..), Property, checkParallel, property, withTests, (===))
import Peculiar.Rpc.Client qualified as Rpc
import Proto.Peculiar.Test.V1.Echo
import System.Exit (exitFailure)

Rpc.deriveClient ''Echo

main :: IO ()
main = do
  passed <- checkParallel (Group "Client" [("a derived client reports a closed port as UNAVAILABLE", withTests 1 closedPort)])
  unless passed exitFailure

closedPort :: Property
closedPort = property do
  outcome <- liftIO $ try @Rpc.RpcError $ Rpc.withNativeClient Rpc.Plaintext "127.0.0.1" 1 \transport ->
    (Rpc.client Rpc.defaultOptions transport :: EchoClient).say defMessage
  either (\failure -> failure.status.code) (const Rpc.Ok) outcome === Rpc.Unavailable
