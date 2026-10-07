module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value, decode, decodeStrict)
import Data.ByteString qualified as BS
import Hedgehog (Group (..), Property, checkParallel, evalEither, evalIO, property, withTests, (===))
import Peculiar.Rpc.OpenApi.Command (Command (..), Problem (..), documentOf, parseCommand)
import System.Exit (exitFailure)

main :: IO ()
main = do
  passed <-
    checkParallel $
      Group
        "OpenApi"
        [ ("the document for a descriptor set is golden", golden)
        , ("a service that is not in the set is refused", missingService)
        , ("a command needs a version", needsVersion)
        , ("a command reads its flags", readsFlags)
        ]
  unless passed exitFailure

shop :: Command
shop = Command{title = "Shop", version = "1.1.0", description = Just "Things for sale.", services = [], file = "test/fixtures/shop.binpb"}

golden :: Property
golden = withTests 1 $ property do
  bytes <- evalIO (BS.readFile "test/fixtures/shop.binpb")
  expected <- evalIO (BS.readFile "test/golden/shop.json")
  document <- evalEither (documentOf shop bytes)
  decode @Value document === decodeStrict expected

missingService :: Property
missingService = withTests 1 $ property do
  bytes <- evalIO (BS.readFile "test/fixtures/shop.binpb")
  documentOf shop{services = ["shop.v1.Warehouse"]} bytes === Left (NoServices "test/fixtures/shop.binpb")

needsVersion :: Property
needsVersion = withTests 1 $ property do
  case parseCommand ["--title", "Shop", "shop.binpb"] of
    Left (Usage _) -> pure ()
    other -> fail (show other)

readsFlags :: Property
readsFlags = withTests 1 $ property do
  parseCommand ["--title", "Shop", "--version", "1.1.0", "--service", "shop.v1.Shop", "shop.binpb"]
    === Right Command{title = "Shop", version = "1.1.0", description = Nothing, services = ["shop.v1.Shop"], file = "shop.binpb"}
