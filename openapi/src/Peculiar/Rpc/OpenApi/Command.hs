module Peculiar.Rpc.OpenApi.Command
  ( Command (..)
  , Problem (..)
  , parseCommand
  , documentOf
  , render
  , usage
  , main
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (encode)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.ProtoLens (decodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lens.Family2 ((^.))
import Peculiar.Rpc.Catalog (Catalog (..), fileCatalog)
import Peculiar.Rpc.OpenApi (Info (..), openApi)
import Peculiar.Rpc.Rest (bindingsOf)
import Proto.Google.Protobuf.Descriptor (FileDescriptorSet)
import System.Exit (ExitCode (..))
import System.IO (stderr)

data Command = Command
  { title :: T.Text
  , version :: T.Text
  , description :: Maybe T.Text
  , services :: [T.Text]
  , file :: FilePath
  }
  deriving stock (Eq, Show)

data Problem
  = Usage T.Text
  | Unreadable FilePath
  | Undecodable FilePath T.Text
  | NoServices FilePath
  deriving stock (Eq, Show)

data Flag
  = Title T.Text
  | Version T.Text
  | Description T.Text
  | Service T.Text
  | File FilePath

parseCommand :: [T.Text] -> Either Problem Command
parseCommand arguments = do
  given <- flags arguments
  file <- case [path | File path <- given] of
    [path] -> Right path
    [] -> Left (Usage "name the descriptor set to read")
    _ -> Left (Usage "name only one descriptor set")
  version <- maybe (Left (Usage "give the API's version with --version")) Right (lastOr Nothing [Just text | Version text <- given])
  Right
    Command
      { title = lastOr "API" [text | Title text <- given]
      , version
      , description = lastOr Nothing [Just text | Description text <- given]
      , services = [name | Service name <- given]
      , file
      }
 where
  lastOr = foldl' (\_ value -> value)

flags :: [T.Text] -> Either Problem [Flag]
flags = \case
  [] -> Right []
  "--title" : text : rest -> (Title text :) <$> flags rest
  "--version" : text : rest -> (Version text :) <$> flags rest
  "--description" : text : rest -> (Description text :) <$> flags rest
  "--service" : name : rest -> (Service name :) <$> flags rest
  other : rest
    | "--" `T.isPrefixOf` other -> Left (Usage ("unexpected " <> other))
    | otherwise -> (File (T.unpack other) :) <$> flags rest

documentOf :: Command -> BS.ByteString -> Either Problem LBS.ByteString
documentOf command bytes = do
  set <- first (Undecodable command.file . T.pack) (decodeMessage @FileDescriptorSet bytes)
  let catalog = foldMap fileCatalog (set ^. #file)
      named = if null command.services then [T.drop 1 name | name <- Map.keys catalog.services] else command.services
      present = filter (\name -> Map.member ("." <> name) catalog.services) named
      info = Info{title = command.title, version = command.version, description = command.description}
  if null present
    then Left (NoServices command.file)
    else Right (encode (openApi info catalog (bindingsOf catalog present)))

render :: Problem -> T.Text
render = \case
  Usage reason -> reason <> "\n\n" <> usage
  Unreadable path -> "cannot read " <> T.pack path
  Undecodable path reason -> T.pack path <> " is not a FileDescriptorSet: " <> reason
  NoServices path -> T.pack path <> " defines none of the services asked for"

usage :: T.Text
usage =
  T.unlines
    [ "peculiar-rpc-openapi --version VERSION [--title TEXT] [--description TEXT] [--service NAME]... FILE"
    , ""
    , "Prints the OpenAPI 3.1 document a server serves at /openapi.json, from a"
    , "FileDescriptorSet with source info, so a site can be built without a server:"
    , "  protoc --include_imports --include_source_info --descriptor_set_out=FILE ..."
    , "  buf build --as-file-descriptor-set -o FILE"
    ]

main :: [T.Text] -> IO ExitCode
main arguments = case parseCommand arguments of
  Left problem -> failed problem
  Right command ->
    try @IOException (BS.readFile command.file) >>= \case
      Left _ -> failed (Unreadable command.file)
      Right bytes -> either failed (\document -> ExitSuccess <$ LBS.putStr document) (documentOf command bytes)
 where
  failed problem = ExitFailure 1 <$ TIO.hPutStrLn stderr (render problem)
