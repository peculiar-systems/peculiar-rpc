module Peculiar.Rpc.Reflection.Registry
  ( Registry
  , registry
  , registryWith
  , answer
  ) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as T
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Service (Endpoint (..))
import Proto.Google.Protobuf.Descriptor (DescriptorProto, FileDescriptorProto, FileDescriptorSet)
import Proto.Grpc.Reflection.V1.Reflection qualified as V1

data Registry = Registry
  { files :: Map.Map T.Text (BS.ByteString, FileDescriptorProto)
  , symbols :: Map.Map T.Text T.Text
  , services :: [T.Text]
  }

registry :: [Endpoint] -> Registry
registry = registryWith []

registryWith :: [BS.ByteString] -> [Endpoint] -> Registry
registryWith sets endpoints =
  Registry
    { files = Map.fromList [(file ^. #name, (bytes, file)) | (bytes, file) <- parsed]
    , symbols = Map.fromList [(symbol, file ^. #name) | (_, file) <- parsed, symbol <- symbolsOf file]
    , services = Set.toList (Set.fromList (foldMap (.services) endpoints))
    }
 where
  given = [(encodeMessage file, file) | set <- mapMaybe (decoded @FileDescriptorSet) sets, file <- set ^. #file]
  derived = mapMaybe (\bytes -> (bytes,) <$> decoded bytes) (foldMap (.descriptors) endpoints)
  parsed = Map.elems (Map.fromList [(file ^. #name, entry) | entry@(_, file) <- derived <> given])

decoded :: (Message message) => BS.ByteString -> Maybe message
decoded bytes = case decodeMessage bytes of
  Right message -> Just message
  Left _ -> Nothing

symbolsOf :: FileDescriptorProto -> [T.Text]
symbolsOf file =
  foldMap (messageSymbols prefix) (file ^. #messageType)
    <> fmap (qualified prefix . (^. #name)) (file ^. #enumType)
    <> foldMap serviceSymbols (file ^. #service)
 where
  prefix = file ^. #package
  serviceSymbols service =
    qualified prefix (service ^. #name)
      : fmap (qualified (qualified prefix (service ^. #name)) . (^. #name)) (service ^. #method)

messageSymbols :: T.Text -> DescriptorProto -> [T.Text]
messageSymbols prefix message =
  here
    : foldMap (messageSymbols here) (message ^. #nestedType)
      <> fmap (qualified here . (^. #name)) (message ^. #enumType)
      <> fmap (qualified here . (^. #name)) (message ^. #field)
 where
  here = qualified prefix (message ^. #name)

qualified :: T.Text -> T.Text -> T.Text
qualified prefix name
  | T.null prefix = name
  | otherwise = prefix <> "." <> name

answer :: Registry -> V1.ServerReflectionRequest -> V1.ServerReflectionResponse
answer known request =
  base & case request ^. #maybe'messageRequest of
    Just (V1.ServerReflectionRequest'ListServices _) ->
      #listServicesResponse .~ (defMessage & #service .~ fmap (\name -> defMessage & #name .~ name) known.services)
    Just (V1.ServerReflectionRequest'FileByFilename name) -> withFile name
    Just (V1.ServerReflectionRequest'FileContainingSymbol symbol) ->
      maybe (failure ("unknown symbol " <> symbol)) withFile (Map.lookup (T.dropWhile (== '.') symbol) known.symbols)
    Just (V1.ServerReflectionRequest'FileContainingExtension _) -> failure "this server declares no extensions"
    Just (V1.ServerReflectionRequest'AllExtensionNumbersOfType name) ->
      #allExtensionNumbersResponse .~ (defMessage & #baseTypeName .~ name)
    Nothing -> failure "the request asked for nothing"
 where
  base = defMessage & #validHost .~ request ^. #host & #originalRequest .~ request
  withFile name = case closure known name of
    [] -> failure ("unknown file " <> name)
    found -> #fileDescriptorResponse .~ (defMessage & #fileDescriptorProto .~ found)
  failure message = #errorResponse .~ (defMessage & #errorCode .~ 5 & #errorMessage .~ message)

closure :: Registry -> T.Text -> [BS.ByteString]
closure known root = go Set.empty [root]
 where
  go _ [] = []
  go seen (name : rest)
    | name `Set.member` seen = go seen rest
    | otherwise = case Map.lookup name known.files of
        Nothing -> go (Set.insert name seen) rest
        Just (bytes, file) -> bytes : go (Set.insert name seen) (rest <> file ^. #dependency)
