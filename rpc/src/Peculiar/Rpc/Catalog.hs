module Peculiar.Rpc.Catalog
  ( Catalog (..)
  , Described (..)
  , catalogOf
  , fileCatalog
  , extension
  , fieldNamed
  , commentOf
  ) where

import Data.ByteString qualified as BS
import Data.Either (rights)
import Data.Int (Int32)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (Message, decodeMessage)
import Data.ProtoLens.Encoding.Wire (Tag (..), TaggedValue (..), WireValue (..))
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Message (FieldSet)
import Data.Text qualified as T
import Lens.Family2 ((^.))
import Proto.Google.Protobuf.Descriptor (DescriptorProto, EnumDescriptorProto, FieldDescriptorProto, FileDescriptorProto, ServiceDescriptorProto)

data Described = Described
  { file :: FileDescriptorProto
  , path :: [Int32]
  }

data Catalog = Catalog
  { messages :: Map.Map T.Text (DescriptorProto, Described)
  , enums :: Map.Map T.Text (EnumDescriptorProto, Described)
  , services :: Map.Map T.Text (ServiceDescriptorProto, Described)
  }

catalogOf :: [BS.ByteString] -> Catalog
catalogOf = foldMap fileCatalog . rights . fmap decodeMessage

instance Semigroup Catalog where
  left <> right = Catalog (left.messages <> right.messages) (left.enums <> right.enums) (left.services <> right.services)

instance Monoid Catalog where
  mempty = Catalog Map.empty Map.empty Map.empty

fileCatalog :: FileDescriptorProto -> Catalog
fileCatalog file =
  mconcat
    ( [messageCatalog file scope [4, index] message | (index, message) <- zip [0 ..] (file ^. #messageType)]
        <> [ mempty{enums = Map.singleton (scope <> "." <> enum ^. #name) (enum, Described file [5, index])}
           | (index, enum) <- zip [0 ..] (file ^. #enumType)
           ]
        <> [ mempty{services = Map.singleton (scope <> "." <> service ^. #name) (service, Described file [6, index])}
           | (index, service) <- zip [0 ..] (file ^. #service)
           ]
    )
 where
  scope = if T.null (file ^. #package) then "" else "." <> file ^. #package

messageCatalog :: FileDescriptorProto -> T.Text -> [Int32] -> DescriptorProto -> Catalog
messageCatalog file scope path message =
  mconcat
    ( mempty{messages = Map.singleton named (message, Described file path)}
        : [messageCatalog file named (path <> [3, index]) nested | (index, nested) <- zip [0 ..] (message ^. #nestedType)]
          <> [ mempty{enums = Map.singleton (named <> "." <> enum ^. #name) (enum, Described file (path <> [4, index]))}
             | (index, enum) <- zip [0 ..] (message ^. #enumType)
             ]
    )
 where
  named = scope <> "." <> message ^. #name

extension :: (Message ext) => Int -> FieldSet -> Maybe ext
extension number fields = case [bytes | TaggedValue (Tag tag) (Lengthy bytes) <- fields, tag == number] of
  [] -> Nothing
  parts -> either (const Nothing) Just (decodeMessage (BS.concat parts))

fieldNamed :: T.Text -> DescriptorProto -> Maybe FieldDescriptorProto
fieldNamed name message = find (\field -> field ^. #name == name || field ^. #jsonName == name) (message ^. #field)

commentOf :: Described -> Maybe T.Text
commentOf described = case [location | location <- described.file ^. #sourceCodeInfo . #location, location ^. #path == described.path] of
  location : _ ->
    let leading = T.strip (location ^. #leadingComments)
        trailing = T.strip (location ^. #trailingComments)
     in case filter (not . T.null) [leading, trailing] of
          [] -> Nothing
          parts -> Just (T.intercalate "\n\n" parts)
  [] -> Nothing
