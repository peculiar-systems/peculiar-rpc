{-# LANGUAGE AllowAmbiguousTypes #-}

module Peculiar.Rpc.Descriptor
  ( Messages (..)
  , SomeMessage (..)
  , serviceName
  , fileDescriptors
  ) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (FieldDescriptor (..), FieldTypeDescriptor (..), Message (..), decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Service.Types (MethodInput, MethodOutput, Service (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.TypeLits (Symbol, symbolVal)
import Lens.Family2 ((&), (.~), (^.))
import Proto.Google.Protobuf.Descriptor (FileDescriptorProto, ServiceDescriptorProto)

data SomeMessage = forall message. (Message message) => SomeMessage (Proxy message)

class Messages (service :: Type) (methods :: [Symbol]) where
  messagesOf :: [SomeMessage]

instance Messages service '[] where
  messagesOf = []

instance
  (Message (MethodInput service method), Message (MethodOutput service method), Messages service methods)
  => Messages service (method ': methods)
  where
  messagesOf =
    SomeMessage (Proxy @(MethodInput service method))
      : SomeMessage (Proxy @(MethodOutput service method))
      : messagesOf @service @methods

serviceName :: forall service. (Service service) => T.Text
serviceName = T.pack (symbolVal (Proxy @(ServicePackage service)) <> "." <> symbolVal (Proxy @(ServiceName service)))

fileDescriptors :: forall service. (Service service, Messages service (ServiceMethods service)) => [SomeMessage] -> [BS.ByteString]
fileDescriptors extra = fmap encodeMessage (declaring <> Map.elems pruned)
 where
  found = reachable (messagesOf @service @(ServiceMethods service) <> extra)
  known = Map.keysSet found
  pruned = fmap (prune known) found
  package = T.pack (symbolVal (Proxy @(ServicePackage service)))
  name = T.pack (symbolVal (Proxy @(ServiceName service)))
  declares file = file ^. #package == package && any ((== name) . (^. #name)) (file ^. #service)
  declaring
    | any declares (Map.elems found) = []
    | otherwise = foldMap (pure . synthesized) (decoded @ServiceDescriptorProto (packedServiceDescriptor (Proxy @service)))
  synthesized descriptor =
    defMessage
      & #name .~ T.replace "." "/" package <> "/" <> name <> ".reflected.proto"
      & #package .~ package
      & #syntax .~ "proto3"
      & #dependency .~ Map.keys found
      & #service .~ [descriptor]

decoded :: forall message. (Message message) => BS.ByteString -> Maybe message
decoded bytes = case decodeMessage bytes of
  Right message -> Just message
  Left _ -> Nothing

reachable :: [SomeMessage] -> Map.Map T.Text FileDescriptorProto
reachable = go Set.empty Map.empty
 where
  go _ files [] = files
  go seen files (SomeMessage (proxy :: Proxy message) : rest)
    | messageName proxy `Set.member` seen = go seen files rest
    | otherwise =
        let files' = case decoded @FileDescriptorProto (packedFileDescriptor proxy) of
              Just file -> Map.insert (file ^. #name) file files
              Nothing -> files
         in go (Set.insert (messageName proxy) seen) files' (fieldMessages @message <> rest)

fieldMessages :: forall message. (Message message) => [SomeMessage]
fieldMessages = foldl' collect [] (Map.elems (fieldsByTag @message))
 where
  collect acc (FieldDescriptor _ typed _) = case typed of
    MessageField _ -> SomeMessage (proxyOf typed) : acc
    ScalarField _ -> acc
  proxyOf :: FieldTypeDescriptor value -> Proxy value
  proxyOf _ = Proxy

prune :: Set.Set T.Text -> FileDescriptorProto -> FileDescriptorProto
prune known file
  | all (`Set.member` known) (file ^. #dependency) = file
  | otherwise =
      file
        & #dependency .~ filter (`Set.member` known) (file ^. #dependency)
        & #publicDependency .~ []
        & #weakDependency .~ []
