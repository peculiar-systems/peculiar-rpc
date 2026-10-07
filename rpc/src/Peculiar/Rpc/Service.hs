{-# LANGUAGE AllowAmbiguousTypes #-}

module Peculiar.Rpc.Service
  ( Implementation (..)
  , Client (..)
  , Endpoint (..)
  , endpoint
  , paths
  , idempotentPaths
  , BrowserReachable
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Kind (Constraint, Type)
import Data.ProtoLens (decodeMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Service.Types (MethodName, MethodStreamingType, Service (..), StreamingType (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Lens.Family2 ((^.))
import Peculiar.Rpc.Descriptor (Messages (..), SomeMessage (..), fileDescriptors, serviceName)
import Peculiar.Rpc.Handler (Served)
import Peculiar.Rpc.Json (Registry, register)
import Peculiar.Rpc.Remote (Client (..))
import Proto.Google.Protobuf.Descriptor (MethodOptions'IdempotencyLevel (..), ServiceDescriptorProto)

class Implementation impl where
  type Implementing impl :: Type
  handlers :: impl -> [Served]
  messages :: Proxy impl -> [SomeMessage]
  messages _ = []

data Endpoint = Endpoint
  { handlers :: [Served]
  , paths :: [BS.ByteString]
  , idempotent :: [BS.ByteString]
  , services :: [T.Text]
  , descriptors :: [BS.ByteString]
  , registry :: Registry
  }

endpoint
  :: forall impl
   . ( Implementation impl
     , Service (Implementing impl)
     , Named (Implementing impl) (ServiceMethods (Implementing impl))
     , Messages (Implementing impl) (ServiceMethods (Implementing impl))
     )
  => impl
  -> Endpoint
endpoint impl =
  Endpoint
    { handlers = handlers impl
    , paths = paths @(Implementing impl)
    , idempotent = idempotentPaths @(Implementing impl)
    , services = [serviceName @(Implementing impl)]
    , descriptors = fileDescriptors @(Implementing impl) (messages (Proxy @impl))
    , registry = foldMap (\(SomeMessage message) -> register message) (messagesOf @(Implementing impl) @(ServiceMethods (Implementing impl)) <> messages (Proxy @impl))
    }

prefix :: forall service. (KnownSymbol (ServicePackage service), KnownSymbol (ServiceName service)) => BS.ByteString
prefix = BC.pack ("/" <> symbolVal (Proxy @(ServicePackage service)) <> "." <> symbolVal (Proxy @(ServiceName service)) <> "/")

class Named service (methods :: [Symbol]) where
  names :: [BS.ByteString]

instance Named service '[] where
  names = []

instance (KnownSymbol (MethodName service method), Named service methods) => Named service (method ': methods) where
  names = BC.pack (symbolVal (Proxy @(MethodName service method))) : names @service @methods

paths
  :: forall service
   . (KnownSymbol (ServicePackage service), KnownSymbol (ServiceName service), Named service (ServiceMethods service))
  => [BS.ByteString]
paths = fmap (prefix @service <>) (names @service @(ServiceMethods service))

idempotentPaths
  :: forall service
   . (Service service)
  => [BS.ByteString]
idempotentPaths = case decodeMessage @ServiceDescriptorProto (packedServiceDescriptor (Proxy @service)) of
  Left _ -> []
  Right descriptor ->
    [ prefix @service <> TE.encodeUtf8 (method ^. #name)
    | method <- descriptor ^. #method
    , method ^. #options . #idempotencyLevel == MethodOptions'NO_SIDE_EFFECTS
    ]

type family BrowserReachable (service :: Type) :: Constraint where
  BrowserReachable service = Reachable service (ServiceMethods service)

type family Reachable (service :: Type) (methods :: [Symbol]) :: Constraint where
  Reachable _ '[] = ()
  Reachable service (method ': methods) = (ReachableKind service method (MethodStreamingType service method), Reachable service methods)

type family ReachableKind (service :: Type) (method :: Symbol) (kind :: StreamingType) :: Constraint where
  ReachableKind _ _ 'NonStreaming = ()
  ReachableKind _ _ 'ServerStreaming = ()
  ReachableKind service method kind =
    TypeError
      ( 'Text "The method "
          :<>: 'ShowType method
          :<>: 'Text " of "
          :<>: 'ShowType service
          :<>: 'Text " is "
          :<>: 'ShowType kind
          :$$: 'Text "and a browser can only make unary and server-streaming calls."
      )
