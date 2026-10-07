{-# LANGUAGE AllowAmbiguousTypes #-}

module Peculiar.Rpc.Rpc
  ( Method
  , methodPath
  , idempotent
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Kind (Type)
import Data.ProtoLens (Message, decodeMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Service.Types
  ( HasMethodImpl
  , MethodInput
  , MethodName
  , MethodOutput
  , Service (..)
  , ServiceName
  , ServicePackage
  )
import Data.Proxy (Proxy (..))
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Lens.Family2 ((^.))
import Proto.Google.Protobuf.Descriptor (MethodOptions'IdempotencyLevel (..), ServiceDescriptorProto)

type Method (service :: Type) (method :: Symbol) =
  ( Service service
  , HasMethodImpl service method
  , KnownSymbol (ServicePackage service)
  , KnownSymbol (ServiceName service)
  , KnownSymbol (MethodName service method)
  , Message (MethodInput service method)
  , Message (MethodOutput service method)
  )

methodName :: forall (service :: Type) method. (Method service method) => BS.ByteString
methodName = BC.pack (symbolVal (Proxy @(MethodName service method)))

idempotent :: forall (service :: Type) method. (Method service method) => Bool
idempotent = case decodeMessage @ServiceDescriptorProto (packedServiceDescriptor (Proxy @service)) of
  Left _ -> False
  Right descriptor ->
    or
      [ method ^. #options . #idempotencyLevel == MethodOptions'NO_SIDE_EFFECTS
      | method <- descriptor ^. #method
      , TE.encodeUtf8 (method ^. #name) == methodName @service @method
      ]

methodPath :: forall (service :: Type) method. (Method service method) => BS.ByteString
methodPath =
  "/"
    <> BC.pack (symbolVal (Proxy @(ServicePackage service)) <> "." <> symbolVal (Proxy @(ServiceName service)))
    <> "/"
    <> methodName @service @method
