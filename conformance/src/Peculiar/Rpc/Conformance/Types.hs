module Peculiar.Rpc.Conformance.Types
  ( conformanceTypes
  , codecOf
  ) where

import Data.ProtoLens.Labels ()
import Data.Proxy (Proxy (..))
import Lens.Family2 ((^.))
import Peculiar.Rpc (Codec (..), Registry, register, wellKnown)
import Proto.Connectrpc.Conformance.V1.ClientCompat (ClientCompatRequest)
import Proto.Connectrpc.Conformance.V1.Config qualified as Config
import Proto.Connectrpc.Conformance.V1.Service

conformanceTypes :: Registry
conformanceTypes =
  mconcat
    [ wellKnown
    , register (Proxy @UnaryRequest)
    , register (Proxy @IdempotentUnaryRequest)
    , register (Proxy @ServerStreamRequest)
    , register (Proxy @ClientStreamRequest)
    , register (Proxy @BidiStreamRequest)
    , register (Proxy @UnimplementedRequest)
    ]

codecOf :: ClientCompatRequest -> Codec
codecOf request = if request ^. #codec == Config.CODEC_JSON then Json else Proto
