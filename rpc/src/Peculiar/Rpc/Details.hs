module Peculiar.Rpc.Details
  ( errorDetails
  ) where

import Data.Proxy (Proxy (..))
import Peculiar.Rpc.Json (Registry, register)
import Proto.Google.Rpc.ErrorDetails

errorDetails :: Registry
errorDetails =
  mconcat
    [ register (Proxy @BadRequest)
    , register (Proxy @DebugInfo)
    , register (Proxy @ErrorInfo)
    , register (Proxy @Help)
    , register (Proxy @LocalizedMessage)
    , register (Proxy @PreconditionFailure)
    , register (Proxy @QuotaFailure)
    , register (Proxy @RequestInfo)
    , register (Proxy @ResourceInfo)
    , register (Proxy @RetryInfo)
    ]
