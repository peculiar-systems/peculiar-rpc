module Peculiar.Rpc.Conformance.Report
  ( canceledError
  , errorOf
  , headersOf
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (defMessage, maybeToEnum)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Lens.Family2 ((&), (.~))
import Peculiar.Rpc.Core.Base64 qualified as Base64
import Peculiar.Rpc.Core.Code (codeNumber)
import Peculiar.Rpc.Core.Status (Detail (..), Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Metadata (Entry (..), Metadata (..), binary)
import Proto.Connectrpc.Conformance.V1.Config (Code (..))
import Proto.Connectrpc.Conformance.V1.Service (Error, Header)
import Proto.Google.Protobuf.Any (Any)

canceledError :: Error
canceledError = defMessage & #code .~ CODE_CANCELED & #message .~ "the call was canceled"

errorOf :: RpcError -> Error
errorOf failure =
  defMessage
    & #code .~ fromMaybe CODE_UNKNOWN (maybeToEnum (codeNumber failure.status.code))
    & #maybe'message .~ (if T.null failure.status.message then Nothing else Just failure.status.message)
    & #details .~ fmap anyOf failure.status.details
 where
  anyOf detail = defMessage & #typeUrl .~ detail.typeUrl & #value .~ detail.value :: Any

headersOf :: Metadata -> [Header]
headersOf (Metadata entries) =
  [ defMessage & #name .~ name & #value .~ values
  | (name, values) <- Map.toList (Map.fromListWith (flip (<>)) (fmap pair entries))
  ]
 where
  pair entry = (TE.decodeUtf8Lenient entry.name, [TE.decodeUtf8Lenient (if binary entry then Base64.encodeUnpadded entry.value else entry.value)])
