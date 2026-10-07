module Peculiar.Rpc.Violations
  ( invalidRequest
  , unvalidated
  ) where

import Data.ProtoLens (defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~))
import Peculiar.Rpc.Core.Code (Code (..))
import Peculiar.Rpc.Core.Status (Detail (..), Status (..))
import Peculiar.Rpc.Error (RpcError (..))
import Peculiar.Rpc.Validate (Failure (..), Violation (..))
import Proto.Google.Rpc.ErrorDetails (BadRequest)

invalidRequest :: [Violation] -> RpcError
invalidRequest found =
  RpcError
    { status =
        Status
          { code = InvalidArgument
          , message = "invalid request: " <> T.intercalate "; " [described violation | violation <- take 3 found] <> more
          , details = [Detail{typeUrl = "type.googleapis.com/google.rpc.BadRequest", value = encodeMessage badRequest}]
          }
    , metadata = mempty
    }
 where
  described violation
    | T.null violation.field = violation.description
    | otherwise = violation.field <> ": " <> violation.description
  more = case drop 3 found of
    [] -> ""
    rest -> " and " <> T.pack (show (length rest)) <> " more"
  badRequest =
    defMessage
      & #fieldViolations
        .~ [defMessage & #field .~ violation.field & #description .~ violation.description & #reason .~ violation.rule | violation <- found]
      :: BadRequest

unvalidated :: Failure -> RpcError
unvalidated failure =
  RpcError
    { status =
        Status
          { code = InvalidArgument
          , message = case failure of
              CompilationError reason -> "compilation error: " <> reason
              RuntimeError reason -> "runtime error: " <> reason
          , details = []
          }
    , metadata = mempty
    }
