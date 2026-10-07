module Peculiar.Rpc.Core.Code
  ( Code (..)
  , codeNumber
  , codeFromNumber
  , connectName
  , codeFromConnectName
  , connectHttpStatus
  , codeFromHttpStatus
  ) where

import Data.List (find)
import Data.Text qualified as T

data Code
  = Ok
  | Cancelled
  | Unknown
  | InvalidArgument
  | DeadlineExceeded
  | NotFound
  | AlreadyExists
  | PermissionDenied
  | ResourceExhausted
  | FailedPrecondition
  | Aborted
  | OutOfRange
  | Unimplemented
  | Internal
  | Unavailable
  | DataLoss
  | Unauthenticated
  deriving stock (Eq, Ord, Show, Enum, Bounded)

codeNumber :: Code -> Int
codeNumber = \case
  Ok -> 0
  Cancelled -> 1
  Unknown -> 2
  InvalidArgument -> 3
  DeadlineExceeded -> 4
  NotFound -> 5
  AlreadyExists -> 6
  PermissionDenied -> 7
  ResourceExhausted -> 8
  FailedPrecondition -> 9
  Aborted -> 10
  OutOfRange -> 11
  Unimplemented -> 12
  Internal -> 13
  Unavailable -> 14
  DataLoss -> 15
  Unauthenticated -> 16

codeFromNumber :: Int -> Maybe Code
codeFromNumber n = find ((== n) . codeNumber) [minBound ..]

connectName :: Code -> T.Text
connectName = \case
  Ok -> "ok"
  Cancelled -> "canceled"
  Unknown -> "unknown"
  InvalidArgument -> "invalid_argument"
  DeadlineExceeded -> "deadline_exceeded"
  NotFound -> "not_found"
  AlreadyExists -> "already_exists"
  PermissionDenied -> "permission_denied"
  ResourceExhausted -> "resource_exhausted"
  FailedPrecondition -> "failed_precondition"
  Aborted -> "aborted"
  OutOfRange -> "out_of_range"
  Unimplemented -> "unimplemented"
  Internal -> "internal"
  Unavailable -> "unavailable"
  DataLoss -> "data_loss"
  Unauthenticated -> "unauthenticated"

codeFromConnectName :: T.Text -> Maybe Code
codeFromConnectName name = find ((== name) . connectName) [minBound ..]

connectHttpStatus :: Code -> Int
connectHttpStatus = \case
  Ok -> 200
  Cancelled -> 499
  Unknown -> 500
  InvalidArgument -> 400
  DeadlineExceeded -> 504
  NotFound -> 404
  AlreadyExists -> 409
  PermissionDenied -> 403
  ResourceExhausted -> 429
  FailedPrecondition -> 400
  Aborted -> 409
  OutOfRange -> 400
  Unimplemented -> 501
  Internal -> 500
  Unavailable -> 503
  DataLoss -> 500
  Unauthenticated -> 401

codeFromHttpStatus :: Int -> Code
codeFromHttpStatus = \case
  400 -> Internal
  401 -> Unauthenticated
  403 -> PermissionDenied
  404 -> Unimplemented
  429 -> Unavailable
  502 -> Unavailable
  503 -> Unavailable
  504 -> Unavailable
  _ -> Unknown
