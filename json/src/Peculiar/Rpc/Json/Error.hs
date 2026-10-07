module Peculiar.Rpc.Json.Error
  ( JsonError (..)
  , Problem (..)
  , failure
  , within
  , render
  ) where

import Data.Text qualified as T

data Problem
  = Expected T.Text
  | OutOfRange
  | UnknownField T.Text
  | UnknownEnum T.Text
  | UnknownType T.Text
  | Malformed T.Text
  | SeveralOfOneof T.Text
  | NamedTwice T.Text
  | NotJson T.Text
  deriving stock (Eq, Show)

data JsonError = JsonError
  { path :: [T.Text]
  , problem :: Problem
  }
  deriving stock (Eq, Show)

failure :: Problem -> Either JsonError a
failure problem = Left JsonError{path = [], problem}

within :: T.Text -> Either JsonError a -> Either JsonError a
within step = \case
  Left JsonError{path, problem} -> Left JsonError{path = step : path, problem}
  right -> right

render :: JsonError -> T.Text
render JsonError{path, problem} = case path of
  [] -> described
  _ -> T.intercalate "." path <> ": " <> described
 where
  described = case problem of
    Expected what -> "expected " <> what
    OutOfRange -> "out of range"
    UnknownField name -> "no field named " <> name
    UnknownEnum name -> "no enum value named " <> name
    UnknownType name -> "no message type " <> name <> " is known"
    Malformed why -> why
    SeveralOfOneof name -> "more than one field of the oneof " <> name <> " is set"
    NamedTwice name -> "the field " <> name <> " is given twice"
    NotJson why -> "not JSON: " <> why
