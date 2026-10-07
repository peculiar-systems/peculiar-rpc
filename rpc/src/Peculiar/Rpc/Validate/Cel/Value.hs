module Peculiar.Rpc.Validate.Cel.Value
  ( Value (..)
  , Key (..)
  , Message (..)
  , Slot (..)
  , CelType (..)
  , CelError (..)
  , CompileError (..)
  , keyValue
  , valueKey
  , typeOf
  , typeName
  , renderError
  , renderCompileError
  ) where

import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word64)
import Peculiar.Rpc.Validate.Schema (MessageType (..))

data Key
  = BoolK Bool
  | IntK Int64
  | UintK Word64
  | StringK T.Text
  deriving stock (Eq, Ord, Show)

data CelType
  = NullT
  | BoolT
  | IntT
  | UintT
  | DoubleT
  | StringT
  | BytesT
  | ListT
  | MapT
  | DurationT
  | TimestampT
  | TypeT
  | MessageT T.Text
  deriving stock (Eq, Ord, Show)

data Value
  = NullV
  | BoolV Bool
  | IntV Int64
  | UintV Word64
  | DoubleV Double
  | StringV T.Text
  | BytesV BS.ByteString
  | ListV [Value]
  | MapV (Map.Map Key Value)
  | MessageV Message
  | DurationV Int64
  | TimestampV Integer
  | TypeV CelType

data Slot
  = Single Value
  | Many [Value]
  | Pairs (Map.Map Key Value)

data Message = Message
  { described :: MessageType
  , slots :: Map.Map Int32 Slot
  }

data CelError
  = NoSuchOverload T.Text
  | NoSuchField T.Text
  | NoSuchKey T.Text
  | NoSuchAttribute T.Text
  | DivideByZero
  | ModulusByZero
  | IntOverflow
  | UintOverflow
  | IndexOutOfRange
  | TimestampOverflow
  | InvalidArgument T.Text
  | UnknownType T.Text
  deriving stock (Eq, Show)

renderError :: CelError -> T.Text
renderError = \case
  NoSuchOverload function -> "no such overload: " <> function
  NoSuchField field -> "no such field '" <> field <> "'"
  NoSuchKey key -> "no such key: " <> key
  NoSuchAttribute name -> "undeclared reference to '" <> name <> "'"
  DivideByZero -> "divide by zero"
  ModulusByZero -> "modulus by zero"
  IntOverflow -> "integer overflow"
  UintOverflow -> "unsigned integer overflow"
  IndexOutOfRange -> "index out of range"
  TimestampOverflow -> "timestamp overflow"
  InvalidArgument reason -> reason
  UnknownType name -> "unknown type: " <> name

keyValue :: Key -> Value
keyValue = \case
  BoolK flag -> BoolV flag
  IntK number -> IntV number
  UintK number -> UintV number
  StringK text -> StringV text

valueKey :: Value -> Maybe Key
valueKey = \case
  BoolV flag -> Just (BoolK flag)
  IntV number -> Just (IntK number)
  UintV number -> Just (UintK number)
  StringV text -> Just (StringK text)
  _ -> Nothing

typeOf :: Value -> CelType
typeOf = \case
  NullV -> NullT
  BoolV _ -> BoolT
  IntV _ -> IntT
  UintV _ -> UintT
  DoubleV _ -> DoubleT
  StringV _ -> StringT
  BytesV _ -> BytesT
  ListV _ -> ListT
  MapV _ -> MapT
  MessageV message -> MessageT message.described.fullName
  DurationV _ -> DurationT
  TimestampV _ -> TimestampT
  TypeV _ -> TypeT

typeName :: CelType -> T.Text
typeName = \case
  NullT -> "null_type"
  BoolT -> "bool"
  IntT -> "int"
  UintT -> "uint"
  DoubleT -> "double"
  StringT -> "string"
  BytesT -> "bytes"
  ListT -> "list"
  MapT -> "map"
  DurationT -> "google.protobuf.Duration"
  TimestampT -> "google.protobuf.Timestamp"
  TypeT -> "type"
  MessageT name -> name

data CompileError
  = Unparsable T.Text
  | Undeclared T.Text
  | Unconstructible T.Text
  | UndefinedField T.Text
  | Unselectable CelType
  | NoOverload T.Text T.Text
  | WrongOutput T.Text
  | BadPattern T.Text
  deriving stock (Eq, Show)

renderCompileError :: CompileError -> T.Text
renderCompileError = \case
  Unparsable reason -> reason
  Undeclared name -> "undeclared reference to '" <> name <> "'"
  Unconstructible name -> "message construction is not supported for " <> name
  UndefinedField name -> "undefined field '" <> name <> "'"
  Unselectable celType -> "type '" <> typeName celType <> "' does not support field selection"
  NoOverload function applied -> "found no matching overload for '" <> function <> "' applied to '" <> applied <> "'"
  WrongOutput rendered -> "expression outputs " <> rendered <> ", wanted either bool or string"
  BadPattern reason -> reason
