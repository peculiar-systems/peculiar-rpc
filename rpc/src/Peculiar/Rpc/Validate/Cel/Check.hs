module Peculiar.Rpc.Validate.Cel.Check
  ( Type (..)
  , check
  , output
  , fieldType
  , elementType
  ) where

import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Peculiar.Rpc.Validate.Cel.Syntax
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Schema

data Type
  = Dyn
  | Primitive CelType
  | ListOf Type
  | MapOf Type Type
  deriving stock (Eq, Show)

output :: Type -> Either CompileError ()
output = \case
  Primitive BoolT -> Right ()
  Primitive StringT -> Right ()
  Dyn -> Right ()
  other -> Left (WrongOutput (rendered other))

rendered :: Type -> T.Text
rendered = \case
  Dyn -> "dyn"
  Primitive celType -> typeName celType
  ListOf element -> "list(" <> rendered element <> ")"
  MapOf key value -> "map(" <> rendered key <> ", " <> rendered value <> ")"

elementType :: Schema -> Field -> Type
elementType schema field = case field.kind of
  DoubleKind -> Primitive DoubleT
  FloatKind -> Primitive DoubleT
  Int64Kind -> Primitive IntT
  Uint64Kind -> Primitive UintT
  Int32Kind -> Primitive IntT
  Fixed64Kind -> Primitive UintT
  Fixed32Kind -> Primitive UintT
  BoolKind -> Primitive BoolT
  StringKind -> Primitive StringT
  BytesKind -> Primitive BytesT
  Uint32Kind -> Primitive UintT
  Sfixed32Kind -> Primitive IntT
  Sfixed64Kind -> Primitive IntT
  Sint32Kind -> Primitive IntT
  Sint64Kind -> Primitive IntT
  EnumKind
    | field.typeName == "google.protobuf.NullValue" -> Primitive NullT
    | otherwise -> Primitive IntT
  _ -> case field.typeName of
    "google.protobuf.Timestamp" -> Primitive TimestampT
    "google.protobuf.Duration" -> Primitive DurationT
    "google.protobuf.Struct" -> MapOf (Primitive StringT) Dyn
    "google.protobuf.ListValue" -> ListOf Dyn
    name
      | name `elem` dynamic -> Dyn
      | Map.member name schema.messages -> Primitive (MessageT name)
      | otherwise -> Dyn
 where
  dynamic =
    [ "google.protobuf.Any"
    , "google.protobuf.Value"
    , "google.protobuf.BoolValue"
    , "google.protobuf.BytesValue"
    , "google.protobuf.DoubleValue"
    , "google.protobuf.FloatValue"
    , "google.protobuf.Int32Value"
    , "google.protobuf.Int64Value"
    , "google.protobuf.StringValue"
    , "google.protobuf.UInt32Value"
    , "google.protobuf.UInt64Value"
    ]

fieldType :: Schema -> Field -> Type
fieldType schema field = case mapEntryOf schema field of
  Just entry -> case (fieldNamed "key" entry, fieldNamed "value" entry) of
    (Just key, Just value) -> MapOf (elementType schema key) (elementType schema value)
    _ -> MapOf Dyn Dyn
  Nothing
    | field.repeated -> ListOf (elementType schema field)
    | otherwise -> elementType schema field

concrete :: Type -> Bool
concrete = \case
  Dyn -> False
  Primitive _ -> True
  ListOf element -> concrete element
  MapOf key value -> concrete key && concrete value

check :: Schema -> Map.Map T.Text Type -> Expr -> Either CompileError Type
check schema = go
 where
  go environment = \case
    Literal value -> Right (literalType value)
    Ident name -> Right (Map.findWithDefault Dyn name environment)
    Select target name -> go environment target >>= selection name
    Present target name -> Primitive BoolT <$ (go environment target >>= selection name)
    Call name arguments -> traverse (go environment) arguments >>= overload name Nothing
    Member target name arguments -> do
      receiver <- go environment target
      values <- traverse (go environment) arguments
      overload name (Just receiver) values
    Index target position -> do
      container <- go environment target
      _ <- go environment position
      Right case container of
        ListOf element -> element
        MapOf _ value -> value
        _ -> Dyn
    List elements -> ListOf . joined <$> traverse (go environment) elements
    Entries entries -> do
      keys <- traverse (go environment . fst) entries
      values <- traverse (go environment . snd) entries
      Right (MapOf (joined keys) (joined values))
    Construct name _ -> Left (Unconstructible name)
    Not operand -> go environment operand >>= expectBool "!_" >> Right (Primitive BoolT)
    Negate operand ->
      go environment operand >>= \case
        operandType@(Primitive kind)
          | kind `elem` [IntT, DoubleT, DurationT] -> Right operandType
          | otherwise -> Left (NoOverload "-_" ("(" <> typeName kind <> ")"))
        other -> Right other
    And left right -> logical environment "_&&_" left right
    Or left right -> logical environment "_||_" left right
    Conditional condition whenTrue whenFalse -> do
      go environment condition >>= expectBool "_?_:_"
      a <- go environment whenTrue
      b <- go environment whenFalse
      Right (if a == b then a else Dyn)
    Binary operator left right -> do
      a <- go environment left
      b <- go environment right
      binary operator a b
    Loop quantifier variable range body -> do
      rangeType <- go environment range
      let bound = Map.insert variable (iteration rangeType) environment
      bodyType <- go bound body
      case quantifier of
        Keeping -> rangeType <$ expectBool "filter" bodyType
        _ -> Primitive BoolT <$ expectBool "comprehension" bodyType
    Transform variable range keep body -> do
      rangeType <- go environment range
      let bound = Map.insert variable (iteration rangeType) environment
      traverse_ (go bound >=> expectBool "map") keep
      ListOf <$> go bound body
    Matching target _ -> Primitive BoolT <$ (go environment target >>= expectOne "matches" [StringT])
  logical environment name left right = do
    go environment left >>= expectBool name
    go environment right >>= expectBool name
    Right (Primitive BoolT)
  iteration = \case
    ListOf element -> element
    MapOf key _ -> key
    _ -> Dyn
  selection name = \case
    Primitive (MessageT message) -> case Map.lookup message schema.messages >>= fieldNamed name of
      Just field -> Right (fieldType schema field)
      Nothing -> Left (UndefinedField name)
    MapOf _ value -> Right value
    Primitive other -> Left (Unselectable other)
    _ -> Right Dyn

literalType :: Value -> Type
literalType = \case
  ListV _ -> ListOf Dyn
  MapV _ -> MapOf Dyn Dyn
  other -> Primitive (typeOf other)

joined :: [Type] -> Type
joined = \case
  first : rest | all (== first) rest -> first
  _ -> Dyn

expectBool :: T.Text -> Type -> Either CompileError ()
expectBool name = expectOne name [BoolT]

expectOne :: T.Text -> [CelType] -> Type -> Either CompileError ()
expectOne name allowed = \case
  Primitive kind | kind `notElem` allowed -> Left (NoOverload name ("(" <> typeName kind <> ")"))
  ListOf _ | ListT `notElem` allowed -> Left (NoOverload name "(list)")
  MapOf _ _ | MapT `notElem` allowed -> Left (NoOverload name "(map)")
  _ -> Right ()

numeric :: [CelType]
numeric = [IntT, UintT, DoubleT]

binary :: Operator -> Type -> Type -> Either CompileError Type
binary operator a b = case operator of
  Equals -> Right (Primitive BoolT)
  NotEquals -> Right (Primitive BoolT)
  Within -> Right (Primitive BoolT)
  _
    | not (concrete a && concrete b) -> Right (if operator `elem` [Less, LessOrEqual, Greater, GreaterOrEqual] then Primitive BoolT else if a == b then a else Dyn)
    | operator `elem` [Less, LessOrEqual, Greater, GreaterOrEqual] -> case (a, b) of
        (Primitive x, Primitive y)
          | x `elem` numeric && y `elem` numeric -> Right (Primitive BoolT)
          | x == y && x `elem` [BoolT, StringT, BytesT, TimestampT, DurationT] -> Right (Primitive BoolT)
        _ -> mismatch
    | otherwise -> case (operator, a, b) of
        (Plus, Primitive x, Primitive y)
          | x == y && x `elem` [IntT, UintT, DoubleT, StringT, BytesT, DurationT] -> Right a
          | x == TimestampT && y == DurationT -> Right a
          | x == DurationT && y == TimestampT -> Right b
        (Plus, ListOf _, ListOf _) -> Right (if a == b then a else ListOf Dyn)
        (Minus, Primitive x, Primitive y)
          | x == y && x `elem` [IntT, UintT, DoubleT, DurationT] -> Right a
          | x == TimestampT && y == TimestampT -> Right (Primitive DurationT)
          | x == TimestampT && y == DurationT -> Right a
        (Times, Primitive x, Primitive y) | x == y && x `elem` numeric -> Right a
        (Divide, Primitive x, Primitive y) | x == y && x `elem` numeric -> Right a
        (Remainder, Primitive x, Primitive y) | x == y && x `elem` [IntT, UintT] -> Right a
        _ -> mismatch
 where
  mismatch = Left (NoOverload "operator" ("(" <> rendered a <> ", " <> rendered b <> ")"))

overload :: T.Text -> Maybe Type -> [Type] -> Either CompileError Type
overload name receiver arguments
  | not (all concrete (maybe arguments (: arguments) receiver)) = Right (resultOf name)
  | maybe (globalAccepts name arguments) (\target -> memberAccepts name target arguments) receiver = Right (resultOf name)
  | otherwise =
      Left (NoOverload name (maybe "" (\target -> rendered target <> ".") receiver <> "(" <> T.intercalate ", " (fmap rendered arguments) <> ")"))

sized :: Type -> Bool
sized = \case
  Primitive kind -> kind `elem` [StringT, BytesT]
  ListOf _ -> True
  MapOf _ _ -> True
  Dyn -> True

converts :: [CelType] -> [Type] -> Bool
converts allowed = \case
  [Primitive kind] -> kind `elem` allowed
  _ -> False

globalAccepts :: T.Text -> [Type] -> Bool
globalAccepts name arguments = case (name, arguments) of
  ("size", [value]) -> sized value
  ("matches", [Primitive StringT, Primitive StringT]) -> True
  ("int", _) -> converts [IntT, UintT, DoubleT, StringT, TimestampT] arguments
  ("uint", _) -> converts [IntT, UintT, DoubleT, StringT] arguments
  ("double", _) -> converts [IntT, UintT, DoubleT, StringT] arguments
  ("string", _) -> converts [StringT, BoolT, IntT, UintT, DoubleT, BytesT, TimestampT, DurationT] arguments
  ("bytes", _) -> converts [StringT, BytesT] arguments
  ("bool", _) -> converts [BoolT, StringT] arguments
  ("dyn", [_]) -> True
  ("type", [_]) -> True
  ("duration", _) -> converts [StringT, DurationT] arguments
  ("timestamp", _) -> converts [StringT, IntT, TimestampT] arguments
  ("getField", [_, Primitive StringT]) -> True
  ("strings.quote", [Primitive StringT]) -> True
  _ -> False

memberAccepts :: T.Text -> Type -> [Type] -> Bool
memberAccepts name receiver arguments = case (name, receiver, arguments) of
  ("size", value, []) -> sized value
  (_, Primitive StringT, [Primitive StringT]) | name `elem` ["contains", "startsWith", "endsWith", "matches"] -> True
  (_, Primitive BytesT, [Primitive BytesT]) | name `elem` ["contains", "startsWith", "endsWith"] -> True
  ("isNan", Primitive DoubleT, []) -> True
  ("isInf", Primitive DoubleT, rest) -> rest `elem` [[], [Primitive IntT]]
  (_, Primitive StringT, [])
    | name `elem` ["isHostname", "isEmail", "isIp", "isIpPrefix", "isUri", "isUriRef", "lowerAscii", "upperAscii", "trim", "reverse"] -> True
  ("isIp", Primitive StringT, [Primitive IntT]) -> True
  ("isIpPrefix", Primitive StringT, rest) -> rest `elem` [[Primitive IntT], [Primitive BoolT], [Primitive IntT, Primitive BoolT]]
  ("isHostAndPort", Primitive StringT, [Primitive BoolT]) -> True
  ("unique", ListOf element, []) -> element `elem` fmap Primitive [BoolT, IntT, UintT, DoubleT, StringT, BytesT]
  ("charAt", Primitive StringT, [Primitive IntT]) -> True
  ("indexOf", Primitive StringT, Primitive StringT : rest) -> rest `elem` [[], [Primitive IntT]]
  ("lastIndexOf", Primitive StringT, Primitive StringT : rest) -> rest `elem` [[], [Primitive IntT]]
  ("replace", Primitive StringT, Primitive StringT : Primitive StringT : rest) -> rest `elem` [[], [Primitive IntT]]
  ("split", Primitive StringT, Primitive StringT : rest) -> rest `elem` [[], [Primitive IntT]]
  ("substring", Primitive StringT, Primitive IntT : rest) -> rest `elem` [[], [Primitive IntT]]
  ("join", ListOf (Primitive StringT), rest) -> rest `elem` [[], [Primitive StringT]]
  ("format", Primitive StringT, [ListOf _]) -> True
  (_, Primitive TimestampT, rest) | name `elem` timeGetters -> rest `elem` [[], [Primitive StringT]]
  (_, Primitive DurationT, []) -> name `elem` ["getHours", "getMinutes", "getSeconds", "getMilliseconds"]
  _ -> False

timeGetters :: [T.Text]
timeGetters = ["getFullYear", "getMonth", "getDayOfYear", "getDayOfMonth", "getDate", "getDayOfWeek", "getHours", "getMinutes", "getSeconds", "getMilliseconds"]

resultOf :: T.Text -> Type
resultOf name = case name of
  "size" -> Primitive IntT
  "int" -> Primitive IntT
  "uint" -> Primitive UintT
  "double" -> Primitive DoubleT
  "string" -> Primitive StringT
  "bytes" -> Primitive BytesT
  "bool" -> Primitive BoolT
  "type" -> Primitive TypeT
  "duration" -> Primitive DurationT
  "timestamp" -> Primitive TimestampT
  "split" -> ListOf (Primitive StringT)
  "charAt" -> Primitive StringT
  "indexOf" -> Primitive IntT
  "lastIndexOf" -> Primitive IntT
  "lowerAscii" -> Primitive StringT
  "upperAscii" -> Primitive StringT
  "replace" -> Primitive StringT
  "substring" -> Primitive StringT
  "trim" -> Primitive StringT
  "join" -> Primitive StringT
  "reverse" -> Primitive StringT
  "format" -> Primitive StringT
  "strings.quote" -> Primitive StringT
  _
    | name `elem` ["contains", "startsWith", "endsWith", "matches", "isNan", "isInf", "isHostname", "isEmail", "isIp", "isIpPrefix", "isUri", "isUriRef", "isHostAndPort", "unique"] -> Primitive BoolT
    | name `elem` timeGetters -> Primitive IntT
    | otherwise -> Dyn
