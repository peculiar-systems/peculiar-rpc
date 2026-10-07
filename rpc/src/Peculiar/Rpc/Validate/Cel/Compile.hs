module Peculiar.Rpc.Validate.Cel.Compile
  ( compile
  , expressionOf
  , specialize
  , Outcome (..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Peculiar.Rpc.Validate.Cel.Check (Type, check, output)
import Peculiar.Rpc.Validate.Cel.Eval (Activation (..), evaluate)
import Peculiar.Rpc.Validate.Cel.Library (compilePattern, knownFunction)
import Peculiar.Rpc.Validate.Cel.Parse (parseExpr)
import Peculiar.Rpc.Validate.Cel.Syntax
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Schema (EnumType (..), Schema (..))

data Outcome
  = Constant Value
  | Residual Expr

compile :: Schema -> Map.Map T.Text Type -> T.Text -> Either CompileError Expr
compile schema declared source = do
  (expression, outputType) <- expressionOf schema declared source
  expression <$ output outputType

expressionOf :: Schema -> Map.Map T.Text Type -> T.Text -> Either CompileError (Expr, Type)
expressionOf schema declared source = do
  parsed <- parseExpr source
  resolved <- resolve schema (Map.keysSet declared) parsed
  (,) resolved <$> check schema declared resolved

resolve :: Schema -> Set.Set T.Text -> Expr -> Either CompileError Expr
resolve schema = go
 where
  go bound = \case
    Literal value -> Right (Literal value)
    Ident name
      | name `Set.member` bound -> Right (Ident name)
      | otherwise -> maybe (Left (Undeclared name)) (Right . Literal) (named schema name)
    whole@(Select inner field) -> case qualified whole of
      Just (root, path)
        | root `Set.notMember` bound
        , Just value <- named schema path ->
            Right (Literal value)
      _ -> (`Select` field) <$> go bound inner
    Present inner field -> (`Present` field) <$> go bound inner
    Call name arguments
      | knownFunction name -> Call name <$> traverse (go bound) arguments
      | otherwise -> Left (Undeclared name)
    Member target name arguments -> case qualified target of
      Just (root, path)
        | root `Set.notMember` bound
        , knownFunction (path <> "." <> name) ->
            Call (path <> "." <> name) <$> traverse (go bound) arguments
      _
        | knownFunction name -> Member <$> go bound target <*> pure name <*> traverse (go bound) arguments
        | otherwise -> Left (Undeclared name)
    Index target position -> Index <$> go bound target <*> go bound position
    List elements -> List <$> traverse (go bound) elements
    Entries entries -> Entries <$> traverse (\(key, value) -> (,) <$> go bound key <*> go bound value) entries
    Construct name _ -> Left (Unconstructible name)
    Not operand -> Not <$> go bound operand
    Negate operand -> Negate <$> go bound operand
    And left right -> And <$> go bound left <*> go bound right
    Or left right -> Or <$> go bound left <*> go bound right
    Conditional condition whenTrue whenFalse -> Conditional <$> go bound condition <*> go bound whenTrue <*> go bound whenFalse
    Binary operator left right -> Binary operator <$> go bound left <*> go bound right
    Loop quantifier variable range body -> Loop quantifier variable <$> go bound range <*> go (Set.insert variable bound) body
    Transform variable range keep body ->
      Transform variable <$> go bound range <*> traverse (go (Set.insert variable bound)) keep <*> go (Set.insert variable bound) body
    Matching target compiled -> (`Matching` compiled) <$> go bound target

qualified :: Expr -> Maybe (T.Text, T.Text)
qualified = \case
  Ident name -> Just (T.dropWhile (== '.') name, T.dropWhile (== '.') name)
  Select inner field -> (\(root, path) -> (root, path <> "." <> field)) <$> qualified inner
  _ -> Nothing

named :: Schema -> T.Text -> Maybe Value
named schema written = case name of
  "int" -> Just (TypeV IntT)
  "uint" -> Just (TypeV UintT)
  "double" -> Just (TypeV DoubleT)
  "bool" -> Just (TypeV BoolT)
  "string" -> Just (TypeV StringT)
  "bytes" -> Just (TypeV BytesT)
  "list" -> Just (TypeV ListT)
  "map" -> Just (TypeV MapT)
  "null_type" -> Just (TypeV NullT)
  "type" -> Just (TypeV TypeT)
  "google.protobuf.Timestamp" -> Just (TypeV TimestampT)
  "google.protobuf.Duration" -> Just (TypeV DurationT)
  _
    | Map.member name schema.messages -> Just (TypeV (MessageT name))
    | otherwise ->
        let (owner, value) = T.breakOnEnd "." name
         in case Map.lookup (T.dropEnd 1 owner) schema.enums of
              Just enum | not (T.null owner) -> IntV . fromIntegral <$> lookup value enum.values
              _ -> Nothing
 where
  name = T.dropWhile (== '.') written

specialize :: Schema -> Map.Map T.Text Value -> Expr -> Either CompileError Outcome
specialize schema known expression = do
  folded <- fold expression
  pure case folded of
    Literal value -> Constant value
    other -> Residual other
 where
  activation = Activation{schema, variables = known}
  fold expr = do
    children <- foldChildren expr
    if Set.null (Set.difference (freeVariables children) (Map.keysSet known))
      then case evaluate activation children of
        Right value -> Right (Literal value)
        Left _ -> Right children
      else Right children
  foldChildren = \case
    Literal value -> Right (Literal value)
    Ident name -> Right (Ident name)
    Select inner field -> (`Select` field) <$> fold inner
    Present inner field -> (`Present` field) <$> fold inner
    Call "matches" [target, regexSource] -> do
      target' <- fold target
      regexSource' <- fold regexSource
      matching target' regexSource' (Call "matches" [target', regexSource'])
    Call name arguments -> Call name <$> traverse fold arguments
    Member target "matches" [regexSource] -> do
      target' <- fold target
      regexSource' <- fold regexSource
      matching target' regexSource' (Member target' "matches" [regexSource'])
    Member target name arguments -> Member <$> fold target <*> pure name <*> traverse fold arguments
    Index target position -> Index <$> fold target <*> fold position
    List elements -> List <$> traverse fold elements
    Entries entries -> Entries <$> traverse (\(key, value) -> (,) <$> fold key <*> fold value) entries
    Construct name fields -> Right (Construct name fields)
    Not operand -> Not <$> fold operand
    Negate operand -> Negate <$> fold operand
    And left right -> do
      left' <- fold left
      right' <- fold right
      Right case (left', right') of
        (Literal (BoolV False), _) -> Literal (BoolV False)
        (_, Literal (BoolV False)) -> Literal (BoolV False)
        _ -> And left' right'
    Or left right -> do
      left' <- fold left
      right' <- fold right
      Right case (left', right') of
        (Literal (BoolV True), _) -> Literal (BoolV True)
        (_, Literal (BoolV True)) -> Literal (BoolV True)
        _ -> Or left' right'
    Conditional condition whenTrue whenFalse ->
      fold condition >>= \case
        Literal (BoolV True) -> fold whenTrue
        Literal (BoolV False) -> fold whenFalse
        condition' -> Conditional condition' <$> fold whenTrue <*> fold whenFalse
    Binary operator left right -> Binary operator <$> fold left <*> fold right
    Loop quantifier variable range body -> Loop quantifier variable <$> fold range <*> pure body
    Transform variable range keep body -> Transform variable <$> fold range <*> pure keep <*> pure body
    Matching target compiled -> (`Matching` compiled) <$> fold target
  matching target regexSource fallback = case regexSource of
    Literal (StringV source) -> case compilePattern source of
      Right compiled -> Right (Matching target Compiled{source, regex = compiled})
      Left failure -> Left (BadPattern (renderError failure))
    _ -> Right fallback

freeVariables :: Expr -> Set.Set T.Text
freeVariables = \case
  Literal _ -> Set.empty
  Ident name -> Set.singleton name
  Select inner _ -> freeVariables inner
  Present inner _ -> freeVariables inner
  Call _ arguments -> foldMap freeVariables arguments
  Member target _ arguments -> freeVariables target <> foldMap freeVariables arguments
  Index target position -> freeVariables target <> freeVariables position
  List elements -> foldMap freeVariables elements
  Entries entries -> foldMap (\(key, value) -> freeVariables key <> freeVariables value) entries
  Construct _ fields -> foldMap (freeVariables . snd) fields
  Not operand -> freeVariables operand
  Negate operand -> freeVariables operand
  And left right -> freeVariables left <> freeVariables right
  Or left right -> freeVariables left <> freeVariables right
  Conditional condition whenTrue whenFalse -> freeVariables condition <> freeVariables whenTrue <> freeVariables whenFalse
  Binary _ left right -> freeVariables left <> freeVariables right
  Loop _ variable range body -> freeVariables range <> Set.delete variable (freeVariables body)
  Transform variable range keep body -> freeVariables range <> Set.delete variable (foldMap freeVariables keep <> freeVariables body)
  Matching target _ -> freeVariables target
