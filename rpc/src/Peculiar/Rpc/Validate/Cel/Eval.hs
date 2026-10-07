module Peculiar.Rpc.Validate.Cel.Eval
  ( Activation (..)
  , evaluate
  ) where

import Control.Monad ((>=>))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Peculiar.Rpc.Validate.Cel.Library
import Peculiar.Rpc.Validate.Cel.Syntax
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Dynamic (equal)
import Peculiar.Rpc.Validate.Schema (Schema)
import Regex.RE2 qualified as RE2

data Activation = Activation
  { schema :: Schema
  , variables :: Map.Map T.Text Value
  }

evaluate :: Activation -> Expr -> Either CelError Value
evaluate activation = \case
  Literal value -> Right value
  Ident name -> maybe (Left (NoSuchAttribute name)) Right (Map.lookup name activation.variables)
  Select target name -> evaluate activation target >>= \value -> selected activation.schema value name
  Present target name -> BoolV <$> (evaluate activation target >>= (`present` name))
  Call name arguments -> traverse (evaluate activation) arguments >>= call activation.schema name Nothing
  Member target name arguments -> do
    receiver <- evaluate activation target
    values <- traverse (evaluate activation) arguments
    call activation.schema name (Just receiver) values
  Index target position -> do
    container <- evaluate activation target
    key <- evaluate activation position
    indexed container key
  List elements -> ListV <$> traverse (evaluate activation) elements
  Entries entries -> MapV <$> foldr insertEntry (Right Map.empty) entries
  Construct name _ -> Left (UnknownType name)
  Not operand ->
    evaluate activation operand >>= \case
      BoolV flag -> Right (BoolV (not flag))
      other -> Left (NoSuchOverload ("!_(" <> typeName (typeOf other) <> ")"))
  Negate operand -> evaluate activation operand >>= negated
  And left right -> logical False (evaluate activation left) (evaluate activation right)
  Or left right -> logical True (evaluate activation left) (evaluate activation right)
  Conditional condition whenTrue whenFalse ->
    evaluate activation condition >>= \case
      BoolV True -> evaluate activation whenTrue
      BoolV False -> evaluate activation whenFalse
      other -> Left (NoSuchOverload ("_?_:_(" <> typeName (typeOf other) <> ")"))
  Binary operator left right -> do
    a <- evaluate activation left
    b <- evaluate activation right
    case operator of
      Equals -> Right (BoolV (equal a b))
      NotEquals -> Right (BoolV (not (equal a b)))
      Less -> BoolV . (== LT) <$> ordering a b
      LessOrEqual -> BoolV . (/= GT) <$> ordering a b
      Greater -> BoolV . (== GT) <$> ordering a b
      GreaterOrEqual -> BoolV . (/= LT) <$> ordering a b
      Within -> BoolV <$> member a b
      _ -> arithmetic operator a b
  Loop quantifier variable range body -> evaluate activation range >>= iterated >>= looped activation quantifier variable body
  Transform variable range keep body -> evaluate activation range >>= iterated >>= transformed activation variable keep body
  Matching target compiled ->
    evaluate activation target >>= \case
      StringV text -> Right (BoolV (isJust (RE2.find compiled.regex (TE.encodeUtf8 text))))
      other -> Left (NoSuchOverload ("matches(" <> typeName (typeOf other) <> ", string)"))
 where
  insertEntry (keyExpr, valueExpr) rest = do
    key <- evaluate activation keyExpr
    value <- evaluate activation valueExpr
    pairs <- rest
    case valueKey key of
      Just mapKey
        | Map.member mapKey pairs -> Left (InvalidArgument "Failed with repeated key")
        | otherwise -> Right (Map.insert mapKey value pairs)
      Nothing -> Left (InvalidArgument ("unsupported key type: " <> typeName (typeOf key)))

truth :: Value -> Either CelError Bool
truth = \case
  BoolV flag -> Right flag
  other -> Left (NoSuchOverload ("expected bool, got " <> typeName (typeOf other)))

iterated :: Value -> Either CelError [Value]
iterated = \case
  ListV values -> Right values
  MapV pairs -> Right (fmap keyValue (Map.keys pairs))
  other -> Left (NoSuchOverload ("cannot iterate over " <> typeName (typeOf other)))

logical :: Bool -> Either CelError Value -> Either CelError Value -> Either CelError Value
logical absorbing left right = case left of
  Right (BoolV flag) | flag == absorbing -> Right (BoolV absorbing)
  _ -> case right of
    Right (BoolV flag) | flag == absorbing -> Right (BoolV absorbing)
    _ -> case (left, right) of
      (Right (BoolV _), Right (BoolV _)) -> Right (BoolV (not absorbing))
      (Left failure, _) -> Left failure
      (_, Left failure) -> Left failure
      (Right other, _) -> Left (NoSuchOverload ("logical operator on " <> typeName (typeOf other)))

quantified :: Bool -> [Value] -> (Value -> Either CelError Value) -> Either CelError Value
quantified absorbing items check = go (Right (BoolV (not absorbing))) items
 where
  go accumulated = \case
    [] -> accumulated
    item : rest -> case accumulated of
      Right (BoolV flag) | flag == absorbing -> accumulated
      _ -> go (logical absorbing accumulated (check item)) rest

bind :: Activation -> T.Text -> Value -> Activation
bind activation variable item = activation{variables = Map.insert variable item activation.variables}

looped :: Activation -> Quantifier -> T.Text -> Expr -> [Value] -> Either CelError Value
looped activation quantifier variable body items = case quantifier of
  Every -> quantified False items check
  Some -> quantified True items check
  ExactlyOne -> do
    outcomes <- traverse (check >=> truth) items
    Right (BoolV (length (filter id outcomes) == 1))
  Keeping -> ListV . fmap fst . filter snd <$> traverse (\item -> (,) item <$> (check >=> truth) item) items
 where
  check item = evaluate (bind activation variable item) body

transformed :: Activation -> T.Text -> Maybe Expr -> Expr -> [Value] -> Either CelError Value
transformed activation variable keep body items = do
  kept <- fmap fst . filter snd <$> traverse (\item -> (,) item <$> keeps item) items
  ListV <$> traverse (\item -> evaluate (bind activation variable item) body) kept
 where
  keeps item = maybe (Right True) (evaluate (bind activation variable item) >=> truth) keep
