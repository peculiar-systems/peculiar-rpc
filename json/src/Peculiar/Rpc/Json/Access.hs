{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Json.Access
  ( Field (..)
  , Many (..)
  , scalarAt
  , repeatedAt
  , camelPath
  , snakePath
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, toLower, toUpper)
import Data.ProtoLens (ScalarField (..))
import Data.ProtoLens.Message (FieldAccessor (..))
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (..))
import Lens.Family2 (Lens')
import Peculiar.Rpc.Json.Plan (Kind (..), Plan, Slot (..), slotAt)

newtype Field msg v = Field (Lens' msg v)

newtype Many msg v = Many (Lens' msg [v])

scalarAt :: ScalarField v -> Int -> Plan msg -> Maybe (Field msg v)
scalarAt wanted number plan = case slotAt number plan of
  Just (Slot _ (Scalar found) (PlainField _ lens)) -> (\Refl -> Field lens) <$> same wanted found
  _ -> Nothing

repeatedAt :: ScalarField v -> Int -> Plan msg -> Maybe (Many msg v)
repeatedAt wanted number plan = case slotAt number plan of
  Just (Slot _ (Scalar found) (RepeatedField _ lens)) -> (\Refl -> Many lens) <$> same wanted found
  _ -> Nothing

same :: ScalarField a -> ScalarField b -> Maybe (a :~: b)
same = \cases
  Int32Field Int32Field -> Just Refl
  Int64Field Int64Field -> Just Refl
  UInt32Field UInt32Field -> Just Refl
  UInt64Field UInt64Field -> Just Refl
  FloatField FloatField -> Just Refl
  DoubleField DoubleField -> Just Refl
  BoolField BoolField -> Just Refl
  StringField StringField -> Just Refl
  BytesField BytesField -> Just Refl
  _ _ -> Nothing

camelPath :: T.Text -> Maybe T.Text
camelPath = fmap T.pack . go False . T.unpack
 where
  go underscore = \case
    [] -> if underscore then Nothing else Just []
    '_' : rest -> if underscore then Nothing else go True rest
    c : rest
      | underscore -> if isAsciiLower c then (toUpper c :) <$> go False rest else Nothing
      | isAsciiUpper c -> Nothing
      | otherwise -> (c :) <$> go False rest

snakePath :: T.Text -> Maybe T.Text
snakePath text
  | T.any (== '_') text = Nothing
  | otherwise = Just (T.concatMap (\c -> if isAsciiUpper c then T.pack ['_', toLower c] else T.singleton c) text)
