module Peculiar.Rpc.Core.Codec
  ( Codec (..)
  , codecName
  , codecNamed
  ) where

import Data.ByteString qualified as BS

data Codec = Proto | Json
  deriving stock (Eq, Show, Enum, Bounded)

codecName :: Codec -> BS.ByteString
codecName = \case
  Proto -> "proto"
  Json -> "json"

codecNamed :: BS.ByteString -> Maybe Codec
codecNamed = \case
  "" -> Just Proto
  "proto" -> Just Proto
  "json" -> Just Json
  _ -> Nothing
