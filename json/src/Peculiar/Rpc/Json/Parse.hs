module Peculiar.Rpc.Json.Parse
  ( parseJson
  , deepest
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Lit (..), Number (..), TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isSpace)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Peculiar.Rpc.Json.Error (JsonError, Problem (..), failure)

deepest :: Int
deepest = 100

parseJson :: BS.ByteString -> Either JsonError Value
parseJson bytes = do
  (value, rest) <- tokens 0 (bsToTokens bytes)
  if BC.all isSpace rest then Right value else failure (NotJson "content after the value")

tokens :: Int -> Tokens k String -> Either JsonError (Value, k)
tokens depth = \case
  TkLit LitNull rest -> Right (Null, rest)
  TkLit LitTrue rest -> Right (Bool True, rest)
  TkLit LitFalse rest -> Right (Bool False, rest)
  TkText text rest -> Right (String text, rest)
  TkNumber number rest -> Right (Number (scientific number), rest)
  TkArrayOpen items
    | depth >= deepest -> tooDeep
    | otherwise -> array (depth + 1) [] items
  TkRecordOpen pairs
    | depth >= deepest -> tooDeep
    | otherwise -> record (depth + 1) KeyMap.empty pairs
  TkErr reason -> failure (NotJson (T.pack reason))
 where
  scientific = \case
    NumInteger whole -> fromInteger whole
    NumDecimal value -> value
    NumScientific value -> value
  tooDeep = failure (NotJson ("nested more than " <> T.pack (show deepest) <> " levels deep"))

array :: Int -> [Value] -> TkArray k String -> Either JsonError (Value, k)
array depth seen = \case
  TkItem item -> tokens depth item >>= \(value, next) -> array depth (value : seen) next
  TkArrayEnd rest -> Right (Array (Vector.fromList (reverse seen)), rest)
  TkArrayErr reason -> failure (NotJson (T.pack reason))

record :: Int -> KeyMap.KeyMap Value -> TkRecord k String -> Either JsonError (Value, k)
record depth seen = \case
  TkPair key item
    | KeyMap.member key seen -> failure (NotJson ("the key " <> Key.toText key <> " appears twice"))
    | otherwise -> tokens depth item >>= \(value, next) -> record depth (KeyMap.insert key value seen) next
  TkRecordEnd rest -> Right (Object seen, rest)
  TkRecordErr reason -> failure (NotJson (T.pack reason))
