module Peculiar.Rpc.Web.Table
  ( table
  ) where

import Control.Applicative ((<|>))
import Data.Array (accumArray)
import Data.ByteString qualified as BS
import Network.HTTP.Semantics (TokenHeaderTable, isMaxTokenIx, maxTokenIx, minTokenIx, toToken, tokenIx)

table :: [(BS.ByteString, BS.ByteString)] -> TokenHeaderTable
table headers = (tokens, values)
 where
  tokens = [(toToken name, value) | (name, value) <- headers]
  values =
    accumArray
      (\old value -> old <|> Just value)
      Nothing
      (minTokenIx, maxTokenIx)
      [(tokenIx token, value) | (token, value) <- tokens, not (isMaxTokenIx (tokenIx token))]
