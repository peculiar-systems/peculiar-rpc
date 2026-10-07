module Peculiar.Rpc.Core.Timeout
  ( Timeout (..)
  , parseGrpcTimeout
  , renderGrpcTimeout
  , parseConnectTimeout
  , renderConnectTimeout
  , timeoutSeconds
  , timeoutMillis
  , timeoutMicros
  , microseconds
  , milliseconds
  , seconds
  ) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (isDigit, ord)
import Data.List (find)

newtype Timeout = Timeout {nanos :: Integer}
  deriving stock (Eq, Ord, Show)

units :: [(Char, Integer)]
units =
  [ ('n', 1)
  , ('u', 1_000)
  , ('m', 1_000_000)
  , ('S', 1_000_000_000)
  , ('M', 60_000_000_000)
  , ('H', 3_600_000_000_000)
  ]

digits :: Int -> BS.ByteString -> Maybe Integer
digits most raw
  | BS.null raw || BS.length raw > most || not (BC.all isDigit raw) = Nothing
  | otherwise = Just (BC.foldl' (\acc c -> acc * 10 + toInteger (ord c - ord '0')) 0 raw)

parseGrpcTimeout :: BS.ByteString -> Maybe Timeout
parseGrpcTimeout raw = do
  (amount, unit) <- BC.unsnoc raw
  scale <- lookup unit units
  n <- digits 8 amount
  pure (Timeout (n * scale))

renderGrpcTimeout :: Timeout -> BS.ByteString
renderGrpcTimeout (Timeout ns) = case find exact (reverse rendered) <|> find fits rendered of
  Just (unit, scale) -> BC.pack (show (ceilingDiv ns scale)) <> BC.singleton unit
  Nothing -> "99999999M"
 where
  rendered = filter ((/= 'H') . fst) units
  fits (_, scale) = ceilingDiv ns scale < 100_000_000
  exact (unit, scale) = ns `rem` scale == 0 && fits (unit, scale)

ceilingDiv :: Integer -> Integer -> Integer
ceilingDiv a b = (max 0 a + b - 1) `quot` b

parseConnectTimeout :: BS.ByteString -> Maybe Timeout
parseConnectTimeout raw = Timeout . (* 1_000_000) <$> digits 10 raw

renderConnectTimeout :: Timeout -> BS.ByteString
renderConnectTimeout = BC.pack . show . timeoutMillis

timeoutSeconds :: Timeout -> Integer
timeoutSeconds (Timeout ns) = ceilingDiv (max 0 ns) 1_000_000_000

timeoutMillis :: Timeout -> Integer
timeoutMillis (Timeout ns) = ceilingDiv (max 0 ns) 1_000_000

timeoutMicros :: Timeout -> Integer
timeoutMicros (Timeout ns) = ceilingDiv (max 0 ns) 1_000

microseconds :: Integer -> Timeout
microseconds = Timeout . (* 1_000)

milliseconds :: Integer -> Timeout
milliseconds = Timeout . (* 1_000_000)

seconds :: Integer -> Timeout
seconds = Timeout . (* 1_000_000_000)
