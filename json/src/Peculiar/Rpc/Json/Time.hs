module Peculiar.Rpc.Json.Time
  ( showTimestamp
  , readTimestamp
  , showDuration
  , readDuration
  ) where

import Data.Char (isDigit)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Time.Calendar (addDays, diffDays, fromGregorian, fromGregorianValid, toGregorian)
import Peculiar.Rpc.Json.Error (JsonError, Problem (..), failure)
import Text.Read (readMaybe)

earliest, latest, longest :: Int64
earliest = -62135596800
latest = 253402300799
longest = 315576000000

showTimestamp :: Int64 -> Int32 -> Either JsonError T.Text
showTimestamp seconds nanos
  | seconds < earliest || seconds > latest || nanos < 0 || nanos > 999999999 = failure OutOfRange
  | otherwise =
      Right (T.pack (concat [padded 4 year, "-", padded 2 (toInteger month), "-", padded 2 (toInteger day), "T", padded 2 hours, ":", padded 2 minutes, ":", padded 2 secs, fraction nanos, "Z"]))
 where
  (days, clock) = toInteger seconds `divMod` 86400
  (year, month, day) = toGregorian (addDays days (fromGregorian 1970 1 1))
  (hours, rest) = clock `divMod` 3600
  (minutes, secs) = rest `divMod` 60

readTimestamp :: T.Text -> Either JsonError (Int64, Int32)
readTimestamp text = maybe malformed ranged (parse (T.unpack text))
 where
  malformed = failure (Malformed "not an RFC 3339 timestamp")
  ranged (seconds, nanos)
    | seconds < toInteger earliest || seconds > toInteger latest = failure OutOfRange
    | otherwise = Right (fromInteger seconds, nanos)
  parse input = do
    (year, '-' : afterYear) <- digitsOf 4 input
    (month, '-' : afterMonth) <- digitsOf 2 afterYear
    (day, 'T' : afterDay) <- digitsOf 2 afterMonth
    (hours, ':' : afterHours) <- digitsOf 2 afterDay
    (minutes, ':' : afterMinutes) <- digitsOf 2 afterHours
    (secs, afterSeconds) <- digitsOf 2 afterMinutes
    (nanos, zone) <- fractionOf afterSeconds
    offset <- offsetOf zone
    date <- fromGregorianValid year (fromInteger month) (fromInteger day)
    if hours > 23 || minutes > 59 || secs > 59
      then Nothing
      else Just (diffDays date (fromGregorian 1970 1 1) * 86400 + hours * 3600 + minutes * 60 + secs - offset, nanos)
  offsetOf = \case
    "Z" -> Just 0
    sign : rest | sign `elem` ['+', '-'] -> do
      (hours, ':' : afterHours) <- digitsOf 2 rest
      (minutes, "") <- digitsOf 2 afterHours
      let total = hours * 3600 + minutes * 60
      if hours > 23 || minutes > 59 then Nothing else Just (if sign == '+' then total else negate total)
    _ -> Nothing

showDuration :: Int64 -> Int32 -> Either JsonError T.Text
showDuration seconds nanos
  | abs seconds > longest || abs nanos > 999999999 || seconds > 0 && nanos < 0 || seconds < 0 && nanos > 0 = failure OutOfRange
  | otherwise = Right (T.pack (sign <> show (abs seconds) <> fraction (abs nanos) <> "s"))
 where
  sign = if seconds < 0 || nanos < 0 then "-" else ""

readDuration :: T.Text -> Either JsonError (Int64, Int32)
readDuration text = maybe (failure (Malformed "not a duration such as 1.5s")) ranged (parse (T.unpack text))
 where
  ranged (negative, seconds, nanos)
    | seconds > toInteger longest = failure OutOfRange
    | otherwise = Right (signed negative (fromInteger seconds), signed negative nanos)
  signed negative value = if negative then negate value else value
  parse = \case
    '-' : rest -> (\(seconds, nanos) -> (True, seconds, nanos)) <$> body rest
    rest -> (\(seconds, nanos) -> (False, seconds, nanos)) <$> body rest
  body input = do
    let (whole, rest) = span isDigit input
    seconds <- if null whole then Nothing else readMaybe whole
    (nanos, suffix) <- fractionOf rest
    if suffix == "s" then Just (seconds, nanos) else Nothing

digitsOf :: Int -> String -> Maybe (Integer, String)
digitsOf count input =
  let (taken, rest) = splitAt count input
   in if length taken == count && all isDigit taken then (,rest) <$> readMaybe taken else Nothing

fractionOf :: String -> Maybe (Int32, String)
fractionOf = \case
  '.' : rest ->
    let (digits, after) = span isDigit rest
     in if null digits || length digits > 9 then Nothing else (,after) <$> readMaybe (take 9 (digits <> repeat '0'))
  rest -> Just (0, rest)

fraction :: Int32 -> String
fraction nanos
  | nanos == 0 = ""
  | snd (nanos `quotRem` 1000000) == 0 = '.' : padded 3 (toInteger (fst (nanos `quotRem` 1000000)))
  | snd (nanos `quotRem` 1000) == 0 = '.' : padded 6 (toInteger (fst (nanos `quotRem` 1000)))
  | otherwise = '.' : padded 9 (toInteger nanos)

padded :: Int -> Integer -> String
padded width value = let shown = show value in replicate (width - length shown) '0' <> shown
