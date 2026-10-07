module Peculiar.Rpc.Validate.Cel.Time
  ( Component (..)
  , parseTimestamp
  , renderTimestamp
  , parseDuration
  , renderDuration
  , timestampComponent
  , durationComponent
  , minimumSeconds
  , maximumSeconds
  , shortestDecimal
  ) where

import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (TimeZone (..), fromGregorianValid, toGregorian)
import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian)
import Data.Time.Calendar.OrdinalDate (mondayStartWeek, toOrdinalDate)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (tzByName)
import Numeric (floatToDigits)
import Peculiar.Rpc.Validate.Arithmetic (digitValue, floorQuotRem, quotient, remainder)

data Component
  = FullYear
  | Month
  | DayOfYear
  | DayOfMonth
  | Date
  | DayOfWeek
  | Hours
  | Minutes
  | Seconds
  | Milliseconds
  deriving stock (Eq, Show)

minimumSeconds :: Integer
minimumSeconds = -62135596800

maximumSeconds :: Integer
maximumSeconds = 253402300799

epoch :: Day
epoch = fromGregorian 1970 1 1

parseTimestamp :: T.Text -> Maybe Integer
parseTimestamp text = do
  (year, rest0) <- fixedDigits 4 text
  rest1 <- T.stripPrefix "-" rest0
  (month, rest2) <- fixedDigits 2 rest1
  rest3 <- T.stripPrefix "-" rest2
  (day, rest4) <- fixedDigits 2 rest3
  rest5 <- T.stripPrefix "T" rest4 `orElse` T.stripPrefix "t" rest4
  (hour, rest6) <- fixedDigits 2 rest5
  rest7 <- T.stripPrefix ":" rest6
  (minute, rest8) <- fixedDigits 2 rest7
  rest9 <- T.stripPrefix ":" rest8
  (second, rest10) <- fixedDigits 2 rest9
  let (fraction, rest11) = case T.stripPrefix "." rest10 of
        Just afterDot -> T.span isDigit afterDot
        Nothing -> ("", rest10)
  offset <- zone rest11
  date <- fromGregorianValid year (fromInteger month) (fromInteger day)
  if hour > 23 || minute > 59 || second > 59 || T.null fraction && "." `T.isPrefixOf` rest10
    then Nothing
    else
      let seconds = diffDays date epoch * 86400 + hour * 3600 + minute * 60 + second - offset
          nanos = digitsValue (T.take 9 (fraction <> "000000000"))
       in Just (seconds * 1000000000 + nanos)
 where
  orElse (Just value) _ = Just value
  orElse Nothing other = other
  zone remaining
    | remaining == "Z" || remaining == "z" = Just 0
    | otherwise = do
        (sign, afterSign) <- T.uncons remaining
        (hours, afterHours) <- fixedDigits 2 afterSign
        afterColon <- T.stripPrefix ":" afterHours
        (minutes, end) <- fixedDigits 2 afterColon
        if not (T.null end) || hours > 23 || minutes > 59
          then Nothing
          else case sign of
            '+' -> Just ((hours * 60 + minutes) * 60)
            '-' -> Just (negate ((hours * 60 + minutes) * 60))
            _ -> Nothing

fixedDigits :: Int -> T.Text -> Maybe (Integer, T.Text)
fixedDigits count text =
  let (digits, rest) = T.splitAt count text
   in if T.length digits == count && T.all isDigit digits then Just (digitsValue digits, rest) else Nothing

digitsValue :: T.Text -> Integer
digitsValue = T.foldl' (\acc digit -> acc * 10 + toInteger (digitValue digit)) 0

renderTimestamp :: Integer -> T.Text
renderTimestamp total =
  let (seconds, nanos) = floorQuotRem total 1000000000
      (days, daySeconds) = floorQuotRem seconds 86400
      (year, month, day) = toGregorian (addDays days epoch)
      (hour, minuteSeconds) = floorQuotRem daySeconds 3600
      (minute, second) = floorQuotRem minuteSeconds 60
      fraction = T.dropWhileEnd (== '0') (padded 9 nanos)
   in T.concat
        [ padded 4 year
        , "-"
        , padded 2 (toInteger month)
        , "-"
        , padded 2 (toInteger day)
        , "T"
        , padded 2 hour
        , ":"
        , padded 2 minute
        , ":"
        , padded 2 second
        , if T.null fraction then "" else "." <> fraction
        , "Z"
        ]
 where
  padded width value = T.justifyRight width '0' (T.pack (show value))

parseDuration :: T.Text -> Maybe Int64
parseDuration text = do
  let (negative, unsigned) = case T.uncons text of
        Just ('-', rest) -> (True, rest)
        Just ('+', rest) -> (False, rest)
        _ -> (False, text)
  total <-
    if unsigned == "0"
      then Just 0
      else
        if T.null unsigned
          then Nothing
          else terms unsigned 0
  let signed = if negative then negate total else total
  if signed < toInteger (minBound :: Int64) || signed > toInteger (maxBound :: Int64)
    then Nothing
    else Just (fromInteger signed)
 where
  terms remaining acc
    | T.null remaining = Just acc
    | otherwise = do
        let (whole, afterWhole) = T.span isDigit remaining
            (fraction, afterFraction) = case T.stripPrefix "." afterWhole of
              Just rest -> T.span isDigit rest
              Nothing -> ("", afterWhole)
        if T.null whole && T.null fraction
          then Nothing
          else do
            let (unit, rest) = T.span (\c -> not (isDigit c) && c /= '.') afterFraction
            scale <- unitScale unit
            let value = toRational (digitsValue whole) + toRational (digitsValue fraction) / 10 ^ T.length fraction
                nanos = truncate (value * toRational scale) :: Integer
                next = acc + nanos
            if next > toInteger (maxBound :: Int64) + 1 then Nothing else terms rest next
  unitScale = \case
    "ns" -> Just (1 :: Integer)
    "us" -> Just 1000
    "\x00b5s" -> Just 1000
    "\x03bcs" -> Just 1000
    "ms" -> Just 1000000
    "s" -> Just 1000000000
    "m" -> Just 60000000000
    "h" -> Just 3600000000000
    _ -> Nothing

renderDuration :: Int64 -> T.Text
renderDuration nanos = shortestDecimal (fromIntegral nanos / 1000000000) <> "s"

shortestDecimal :: Double -> T.Text
shortestDecimal value
  | isNaN value = "NaN"
  | isInfinite value = if value > 0 then "+Inf" else "-Inf"
  | value == 0 = if isNegativeZero value then "-0" else "0"
  | otherwise =
      let (digits, point) = floatToDigits 10 (abs value)
          rendered
            | point <= 0 = "0." <> T.replicate (negate point) "0" <> digitText digits
            | point >= length digits = digitText digits <> T.replicate (point - length digits) "0"
            | otherwise = let (whole, fraction) = splitAt point digits in digitText whole <> "." <> digitText fraction
       in (if value < 0 then "-" else "") <> rendered
 where
  digitText = T.pack . concatMap show

timestampComponent :: Component -> Maybe T.Text -> Integer -> Either T.Text Int64
timestampComponent component zone total = do
  offset <- maybe (Right 0) (zoneOffset total) zone
  let (seconds, nanos) = floorQuotRem total 1000000000
      local = seconds + offset
      (days, daySeconds) = floorQuotRem local 86400
      date = addDays days epoch
      (year, month, day) = toGregorian date
      weekday = snd (mondayStartWeek date)
  pure $ fromInteger case component of
    FullYear -> year
    Month -> toInteger month - 1
    DayOfYear -> toInteger (snd (toOrdinalDate date)) - 1
    DayOfMonth -> toInteger day - 1
    Date -> toInteger day
    DayOfWeek -> remainder (toInteger weekday) 7
    Hours -> quotient daySeconds 3600
    Minutes -> quotient (remainder daySeconds 3600) 60
    Seconds -> remainder daySeconds 60
    Milliseconds -> quotient nanos 1000000

zoneOffset :: Integer -> T.Text -> Either T.Text Integer
zoneOffset total zone = case T.breakOn ":" zone of
  (_, "") -> case tzByName (TE.encodeUtf8 zone) of
    Just tz ->
      let (seconds, _) = floorQuotRem total 1000000000
          (days, daySeconds) = floorQuotRem seconds 86400
          instant = UTCTime (addDays days epoch) (fromInteger daySeconds)
       in Right (toInteger (timeZoneMinutes (timeZoneForUTCTime tz instant)) * 60)
    Nothing
      | zone == "UTC" || T.null zone -> Right 0
      | otherwise -> Left ("unknown time zone " <> zone)
  (hours, colonMinutes) -> do
    hour <- signedNumber hours
    minute <- signedNumber (T.drop 1 colonMinutes)
    if minute < 0 || minute > 59
      then Left ("timezone offset minutes out of range [0, 59]: " <> zone)
      else Right ((if "-" `T.isPrefixOf` zone then hour * 60 - minute else hour * 60 + minute) * 60)
 where
  signedNumber text = case T.uncons text of
    Just ('-', digits) | not (T.null digits) && T.all isDigit digits -> Right (negate (digitsValue digits))
    Just ('+', digits) | not (T.null digits) && T.all isDigit digits -> Right (digitsValue digits)
    _
      | not (T.null text) && T.all isDigit text -> Right (digitsValue text)
      | otherwise -> Left ("invalid time zone offset " <> zone)

durationComponent :: Component -> Int64 -> Maybe Int64
durationComponent component nanos = case component of
  Hours -> Just (quotient nanos 3600000000000)
  Minutes -> Just (quotient nanos 60000000000)
  Seconds -> Just (quotient nanos 1000000000)
  Milliseconds -> Just (quotient nanos 1000000)
  _ -> Nothing
