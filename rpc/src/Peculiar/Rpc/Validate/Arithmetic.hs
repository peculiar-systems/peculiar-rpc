module Peculiar.Rpc.Validate.Arithmetic
  ( quotient
  , remainder
  , floorQuotRem
  , shiftedLeft
  , shiftedRight
  , digitValue
  , hexDigit
  ) where

import Data.Bits (Bits, shiftL, shiftR)
import Data.Char (chr, isAsciiLower, isAsciiUpper, ord)

quotient :: (Integral a) => a -> a -> a
quotient value divisor
  | divisor == 0 = 0
  | otherwise = quot value divisor

remainder :: (Integral a) => a -> a -> a
remainder value divisor
  | divisor == 0 = value
  | otherwise = rem value divisor

floorQuotRem :: Integer -> Integer -> (Integer, Integer)
floorQuotRem value divisor
  | divisor == 0 = (0, value)
  | otherwise =
      let (whole, left) = quotRem value divisor
       in if left /= 0 && (left < 0) /= (divisor < 0) then (whole - 1, left + divisor) else (whole, left)

shiftedLeft :: (Bits a, Num a) => a -> Int -> a
shiftedLeft value count
  | count < 0 = 0
  | otherwise = shiftL value count

shiftedRight :: (Bits a, Num a) => a -> Int -> a
shiftedRight value count
  | count < 0 = 0
  | otherwise = shiftR value count

digitValue :: Char -> Int
digitValue c
  | isAsciiLower c = ord c - ord 'a' + 10
  | isAsciiUpper c = ord c - ord 'A' + 10
  | otherwise = ord c - ord '0'

hexDigit :: Int -> Char
hexDigit value
  | value < 10 = chr (ord '0' + value)
  | otherwise = chr (ord 'a' + value - 10)
