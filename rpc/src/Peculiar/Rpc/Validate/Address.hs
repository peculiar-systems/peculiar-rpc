module Peculiar.Rpc.Validate.Address
  ( isEmail
  , isHostname
  , isIp
  , isIpPrefix
  , isHostAndPort
  ) where

import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.Bits (complement, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Peculiar.Rpc.Validate.Arithmetic (shiftedLeft, shiftedRight)

isEmail :: T.Text -> Bool
isEmail text = case BS.split 64 (TE.encodeUtf8 text) of
  [local, domain] -> not (BS.null local) && BS.all localByte local && domainName domain
  _ -> False
 where
  localByte byte = alphanumeric byte || byte `BS.elem` ".!#$%&'*+/=?^_`{|}~-"
  domainName domain = not (BS.null domain) && all label (BS.split 46 domain)
  label part =
    BS.length part >= 1
      && BS.length part <= 63
      && any (alphanumeric . fst) (BS.uncons part)
      && any (alphanumeric . snd) (BS.unsnoc part)
      && BS.all (\byte -> alphanumeric byte || byte == 45) part

alphanumeric :: Word8 -> Bool
alphanumeric byte = between 48 57 byte || between 65 90 byte || between 97 122 byte

between :: Word8 -> Word8 -> Word8 -> Bool
between low high byte = byte >= low && byte <= high

isDigitByte :: Word8 -> Bool
isDigitByte = between 48 57

isHexByte :: Word8 -> Bool
isHexByte byte = isDigitByte byte || between 97 102 byte || between 65 70 byte

isHostname :: T.Text -> Bool
isHostname = hostnameBytes . TE.encodeUtf8

hostnameBytes :: BS.ByteString -> Bool
hostnameBytes value =
  BS.length trimmed <= 253 && go True (BS.split 46 trimmed)
 where
  trimmed = if BS.isSuffixOf "." value then BS.take (BS.length value - 1) value else value
  go allDigits = \case
    [] -> not allDigits
    part : rest
      | BS.null part || BS.length part > 63 || BS.isPrefixOf "-" part || BS.isSuffixOf "-" part -> False
      | not (BS.all (\byte -> alphanumeric byte || byte == 45) part) -> False
      | otherwise -> go (BS.all isDigitByte part) rest

data Scanner = Scanner
  { input :: BS.ByteString
  , index :: Int
  , octets :: [Word8]
  , prefixLength :: Int64
  , pieces :: [Word16]
  , doubleColonAt :: Int
  , doubleColonSeen :: Bool
  , dottedRaw :: BS.ByteString
  , dotted :: Maybe Word32
  , zoneFound :: Bool
  }

scanner :: BS.ByteString -> Scanner
scanner bytes =
  Scanner
    { input = bytes
    , index = 0
    , octets = []
    , prefixLength = 0
    , pieces = []
    , doubleColonAt = -1
    , doubleColonSeen = False
    , dottedRaw = BS.empty
    , dotted = Nothing
    , zoneFound = False
    }

type Scan = State Scanner

current :: Scan (Maybe Word8)
current = do
  bytes <- gets (.input)
  position <- gets (.index)
  pure (if position < BS.length bytes then Just (BS.index bytes position) else Nothing)

advance :: Scan ()
advance = modify' \state -> state{index = state.index + 1}

takeWhen :: (Word8 -> Bool) -> Scan Bool
takeWhen accept =
  current >>= \case
    Just byte | accept byte -> True <$ advance
    _ -> pure False

takeByte :: Word8 -> Scan Bool
takeByte expected = takeWhen (== expected)

digit :: Scan Bool
digit = takeWhen isDigitByte

hexDigit :: Scan Bool
hexDigit = takeWhen isHexByte

atEnd :: Scan Bool
atEnd = (\state -> state.index == BS.length state.input) <$> gets id

restoreTo :: Int -> Scan ()
restoreTo position = modify' \state -> state{index = position}

slice :: Int -> Scan BS.ByteString
slice start = do
  state <- gets id
  pure (BS.take (state.index - start) (BS.drop start state.input))

andThen :: Scan Bool -> Scan Bool -> Scan Bool
andThen first second = first >>= \ok -> if ok then second else pure False

infixr 3 `andThen`

decimalValue :: BS.ByteString -> Integer
decimalValue = foldl' (\acc byte -> acc * 10 + toInteger (byte - 48)) 0 . BS.unpack

digitsBounded :: Int -> Scan (Maybe BS.ByteString)
digitsBounded limit = do
  start <- gets (.index)
  let loop =
        digit >>= \case
          False -> pure True
          True -> do
            position <- gets (.index)
            if position - start > limit then pure False else loop
  ok <- loop
  if not ok
    then pure Nothing
    else do
      written <- slice start
      pure if BS.null written || BS.length written > 1 && BS.isPrefixOf "0" written then Nothing else Just written

v4Octet :: Scan Bool
v4Octet =
  digitsBounded 3 >>= \case
    Just written
      | decimalValue written <= 255 -> True <$ modify' (\state -> state{octets = state.octets <> [fromInteger (decimalValue written)]})
    _ -> pure False

v4AddressPart :: Scan Bool
v4AddressPart = do
  start <- gets (.index)
  ok <- v4Octet `andThen` takeByte 46 `andThen` v4Octet `andThen` takeByte 46 `andThen` v4Octet `andThen` takeByte 46 `andThen` v4Octet
  if ok then pure True else False <$ restoreTo start

v4PrefixLength :: Scan Bool
v4PrefixLength =
  digitsBounded 2 >>= \case
    Just written
      | decimalValue written <= 32 -> True <$ modify' (\state -> state{prefixLength = fromInteger (decimalValue written)})
    _ -> pure False

v4Bits :: [Word8] -> Word32
v4Bits = \case
  [a, b, c, d] -> shiftedLeft (fromIntegral a) 24 .|. shiftedLeft (fromIntegral b) 16 .|. shiftedLeft (fromIntegral c) 8 .|. fromIntegral d
  _ -> 0

v4Address :: BS.ByteString -> Bool
v4Address bytes = evalState (v4AddressPart `andThen` atEnd) (scanner bytes)

v4Prefix :: Bool -> BS.ByteString -> Bool
v4Prefix strict bytes = flip evalState (scanner bytes) do
  ok <- v4AddressPart `andThen` takeByte 47 `andThen` v4PrefixLength `andThen` atEnd
  state <- gets id
  let bits = v4Bits state.octets
      mask = if state.prefixLength == 32 then 0xffffffff else complement (shiftedRight 0xffffffff (fromIntegral state.prefixLength))
  pure (ok && (not strict || bits == bits .&. mask))

v6Address :: BS.ByteString -> Bool
v6Address bytes = evalState (v6AddressPart `andThen` atEnd) (scanner bytes)

v6Prefix :: Bool -> BS.ByteString -> Bool
v6Prefix strict bytes = flip evalState (scanner bytes) do
  ok <- v6AddressPart `andThen` (not <$> gets (.zoneFound)) `andThen` takeByte 47 `andThen` v6PrefixLength `andThen` atEnd
  state <- gets id
  pure (ok && (not strict || prefixOnly state))
 where
  prefixOnly state =
    and
      [ word == word .&. mask
      | (position, word) <- zip [0 :: Int64 ..] (v6Bits state)
      , let size = state.prefixLength - 64 * position
            mask :: Word64
            mask
              | size >= 64 = 0xFFFFFFFFFFFFFFFF
              | size < 0 = 0
              | otherwise = complement (shiftedRight 0xFFFFFFFFFFFFFFFF (fromIntegral size))
      ]

v6Bits :: Scanner -> [Word64]
v6Bits state =
  let withDotted = case state.dotted of
        Just bits -> state.pieces <> [fromIntegral (shiftedRight bits 16), fromIntegral bits]
        Nothing -> state.pieces
      filled
        | state.doubleColonSeen && length withDotted < 8 =
            let (before, after) = splitAt state.doubleColonAt withDotted
             in before <> replicate (8 - length withDotted) 0 <> after
        | otherwise = withDotted
   in case filled of
        [a, b, c, d, e, f, g, h] -> [joined [a, b, c, d], joined [e, f, g, h]]
        _ -> [0, 0]
 where
  joined = foldl' (\acc piece -> shiftedLeft acc 16 .|. fromIntegral piece) 0

v6PrefixLength :: Scan Bool
v6PrefixLength =
  digitsBounded 3 >>= \case
    Just written
      | decimalValue written <= 128 -> True <$ modify' (\state -> state{prefixLength = fromInteger (decimalValue written)})
    _ -> pure False

v6AddressPart :: Scan Bool
v6AddressPart = loop
 where
  loop =
    current >>= \case
      Nothing -> finish
      Just _ -> do
        state <- gets id
        dottedFound <- if state.doubleColonSeen || length state.pieces == 6 then v6Dotted else pure False
        if dottedFound
          then do
            raw <- gets (.dottedRaw)
            let candidate = scanner raw
                parsed = flip evalState candidate do
                  ok <- v4AddressPart `andThen` atEnd
                  found <- gets (.octets)
                  pure (ok, found)
            case parsed of
              (True, found) -> True <$ modify' (\s -> s{dotted = Just (v4Bits found)})
              _ -> pure False
          else
            h16 >>= \case
              Left () -> pure False
              Right True -> loop
              Right False ->
                takeByte 58 >>= \case
                  True ->
                    takeByte 58 >>= \case
                      True -> do
                        seen <- gets (.doubleColonSeen)
                        if seen
                          then pure False
                          else do
                            modify' \s -> s{doubleColonSeen = True, doubleColonAt = length s.pieces}
                            takeByte 58 >>= \case
                              True -> pure False
                              False -> loop
                      False -> do
                        s <- gets id
                        if s.index == 1 || s.index == BS.length s.input then pure False else loop
                  False ->
                    current >>= \case
                      Just 37 ->
                        v6Zone >>= \case
                          True -> finish
                          False -> pure False
                      _ -> finish
  finish = do
    state <- gets id
    pure if state.doubleColonSeen then length state.pieces < 8 else length state.pieces == 8

v6Zone :: Scan Bool
v6Zone = do
  start <- gets (.index)
  taken <- takeByte 37
  state <- gets id
  if taken && BS.length state.input - state.index > 0
    then True <$ modify' (\s -> s{index = BS.length s.input, zoneFound = True})
    else False <$ modify' (\s -> s{index = start, zoneFound = False})

v6Dotted :: Scan Bool
v6Dotted = do
  start <- gets (.index)
  modify' \s -> s{dottedRaw = BS.empty}
  let loop = do
        progressed <- digit
        if progressed then loop else takeByte 46 >>= (`when` loop)
  loop
  position <- gets (.index)
  if position - start >= 7
    then do
      raw <- slice start
      True <$ modify' (\s -> s{dottedRaw = raw})
    else False <$ restoreTo start

h16 :: Scan (Either () Bool)
h16 = do
  start <- gets (.index)
  let loop =
        hexDigit >>= \case
          False -> pure True
          True -> do
            position <- gets (.index)
            if position - start > 4 then pure False else loop
  ok <- loop
  if not ok
    then pure (Left ())
    else do
      written <- slice start
      if BS.null written
        then pure (Right False)
        else do
          let value = foldl' (\acc byte -> acc * 16 + hexValue byte) 0 (BS.unpack written) :: Word16
          Right True <$ modify' (\s -> s{pieces = s.pieces <> [value]})
 where
  hexValue byte
    | byte >= 97 = fromIntegral byte - 87
    | byte >= 65 = fromIntegral byte - 55
    | otherwise = fromIntegral byte - 48

isIp :: T.Text -> Int64 -> Bool
isIp text = ipBytes (TE.encodeUtf8 text)

ipBytes :: BS.ByteString -> Int64 -> Bool
ipBytes bytes = \case
  6 -> v6Address bytes
  4 -> v4Address bytes
  0 -> v4Address bytes || v6Address bytes
  _ -> False

isIpPrefix :: T.Text -> Int64 -> Bool -> Bool
isIpPrefix text version strict = case version of
  6 -> v6Prefix strict bytes
  4 -> v4Prefix strict bytes
  0 -> v6Prefix strict bytes || v4Prefix strict bytes
  _ -> False
 where
  bytes = TE.encodeUtf8 text

isHostAndPort :: T.Text -> Bool -> Bool
isHostAndPort text portRequired
  | BS.null bytes = False
  | BS.isPrefixOf "[" bytes = case BS.elemIndexEnd 93 bytes of
      Just end
        | end + 1 == BS.length bytes -> not portRequired && ipBytes (sliced 1 end) 6
        | Just (end + 1) == split -> ipBytes (sliced 1 end) 6 && isPort (BS.drop (end + 2) bytes)
      _ -> False
  | otherwise = case split of
      Nothing -> not portRequired && (hostnameBytes bytes || ipBytes bytes 4)
      Just position -> (hostnameBytes (BS.take position bytes) || ipBytes (BS.take position bytes) 4) && isPort (BS.drop (position + 1) bytes)
 where
  bytes = TE.encodeUtf8 text
  split = BS.elemIndexEnd 58 bytes
  sliced start end = BS.take (end - start) (BS.drop start bytes)

isPort :: BS.ByteString -> Bool
isPort port =
  not (BS.null port)
    && BS.all isDigitByte port
    && not (BS.length port > 1 && BS.isPrefixOf "0" port)
    && BS.length port <= 10
    && decimalValue port <= 65535
