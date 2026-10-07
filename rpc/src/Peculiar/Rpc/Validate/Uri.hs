module Peculiar.Rpc.Validate.Uri
  ( isUri
  , isUriReference
  ) where

import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Peculiar.Rpc.Validate.Address (isIp)

data Cursor = Cursor
  { input :: BS.ByteString
  , index :: Int
  , percentFound :: Bool
  }

type Scan = State Cursor

isUri :: T.Text -> Bool
isUri text = evalState uri (Cursor (TE.encodeUtf8 text) 0 False)

isUriReference :: T.Text -> Bool
isUriReference text = evalState (uri `orElse` relativeRef) (Cursor (TE.encodeUtf8 text) 0 False)

orElse :: Scan Bool -> Scan Bool -> Scan Bool
orElse first second = first >>= \ok -> if ok then pure True else second

andThen :: Scan Bool -> Scan Bool -> Scan Bool
andThen first second = first >>= \ok -> if ok then second else pure False

infixr 3 `andThen`

infixr 2 `orElse`

position :: Scan Int
position = gets (.index)

restoreTo :: Int -> Scan ()
restoreTo start = modify' \cursor -> cursor{index = start}

current :: Scan (Maybe Word8)
current = do
  cursor <- gets id
  pure (if cursor.index < BS.length cursor.input then Just (BS.index cursor.input cursor.index) else Nothing)

atEnd :: Scan Bool
atEnd = (\cursor -> cursor.index >= BS.length cursor.input) <$> gets id

takeWhen :: (Word8 -> Bool) -> Scan Bool
takeWhen accept =
  current >>= \case
    Just byte | accept byte -> True <$ modify' (\cursor -> cursor{index = cursor.index + 1})
    _ -> pure False

takeByte :: Char -> Scan Bool
takeByte expected = takeWhen (== fromIntegral (ord expected))

peek :: Char -> Scan Bool
peek expected = (== Just (fromIntegral (ord expected))) <$> current

repeatWhile :: Scan Bool -> Scan ()
repeatWhile step = step >>= (`when` repeatWhile step)

failAt :: Int -> Scan Bool
failAt start = False <$ restoreTo start

uri :: Scan Bool
uri = do
  start <- position
  ok <- scheme `andThen` takeByte ':' `andThen` hierPart
  if not ok
    then failAt start
    else do
      queryOk <- takeByte '?' >>= \taken -> if taken then query else pure True
      if not queryOk
        then pure False
        else do
          fragmentOk <- takeByte '#' >>= \taken -> if taken then fragment else pure True
          if not fragmentOk
            then pure False
            else atEnd >>= \end -> if end then pure True else failAt start

hierPart :: Scan Bool
hierPart = do
  start <- position
  ok <- takeDoubleSlash `andThen` authority `andThen` pathAbempty
  if ok then pure True else restoreTo start >> (pathAbsolute `orElse` pathRootless `orElse` pathEmpty)

relativeRef :: Scan Bool
relativeRef = do
  start <- position
  ok <- relativePart
  if not ok
    then pure False
    else do
      queryOk <- takeByte '?' >>= \taken -> if taken then query else pure True
      if not queryOk
        then failAt start
        else do
          fragmentOk <- takeByte '#' >>= \taken -> if taken then fragment else pure True
          if not fragmentOk
            then failAt start
            else atEnd >>= \end -> if end then pure True else failAt start

relativePart :: Scan Bool
relativePart = do
  start <- position
  ok <- takeDoubleSlash `andThen` authority `andThen` pathAbempty
  if ok then pure True else restoreTo start >> (pathAbsolute `orElse` pathNoscheme `orElse` pathEmpty)

scheme :: Scan Bool
scheme = do
  start <- position
  first <- alpha
  if first
    then do
      repeatWhile (alpha `orElse` digit `orElse` takeByte '+' `orElse` takeByte '-' `orElse` takeByte '.')
      colon <- peek ':'
      if colon then pure True else failAt start
    else failAt start

authority :: Scan Bool
authority = do
  start <- position
  hasUser <- userinfo
  userOk <- if hasUser then takeByte '@' else pure True
  if not userOk
    then failAt start
    else do
      hostOk <- host
      if not hostOk
        then failAt start
        else do
          portOk <- takeByte ':' >>= \taken -> if taken then port else pure True
          if not portOk
            then failAt start
            else authorityEnd >>= \end -> if end then pure True else failAt start

authorityEnd :: Scan Bool
authorityEnd =
  current >>= \case
    Nothing -> pure True
    Just byte -> pure (byte `elem` fmap (fromIntegral . ord) (T.unpack "?#/"))

userinfo :: Scan Bool
userinfo = do
  start <- position
  repeatWhile (unreserved `orElse` pctEncoded `orElse` subDelims `orElse` takeByte ':')
  at <- peek '@'
  if at then pure True else failAt start

hostPercentValid :: BS.ByteString -> Bool
hostPercentValid raw = case TE.decodeUtf8' (BS.pack (unescape (BS.unpack raw))) of
  Right _ -> True
  Left _ -> False
 where
  unescape = \case
    37 : high : low : rest -> (hexValue high * 16 + hexValue low) : unescape rest
    byte : rest -> byte : unescape rest
    [] -> []
  hexValue byte
    | byte >= 48 && byte <= 57 = byte - 48
    | byte >= 97 && byte <= 102 = byte - 87
    | byte >= 65 && byte <= 70 = byte - 55
    | otherwise = 0

host :: Scan Bool
host = do
  start <- position
  modify' \cursor -> cursor{percentFound = False}
  ok <- (peek '[' `andThen` ipLiteral) `orElse` regName
  if not ok
    then pure False
    else do
      cursor <- gets id
      if cursor.percentFound
        then pure (hostPercentValid (BS.take (cursor.index - start) (BS.drop start cursor.input)))
        else pure True

port :: Scan Bool
port = do
  start <- position
  repeatWhile digit
  end <- authorityEnd
  if end then pure True else failAt start

ipLiteral :: Scan Bool
ipLiteral = do
  start <- position
  opened <- takeByte '['
  if not opened
    then failAt start
    else do
      inside <- position
      address <- ipv6Address `andThen` takeByte ']'
      if address
        then pure True
        else do
          restoreTo inside
          zoned <- ipv6Zoned `andThen` takeByte ']'
          if zoned
            then pure True
            else do
              restoreTo inside
              future <- ipvFuture `andThen` takeByte ']'
              if future then pure True else failAt start

ipv6Address :: Scan Bool
ipv6Address = do
  start <- position
  repeatWhile (hexDigit `orElse` takeByte ':')
  cursor <- gets id
  let candidate = BS.take (cursor.index - start) (BS.drop start cursor.input)
  if isIp (TE.decodeLatin1 candidate) 6 then pure True else failAt start

ipv6Zoned :: Scan Bool
ipv6Zoned = do
  start <- position
  ok <- ipv6Address `andThen` takeByte '%' `andThen` takeByte '2' `andThen` takeByte '5' `andThen` zoneId
  if ok then pure True else failAt start

zoneId :: Scan Bool
zoneId = do
  start <- position
  repeatWhile (unreserved `orElse` pctEncoded)
  end <- position
  if end - start > 0 then pure True else failAt start

ipvFuture :: Scan Bool
ipvFuture = do
  start <- position
  ok <- takeByte 'v' `andThen` hexDigit
  if not ok
    then failAt start
    else do
      repeatWhile hexDigit
      dot <- takeByte '.'
      if not dot
        then failAt start
        else do
          before <- position
          repeatWhile (unreserved `orElse` subDelims `orElse` takeByte ':')
          after <- position
          if after - before >= 1 then pure True else failAt start

regName :: Scan Bool
regName = do
  start <- position
  repeatWhile (unreserved `orElse` pctEncoded `orElse` subDelims)
  end <- authorityEnd
  colon <- peek ':'
  if end || colon then pure True else failAt start

pathEnd :: Scan Bool
pathEnd =
  current >>= \case
    Nothing -> pure True
    Just byte -> pure (byte == 63 || byte == 35)

pathAbempty :: Scan Bool
pathAbempty = do
  start <- position
  repeatWhile (takeByte '/' `andThen` segment)
  end <- pathEnd
  if end then pure True else failAt start

pathAbsolute :: Scan Bool
pathAbsolute = do
  start <- position
  slash <- takeByte '/'
  if not slash
    then failAt start
    else do
      nonEmpty <- segmentNz
      when nonEmpty (repeatWhile (takeByte '/' `andThen` segment))
      end <- pathEnd
      if end then pure True else failAt start

pathNoscheme :: Scan Bool
pathNoscheme = do
  start <- position
  first <- segmentNzNc
  if not first
    then failAt start
    else do
      repeatWhile (takeByte '/' `andThen` segment)
      end <- pathEnd
      if end then pure True else failAt start

pathRootless :: Scan Bool
pathRootless = do
  start <- position
  first <- segmentNz
  if not first
    then failAt start
    else do
      repeatWhile (takeByte '/' `andThen` segment)
      end <- pathEnd
      if end then pure True else failAt start

pathEmpty :: Scan Bool
pathEmpty = pathEnd

segment :: Scan Bool
segment = True <$ repeatWhile pchar

segmentNz :: Scan Bool
segmentNz = do
  start <- position
  first <- pchar
  if first then segment else failAt start

segmentNzNc :: Scan Bool
segmentNzNc = do
  start <- position
  repeatWhile (unreserved `orElse` pctEncoded `orElse` subDelims `orElse` takeByte '@')
  end <- position
  if end - start > 0 then pure True else failAt start

pchar :: Scan Bool
pchar = unreserved `orElse` pctEncoded `orElse` subDelims `orElse` takeByte ':' `orElse` takeByte '@'

query :: Scan Bool
query = do
  start <- position
  repeatWhile (pchar `orElse` takeByte '/' `orElse` takeByte '?')
  next <- current
  if isNothing next || next == Just 35 then pure True else failAt start

fragment :: Scan Bool
fragment = do
  start <- position
  repeatWhile (pchar `orElse` takeByte '/' `orElse` takeByte '?')
  end <- atEnd
  if end then pure True else failAt start

pctEncoded :: Scan Bool
pctEncoded = do
  start <- position
  ok <- takeByte '%' `andThen` hexDigit `andThen` hexDigit
  if ok then True <$ modify' (\cursor -> cursor{percentFound = True}) else failAt start

unreserved :: Scan Bool
unreserved = alpha `orElse` digit `orElse` takeByte '-' `orElse` takeByte '_' `orElse` takeByte '.' `orElse` takeByte '~'

subDelims :: Scan Bool
subDelims = takeWhen (`elem` fmap (fromIntegral . ord) (T.unpack "!$&'()*+,;="))

alpha :: Scan Bool
alpha = takeWhen (\byte -> between 65 90 byte || between 97 122 byte)

digit :: Scan Bool
digit = takeWhen (between 48 57)

hexDigit :: Scan Bool
hexDigit = takeWhen (\byte -> between 48 57 byte || between 97 102 byte || between 65 70 byte)

between :: Word8 -> Word8 -> Word8 -> Bool
between low high byte = byte >= low && byte <= high

takeDoubleSlash :: Scan Bool
takeDoubleSlash = do
  first <- takeByte '/'
  if first then takeByte '/' else pure False
