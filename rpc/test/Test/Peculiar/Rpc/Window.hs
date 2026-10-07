module Test.Peculiar.Rpc.Window (tests) where

import Control.Concurrent (threadWaitReadSTM)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shift, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Word (Word32, Word8)
import Hedgehog (Group (..), Property, annotateShow, property, withTests, (===))
import Lens.Family2 ((&), (.~))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import Peculiar.Rpc.Core.Frame (encodeStrict, message)
import Proto.Peculiar.Test.V1.Echo (CountRequest)
import System.IO (hPrint, stderr)
import System.Posix.Types (Fd (..))

tests :: Int -> Group
tests port =
  Group
    "Window"
    [ ("a stream opened right after SETTINGS sends its whole response on the advertised window", withTests 1 (wholeResponses port))
    ]

data Outcome
  = Whole
  | Stalled Int
  | Refused Word8
  | Closed Int
  deriving stock (Eq, Show)

connections :: Int
connections = 2000

atOnce :: Int
atOnce = 64

upto :: Int
upto = 20000

silence :: Int
silence = 30000000

wholeResponses :: Int -> Property
wholeResponses port = property do
  outcomes <- liftIO do
    slots <- newQSem atOnce
    forConcurrently [1 .. connections] \_ -> bracket_ (waitQSem slots) (signalQSem slots) (call port)
  let failed = filter (/= Whole) outcomes
  annotateShow (take 10 failed)
  length failed === 0

call :: Int -> IO Outcome
call port = bracket (connect port) Socket.close \socket -> do
  SocketBS.sendAll socket opening
  outcome <- await socket 0 BS.empty
  unless (outcome == Whole) (hPrint stderr outcome)
  pure outcome

connect :: Int -> IO Socket.Socket
connect port = do
  socket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
  pure socket

largest :: Word32
largest = 2 ^ (31 :: Int) - 1

opening :: BS.ByteString
opening =
  LBS.toStrict . B.toLazyByteString $
    B.byteString "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
      <> frame frameSettings 0 0 (B.word16BE 4 <> B.word32BE largest)
      <> frame frameWindowUpdate 0 0 (B.word32BE (largest - 65535))
      <> frame frameHeaders endHeaders 1 (B.byteString requestHeaders)
      <> frame frameData endStream 1 (B.byteString requestBody)

requestBody :: BS.ByteString
requestBody = encodeStrict (message (encodeMessage (defMessage & #upto .~ fromIntegral upto :: CountRequest)))

requestHeaders :: BS.ByteString
requestHeaders =
  BS.concat
    [ BS.pack [0x83, 0x86]
    , literal 4 "/peculiar.test.v1.Echo/Count"
    , literal 1 "127.0.0.1"
    , literalNamed "content-type" "application/grpc"
    , literalNamed "te" "trailers"
    ]

literal :: Word8 -> BS.ByteString -> BS.ByteString
literal index value = BS.cons index (counted value)

literalNamed :: BS.ByteString -> BS.ByteString -> BS.ByteString
literalNamed name value = BS.cons 0 (counted name <> counted value)

counted :: BS.ByteString -> BS.ByteString
counted text = BS.cons (fromIntegral (BS.length text)) text

expectedBytes :: Int
expectedBytes = sum [5 + 1 + varintLength n | n <- [1 .. upto]]

varintLength :: Int -> Int
varintLength n
  | n < 0x80 = 1
  | otherwise = 1 + varintLength (shift n (-7))

frame :: Word8 -> Word8 -> Word32 -> B.Builder -> B.Builder
frame kind flags stream payload =
  B.word8 (fromIntegral (shift len (-16)))
    <> B.word16BE (fromIntegral len)
    <> B.word8 kind
    <> B.word8 flags
    <> B.word32BE stream
    <> B.lazyByteString body
 where
  body = B.toLazyByteString payload
  len = LBS.length body

frameData, frameHeaders, frameRstStream, frameSettings, frameGoAway, frameWindowUpdate :: Word8
frameData = 0
frameHeaders = 1
frameRstStream = 3
frameSettings = 4
frameGoAway = 7
frameWindowUpdate = 8

endStream, endHeaders, ack, padded :: Word8
endStream = 0x1
ack = 0x1
endHeaders = 0x4
padded = 0x8

await :: Socket.Socket -> Int -> BS.ByteString -> IO Outcome
await socket received held =
  exactly socket received held 9 >>= \case
    Left ended -> pure ended
    Right (header, afterHeader) -> do
      let len = fromIntegral (shift (word32At header 0) (-8))
          kind = BS.index header 3
          flags = BS.index header 4
          stream = word32At header 5 .&. 0x7fffffff
      exactly socket received afterHeader len >>= \case
        Left ended -> pure ended
        Right (payload, rest) -> onFrame kind flags stream payload rest
 where
  onFrame kind flags stream payload rest
    | kind == frameSettings && flags .&. ack == 0 = do
        SocketBS.sendAll socket (LBS.toStrict (B.toLazyByteString (frame frameSettings ack 0 mempty)))
        await socket received rest
    | kind == frameData && stream == 1 = do
        let padding = if flags .&. padded /= 0 then maybe 0 ((+ 1) . fromIntegral . fst) (BS.uncons payload) else 0
        let total = received + BS.length payload - padding
        if flags .&. endStream /= 0 then pure (whole total) else await socket total rest
    | kind == frameHeaders && stream == 1 && flags .&. endStream /= 0 = pure (whole received)
    | kind == frameRstStream || kind == frameGoAway = pure (Refused kind)
    | otherwise = await socket received rest

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset = foldl' (\acc i -> shift acc 8 .|. fromIntegral (BS.index bytes (offset + i))) 0 [0 .. 3]

whole :: Int -> Outcome
whole total
  | total == expectedBytes = Whole
  | otherwise = Stalled total

exactly :: Socket.Socket -> Int -> BS.ByteString -> Int -> IO (Either Outcome (BS.ByteString, BS.ByteString))
exactly socket received held wanted
  | BS.length held >= wanted = pure (Right (BS.splitAt wanted held))
  | otherwise =
      readable socket >>= \case
        False -> pure (Left (Stalled received))
        True -> do
          more <- SocketBS.recv socket 65536
          if BS.null more then pure (Left (Closed received)) else exactly socket received (held <> more) wanted

readable :: Socket.Socket -> IO Bool
readable socket =
  bracket (Socket.withFdSocket socket (threadWaitReadSTM . Fd)) snd \(ready, _) -> do
    quiet <- registerDelay silence
    atomically ((True <$ ready) `orElse` (False <$ (readTVar quiet >>= check)))
