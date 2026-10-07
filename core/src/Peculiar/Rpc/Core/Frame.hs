module Peculiar.Rpc.Core.Frame
  ( Kind (..)
  , Frame (..)
  , FrameError (..)
  , Decoder
  , message
  , encode
  , encodeStrict
  , decoder
  , feed
  , finish
  , decodeAll
  , allowance
  ) where

import Data.Bits (shift, testBit, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, word32BE, word8)
import Data.ByteString.Builder.Extra (byteStringCopy, toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)

data Kind
  = Message
  | GrpcWebTrailers
  | ConnectEndStream
  deriving stock (Eq, Show, Enum, Bounded)

data Frame = Frame
  { kind :: Kind
  , compressed :: Bool
  , payload :: BS.ByteString
  }
  deriving stock (Eq, Show)

data FrameError
  = Truncated
  | TooLarge Int
  | UnknownFlags Word8
  deriving stock (Eq, Show)

data Header = Header
  { frameKind :: Kind
  , frameCompressed :: Bool
  , frameLength :: Int
  }

data Decoder = Decoder
  { limit :: Maybe Int
  , held :: [BS.ByteString]
  , size :: Int
  , header :: Maybe Header
  }

message :: BS.ByteString -> Frame
message = Frame Message False

flagsOf :: Frame -> Word8
flagsOf frame = kindBits frame.kind .|. (if frame.compressed then 0x01 else 0x00)
 where
  kindBits = \case
    Message -> 0x00
    GrpcWebTrailers -> 0x80
    ConnectEndStream -> 0x02

encode :: Frame -> Builder
encode frame = framing frame <> byteString frame.payload

framing :: Frame -> Builder
framing frame = word8 (flagsOf frame) <> word32BE (fromIntegral (BS.length frame.payload))

encodeStrict :: Frame -> BS.ByteString
encodeStrict frame = LBS.toStrict (toLazyByteStringWith (untrimmedStrategy size size) LBS.empty (framing frame <> byteStringCopy frame.payload))
 where
  size = 5 + BS.length frame.payload

decoder :: Maybe Int -> Decoder
decoder limit = Decoder{limit, held = [], size = 0, header = Nothing}

feed :: Decoder -> BS.ByteString -> Either FrameError ([Frame], Decoder)
feed state chunk
  | BS.null chunk = Right ([], state)
  | otherwise = drain [] state{held = chunk : state.held, size = state.size + BS.length chunk}

drain :: [Frame] -> Decoder -> Either FrameError ([Frame], Decoder)
drain done state = case state.header of
  Nothing
    | state.size < 5 -> Right (reverse done, state)
    | otherwise -> do
        let (front, rest) = BS.splitAt 5 (contents state)
        parsed <- parseHeader front
        case state.limit of
          Just most
            | parsed.frameLength > (if parsed.frameCompressed then allowance most else most) ->
                Left (TooLarge parsed.frameLength)
          _ -> drain done (reset state rest){header = Just parsed}
  Just parsed
    | state.size < parsed.frameLength -> Right (reverse done, state)
    | otherwise ->
        let (body, rest) = BS.splitAt parsed.frameLength (contents state)
            frame = Frame{kind = parsed.frameKind, compressed = parsed.frameCompressed, payload = body}
         in drain (frame : done) (reset state rest){header = Nothing}

allowance :: Int -> Int
allowance limit = limit + shift limit (-3) + 1024

contents :: Decoder -> BS.ByteString
contents state = BS.concat (reverse state.held)

reset :: Decoder -> BS.ByteString -> Decoder
reset state rest = state{held = [rest | not (BS.null rest)], size = BS.length rest}

parseHeader :: BS.ByteString -> Either FrameError Header
parseHeader bytes = case BS.unpack bytes of
  [flags, a, b, c, d] -> do
    frameKind <- kindOf flags
    pure
      Header
        { frameKind
        , frameCompressed = testBit flags 0
        , frameLength = foldl' (\acc byte -> shift acc 8 .|. fromIntegral byte) 0 [a, b, c, d]
        }
  _ -> Left Truncated

kindOf :: Word8 -> Either FrameError Kind
kindOf flags = case flags .&. 0xfe of
  0x00 -> Right Message
  0x80 -> Right GrpcWebTrailers
  0x02 -> Right ConnectEndStream
  _ -> Left (UnknownFlags flags)

finish :: Decoder -> Either FrameError ()
finish state
  | state.size == 0, Nothing <- state.header = Right ()
  | otherwise = Left Truncated

decodeAll :: Maybe Int -> BS.ByteString -> Either FrameError [Frame]
decodeAll limit bytes = do
  (frames, rest) <- feed (decoder limit) bytes
  frames <$ finish rest
