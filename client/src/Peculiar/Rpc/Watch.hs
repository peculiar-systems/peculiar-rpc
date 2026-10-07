module Peculiar.Rpc.Watch
  ( Keep (..)
  , FrameHeader (..)
  , watching
  , pingFrame
  , goawayFrame
  , pingType
  , goawayType
  ) where

import Data.Bits (shift, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word32, Word8)

data Keep = Keep | Drop
  deriving stock (Eq, Show)

data FrameHeader = FrameHeader
  { kind :: Word8
  , flags :: Word8
  , stream :: Word32
  }
  deriving stock (Eq, Show)

data State
  = Preface Int
  | Header BS.ByteString
  | Payload Int Keep

pingType :: Word8
pingType = 0x6

goawayType :: Word8
goawayType = 0x7

watching :: Int -> (FrameHeader -> IO Keep) -> IO BS.ByteString -> IO (IO BS.ByteString)
watching preface decide receive = do
  state <- newIORef (if preface > 0 then Preface preface else Header BS.empty)
  let pull = do
        chunk <- receive
        if BS.null chunk
          then pure BS.empty
          else do
            current <- readIORef state
            (kept, next) <- step current chunk []
            writeIORef state next
            let out = BS.concat (reverse kept)
            if BS.null out then pull else pure out
  pure pull
 where
  step current chunk kept
    | BS.null chunk = pure (kept, current)
    | otherwise = case current of
        Preface remaining -> do
          let (passed, rest) = BS.splitAt remaining chunk
              left = remaining - BS.length passed
          step (if left > 0 then Preface left else Header BS.empty) rest (passed : kept)
        Header held -> do
          let (taken, rest) = BS.splitAt (9 - BS.length held) chunk
              header = held <> taken
          if BS.length header < 9
            then pure (kept, Header header)
            else do
              keep <- decide (parse header)
              let size = lengthOf header
                  kept' = if keep == Keep then header : kept else kept
              step (if size > 0 then Payload size keep else Header BS.empty) rest kept'
        Payload remaining keep -> do
          let (body, rest) = BS.splitAt remaining chunk
              left = remaining - BS.length body
              kept' = if keep == Keep then body : kept else kept
          step (if left > 0 then Payload left keep else Header BS.empty) rest kept'

lengthOf :: BS.ByteString -> Int
lengthOf header = foldl' (\acc byte -> shift acc 8 .|. fromIntegral byte) 0 (BS.unpack (BS.take 3 header))

parse :: BS.ByteString -> FrameHeader
parse header =
  FrameHeader
    { kind = BS.index header 3
    , flags = BS.index header 4
    , stream = foldl' (\acc byte -> shift acc 8 .|. fromIntegral byte) 0 (BS.unpack (BS.drop 5 header)) .&. 0x7fffffff
    }

frame :: Word8 -> BS.ByteString -> BS.ByteString
frame kind payload =
  BS.pack (word24 (BS.length payload) <> [kind, 0, 0, 0, 0, 0]) <> payload
 where
  word24 n = [fromIntegral (shift n (-16)), fromIntegral (shift n (-8)), fromIntegral n]

pingFrame :: BS.ByteString
pingFrame = frame pingType (BS.replicate 8 0x70)

goawayFrame :: BS.ByteString
goawayFrame = frame goawayType (BS.pack [0x7f, 0xff, 0xff, 0xff, 0, 0, 0, 0])
