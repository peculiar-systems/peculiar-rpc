module Peculiar.Rpc.Conformance.Delimited
  ( readDelimited
  , writeDelimited
  ) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.Bits (shift, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (hPutBuilder, word32BE)
import Data.ProtoLens (Message, decodeMessage, encodeMessage)
import System.IO (hFlush, stdin, stdout)

readDelimited :: (Message message) => IO (Maybe message)
readDelimited = do
  prefix <- BS.hGet stdin 4
  if BS.length prefix < 4
    then pure Nothing
    else do
      let size = BS.foldl' (\acc byte -> shift acc 8 .|. fromIntegral byte) 0 prefix
      body <- BS.hGet stdin size
      pure case decodeMessage body of
        Right message -> Just message
        Left _ -> Nothing

writeDelimited :: (Message message) => MVar () -> message -> IO ()
writeDelimited lock message = withMVar lock \() -> do
  let body = encodeMessage message
  hPutBuilder stdout (word32BE (fromIntegral (BS.length body)))
  BS.hPut stdout body
  hFlush stdout
