module Peculiar.Rpc.Json.Conformance
  ( runTestee
  , respond
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString, word32LE)
import Data.ProtoLens (Message, decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Json (Codec (..), Parsing (..), Registry, codec, register, render, wellKnown)
import Proto.Conformance.Conformance
import Proto.Google.Protobuf.TestMessagesProto2 (TestAllTypesProto2)
import Proto.Google.Protobuf.TestMessagesProto3 (TestAllTypesProto3)
import System.IO (hFlush, hSetBinaryMode, stdin, stdout)

runTestee :: IO ()
runTestee = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  loop
 where
  loop = do
    prefix <- BS.hGet stdin 4
    if BS.length prefix < 4
      then pure ()
      else do
        body <- BS.hGet stdin (fromIntegral (littleEndian prefix))
        let reply = either (\reason -> defMessage & #runtimeError .~ T.pack reason) respond (decodeMessage body)
        encoded <-
          try @SomeException (evaluate (encodeMessage reply)) >>= \case
            Right bytes -> pure bytes
            Left crash -> pure (encodeMessage (defMessage & #runtimeError .~ T.pack (displayException crash) :: ConformanceResponse))
        BS.hPut stdout (lengthPrefix (fromIntegral (BS.length encoded)) <> encoded)
        hFlush stdout
        loop

littleEndian :: BS.ByteString -> Word32
littleEndian = BS.foldr (\byte acc -> acc * 256 + fromIntegral byte) 0

lengthPrefix :: Word32 -> BS.ByteString
lengthPrefix = BS.toStrict . toLazyByteString . word32LE

registry :: Registry
registry = wellKnown <> register (Proxy @TestAllTypesProto3) <> register (Proxy @TestAllTypesProto2)

respond :: ConformanceRequest -> ConformanceResponse
respond request = case request ^. #messageType of
  "protobuf_test_messages.proto3.TestAllTypesProto3" -> roundTrip (Proxy @TestAllTypesProto3) request
  "protobuf_test_messages.proto2.TestAllTypesProto2" -> roundTrip (Proxy @TestAllTypesProto2) request
  other -> skipped ("no Haskell type for " <> other)

roundTrip :: forall a. (Message a) => Proxy a -> ConformanceRequest -> ConformanceResponse
roundTrip _ request = case parsed of
  Nothing -> skipped "only binary and JSON payloads"
  Just (Left reason) -> defMessage & #parseError .~ reason
  Just (Right message) -> case request ^. #requestedOutputFormat of
    PROTOBUF -> defMessage & #protobufPayload .~ encodeMessage message
    JSON -> either (\problem -> defMessage & #serializeError .~ render problem) (\bytes -> defMessage & #jsonPayload .~ TE.decodeUtf8 bytes) (json.encoded message)
    other -> skipped ("no " <> T.pack (show other) <> " output")
 where
  json = codec @a Parsing{registry, ignoreUnknown = request ^. #testCategory == JSON_IGNORE_UNKNOWN_PARSING_TEST}
  parsed = case request ^. #maybe'payload of
    Just (ConformanceRequest'ProtobufPayload bytes) -> Just (first T.pack (decodeMessage @a bytes))
    Just (ConformanceRequest'JsonPayload text) -> Just (first render (json.decoded (TE.encodeUtf8 text)))
    _ -> Nothing

skipped :: T.Text -> ConformanceResponse
skipped reason = defMessage & #skipped .~ reason
