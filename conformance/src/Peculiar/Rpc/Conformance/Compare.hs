module Peculiar.Rpc.Conformance.Compare
  ( runCompare
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Bits (shift, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (hPutBuilder, word32BE)
import Data.Foldable (for_)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Lens.Family2 ((&), (.~), (^.))
import Proto.Connectrpc.Conformance.V1.Config (HTTPVersion (..), Protocol (..))
import Proto.Connectrpc.Conformance.V1.ServerCompat (ServerCompatRequest, ServerCompatResponse)
import System.Environment (getArgs)
import System.IO (Handle, hClose, hFlush, hSetBinaryMode)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, readProcess, terminateProcess, waitForProcess)
import Text.Printf (printf)

data Report = Report
  { average :: Double
  , rps :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Load = Load
  { label :: T.Text
  , concurrency :: Int
  , total :: Int
  }

loads :: [Load]
loads =
  [ Load{label = "latency, 1 caller", concurrency = 1, total = 3000}
  , Load{label = "throughput, 50 callers", concurrency = 50, total = 30000}
  ]

runCompare :: IO ()
runCompare =
  getArgs >>= \case
    ghz : protoset : servers -> for_ servers \server -> case T.breakOn "=" (T.pack server) of
      (name, command) | not (T.null command) -> measure ghz protoset name (T.unpack (T.drop 1 command))
      _ -> ioError (userError ("expected name=command, got " <> server))
    _ -> ioError (userError "usage: peculiar-rpc-compare GHZ PROTOSET NAME=COMMAND...")

measure :: FilePath -> FilePath -> T.Text -> FilePath -> IO ()
measure ghz protoset name command =
  bracket (start command) stop \(input, output, _) -> do
    send input (defMessage & #protocol .~ PROTOCOL_GRPC & #httpVersion .~ HTTP_VERSION_2 :: ServerCompatRequest)
    started <- receive output
    let target = T.unpack (started ^. #host) <> ":" <> show (started ^. #port)
    for_ loads \load -> do
      raw <-
        readProcess
          ghz
          [ "--insecure"
          , "--protoset"
          , protoset
          , "--call"
          , "connectrpc.conformance.v1.ConformanceService/Unary"
          , "--data"
          , "{}"
          , "--concurrency"
          , show load.concurrency
          , "--total"
          , show load.total
          , "--format"
          , "json"
          , target
          ]
          ""
      case eitherDecodeStrict (TE.encodeUtf8 (T.pack raw)) of
        Left failure -> TIO.putStrLn (name <> ": " <> T.pack failure)
        Right (report :: Report) ->
          printf "%-12s %-24s %9.0f req/s  %8.1f us\n" (T.unpack name) (T.unpack load.label) report.rps (report.average / 1000)
 where
  start path = do
    (Just input, Just output, _, handle) <- createProcess (proc path []){std_in = CreatePipe, std_out = CreatePipe}
    hSetBinaryMode input True
    hSetBinaryMode output True
    pure (input, output, handle)
  stop (input, _, handle) = hClose input *> terminateProcess handle *> void (waitForProcess handle)

send :: Handle -> ServerCompatRequest -> IO ()
send handle message = do
  let body = encodeMessage message
  hPutBuilder handle (word32BE (fromIntegral (BS.length body)))
  BS.hPut handle body
  hFlush handle

receive :: Handle -> IO ServerCompatResponse
receive handle = do
  prefix <- BS.hGet handle 4
  body <- BS.hGet handle (BS.foldl' (\acc byte -> shift acc 8 .|. fromIntegral byte) 0 prefix)
  either (ioError . userError) pure (decodeMessage body)
