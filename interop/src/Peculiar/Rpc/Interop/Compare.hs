module Peculiar.Rpc.Interop.Compare
  ( runCompare
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (unless, void)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Network.Socket qualified as Socket
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (ProcessHandle, readProcess, spawnProcess, terminateProcess, waitForProcess)

data Report = Report
  { average :: Double
  , rps :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Shape = Shape
  { label :: T.Text
  , method :: T.Text
  , messages :: Int
  , calls :: Int
  , body :: T.Text
  }

data Load = Load
  { callers :: Int
  , connections :: Int
  , calls :: Int
  }

messageSize :: Int
messageSize = 100

streamLength :: Int
streamLength = 1000

shapes :: [Shape]
shapes =
  [ Shape{label = "unary", method = "UnaryCall", messages = 1, calls = 3000, body = request [responseSize]}
  , Shape{label = "server stream", method = "StreamingOutputCall", messages = streamLength, calls = 200, body = request [parameters (replicate streamLength parameter)]}
  , Shape{label = "client stream", method = "StreamingInputCall", messages = streamLength, calls = 200, body = array (replicate streamLength (request [payload]))}
  , Shape{label = "bidi stream", method = "FullDuplexCall", messages = streamLength, calls = 200, body = array (replicate streamLength (request [parameters [parameter], payload]))}
  ]
 where
  request fields = "{" <> T.intercalate "," fields <> "}"
  array items = "[" <> T.intercalate "," items <> "]"
  responseSize = "\"responseSize\":" <> tshow messageSize
  parameter = "{\"size\":" <> tshow messageSize <> "}"
  parameters list = "\"responseParameters\":" <> array list
  payload = "\"payload\":{\"body\":\"" <> TE.decodeUtf8 (Base64.encode (BS.replicate messageSize 0)) <> "\"}"

loadsFor :: Shape -> [Load]
loadsFor shape =
  [ Load{callers = 1, connections = 1, calls = shape.calls}
  , Load{callers = 50, connections = 1, calls = 10 * shape.calls}
  , Load{callers = 50, connections = 10, calls = 10 * shape.calls}
  ]

runCompare :: IO ()
runCompare =
  getArgs >>= \case
    ghz : protoset : servers -> do
      TIO.putStrLn (row ["server", "shape", "callers", "calls/s", "messages/s", "latency"])
      for_ shapes \shape -> for_ servers \server -> case T.breakOn "=" (T.pack server) of
        (name, command) | not (T.null command) -> measure ghz protoset name (T.words (T.drop 1 command)) shape
        _ -> ioError (userError ("expected name=command, got " <> server))
    _ -> ioError (userError "usage: peculiar-rpc-interop-compare GHZ PROTOSET NAME=COMMAND...  (%PORT% in COMMAND is replaced)")

measure :: FilePath -> FilePath -> T.Text -> [T.Text] -> Shape -> IO ()
measure ghz protoset name command shape = go (5 :: Int)
 where
  go attempts = do
    port <- freePort
    listening <- bracket (start port) stop \_ ->
      awaitPort port >>= \case
        False -> pure False
        True -> True <$ run port
    unless (listening || attempts <= 1) (go (attempts - 1))
  run port = do
    directory <- getTemporaryDirectory
    let path = directory </> ("ghz-" <> show port <> ".json")
    bracket (TIO.writeFile path shape.body) (const (removeFile path)) \_ ->
      for_ (loadsFor shape) \load -> do
        raw <-
          readProcess
            ghz
            [ "--insecure"
            , "--protoset"
            , protoset
            , "--call"
            , "grpc.testing.TestService/" <> T.unpack shape.method
            , "--data-file"
            , path
            , "--concurrency"
            , show load.callers
            , "--connections"
            , show load.connections
            , "--total"
            , show load.calls
            , "--format"
            , "json"
            , "127.0.0.1:" <> show port
            ]
            ""
        case eitherDecodeStrict (TE.encodeUtf8 (T.pack raw)) of
          Left failure -> TIO.putStrLn (name <> ": " <> T.pack failure)
          Right (report :: Report) ->
            TIO.putStrLn
              ( row
                  [ name
                  , shape.label
                  , tshow load.callers <> " on " <> tshow load.connections
                  , tshow (round report.rps :: Int)
                  , tshow (round (report.rps * fromIntegral shape.messages) :: Int)
                  , tshow (fromIntegral (round (report.average / 10_000) :: Int) / 100 :: Double) <> "ms"
                  ]
              )
  start port = case fmap (T.replace "%PORT%" (tshow port)) command of
    program : arguments -> spawnProcess (T.unpack program) (fmap T.unpack arguments)
    [] -> ioError (userError ("no command for " <> T.unpack name))
  stop :: ProcessHandle -> IO ()
  stop handle = terminateProcess handle *> void (waitForProcess handle)

freePort :: IO Int
freePort =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \socket -> do
    Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    fromIntegral <$> Socket.socketPort socket

awaitPort :: Int -> IO Bool
awaitPort port = go (50 :: Int)
 where
  go 0 = pure False
  go remaining =
    try @IOException connectOnce >>= \case
      Right () -> pure True
      Left _ -> threadDelay 100_000 *> go (remaining - 1)
  connectOnce =
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \socket ->
      Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))

row :: [T.Text] -> T.Text
row cells = T.intercalate "  " (zipWith (`T.justifyLeft` ' ') [13, 14, 12, 10, 12, 10] cells)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
