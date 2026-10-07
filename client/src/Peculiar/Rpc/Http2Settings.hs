module Peculiar.Rpc.Http2Settings
  ( Http2Settings (..)
  , defaultHttp2
  , http2Settings
  ) where

import Network.HTTP2.Server qualified as HTTP2

data Http2Settings = Http2Settings
  { maxConcurrentStreams :: Int
  , streamWindow :: Int
  , connectionWindow :: Int
  , pingRateLimit :: Int
  , emptyFrameRateLimit :: Int
  , settingsRateLimit :: Int
  , rstRateLimit :: Int
  }

defaultHttp2 :: Http2Settings
defaultHttp2 =
  Http2Settings
    { maxConcurrentStreams = 128
    , streamWindow = 256 * 1024
    , connectionWindow = 2 * 1024 * 1024
    , pingRateLimit = 100_000
    , emptyFrameRateLimit = 100_000
    , settingsRateLimit = 1000
    , rstRateLimit = 1000
    }

http2Settings :: Http2Settings -> HTTP2.Settings
http2Settings settings =
  HTTP2.defaultSettings
    { HTTP2.initialWindowSize = settings.streamWindow
    , HTTP2.maxConcurrentStreams = Just settings.maxConcurrentStreams
    , HTTP2.pingRateLimit = settings.pingRateLimit
    , HTTP2.emptyFrameRateLimit = settings.emptyFrameRateLimit
    , HTTP2.settingsRateLimit = settings.settingsRateLimit
    , HTTP2.rstRateLimit = settings.rstRateLimit
    }
