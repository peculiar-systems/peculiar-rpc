module Test.Peculiar.Rpc.Beside
  ( app
  , quiet
  , local
  , warping
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Peculiar.Rpc qualified as Rpc

app :: Wai.Application
app request respond = respond (Wai.responseLBS status200 [("content-type", "text/plain")] ("the app saw " <> LBS.fromStrict (Wai.rawPathInfo request)))

quiet :: Rpc.Calls
quiet = Rpc.defaultCalls{Rpc.report = const (pure ())}

local :: T.Text -> Int -> T.Text -> T.Text
local scheme port path = scheme <> "://127.0.0.1:" <> T.pack (show port) <> path

warping :: Wai.Application -> (Int -> IO a) -> IO a
warping = Warp.testWithApplicationSettings (Warp.setOnException (\_ _ -> pure ()) Warp.defaultSettings) . pure
