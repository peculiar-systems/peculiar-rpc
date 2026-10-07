{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Echo (echo, app) where

import Data.ProtoLens (defMessage)
import Network.HTTP.Types (status404)
import Network.Wai qualified as Wai
import Peculiar.Rpc qualified as Rpc
import Proto.Peculiar.Test.V1.Echo

Rpc.deriveServer ''Echo

echo :: EchoServer
echo =
  EchoServer
    { say = \_ _ -> pure defMessage
    , count = \_ _ _ -> pure ()
    , sum = \_ _ -> pure defMessage
    , chat = \_ _ _ -> pure ()
    }

app :: Wai.Application
app _ respond = respond (Wai.responseLBS status404 [] "")
