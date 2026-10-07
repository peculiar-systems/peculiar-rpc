module Peculiar.Rpc.Wai
  ( bridge
  , reply
  , incomingOf
  ) where

import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.CaseInsensitive qualified as CI
import Data.IORef (newIORef)
import Network.HTTP.Semantics (InpObj (..), NextTrailersMaker (..), OutBody (..), OutBodyIface (..), OutObj (..), TrailersMaker)
import Network.HTTP.Semantics.Server (Server)
import Network.HTTP.Semantics.Server.Internal (Aux (..), Request (..), Response (..))
import Network.HTTP.Types (Header, Status, http20, mkStatus, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Peculiar.Rpc.Core.Wire (Incoming (..))
import Peculiar.Rpc.Web.Table (table)
import System.TimeManager qualified as TimeManager

bridge :: TimeManager.Manager -> Server -> Wai.Application
bridge manager server request respond = do
  handle <- TimeManager.register manager (pure ())
  TimeManager.pause handle
  trailers <- newIORef Nothing
  body <- bodyOf request
  let inner =
        Request
          InpObj
            { inpObjHeaders = table (headersOf request)
            , inpObjBodySize = Nothing
            , inpObjBody = body.pull
            , inpObjTrailers = trailers
            }
      aux =
        Aux
          { auxTimeHandle = handle
          , auxMySockAddr = Wai.remoteHost request
          , auxPeerSockAddr = Wai.remoteHost request
          , auxSendInformational = \_ _ -> pure ()
          }
  answered <- newEmptyMVar
  let answer response _ = void (tryPutMVar answered (Just response))
      serve = server inner aux answer `finally` void (tryPutMVar answered Nothing)
  withAsync serve \running ->
    do
      received <-
        takeMVar answered >>= \case
          Just response -> reply request respond response
          Nothing -> respond (Wai.responseLBS status500 [] "the server gave no response")
      received <$ waitCatch running
      `finally` (body.close *> TimeManager.cancel handle)

reply :: Wai.Request -> (Wai.Response -> IO a) -> Response -> IO a
reply request respond (Response out)
  | Wai.httpVersion request < http20 = respond (toWai out)
  | otherwise = do
      let sent = trailed out
      Warp.setHTTP2Data request (Just Warp.defaultHTTP2Data{Warp.http2dataTrailers = sent.outObjTrailers})
      respond (toWai sent)

trailed :: OutObj -> OutObj
trailed out = case out.outObjBody of
  OutBodyNone
    | any isNative (lookup "content-type" named)
    , any ((== "grpc-status") . fst) named ->
        out
          { outObjHeaders = heading
          , outObjBody = OutBodyBuilder mempty
          , outObjTrailers = ending trailing
          }
  _ -> out
 where
  named = [(CI.foldedCase name, value) | (name, value) <- out.outObjHeaders]
  (heading, trailing) = foldr sort ([], []) out.outObjHeaders
  sort header@(name, _) (heads, tails)
    | CI.foldedCase name `elem` [":status", "content-type"] = (header : heads, tails)
    | otherwise = (heads, header : tails)
  isNative contentType = "application/grpc" `BS.isPrefixOf` contentType && not ("application/grpc-web" `BS.isPrefixOf` contentType)

ending :: [Header] -> TrailersMaker
ending headers = \case
  Nothing -> pure (Trailers headers)
  Just _ -> pure (NextTrailersMaker (ending headers))

incomingOf :: Wai.Request -> Incoming
incomingOf request =
  Incoming
    { method = Wai.requestMethod request
    , path = Wai.rawPathInfo request <> Wai.rawQueryString request
    , headers = headersOf request
    }

headersOf :: Wai.Request -> [(BS.ByteString, BS.ByteString)]
headersOf request =
  [ (":method", Wai.requestMethod request)
  , (":path", Wai.rawPathInfo request <> Wai.rawQueryString request)
  , (":scheme", if Wai.isSecure request then "https" else "http")
  ]
    <> [(":authority", host) | Just host <- [Wai.requestHeaderHost request]]
    <> [(CI.foldedCase name, value) | (name, value) <- Wai.requestHeaders request, name /= "host"]

data Body = Body
  { pull :: IO (BS.ByteString, Bool)
  , close :: IO ()
  }

bodyOf :: Wai.Request -> IO Body
bodyOf request = do
  finished <- newMVar False
  let pull =
        modifyMVar finished \case
          True -> pure (True, (BS.empty, True))
          False -> do
            chunk <- Wai.getRequestBodyChunk request
            pure if BS.null chunk then (True, (BS.empty, True)) else (False, (chunk, False))
      close
        | Wai.httpVersion request < http20 = modifyMVar_ finished \done -> True <$ unless done (drain (0 :: Int))
        | otherwise = pure ()
      drain seen = do
        chunk <- Wai.getRequestBodyChunk request
        unless (BS.null chunk || seen > 67108864) (drain (seen + BS.length chunk))
  pure Body{pull, close}

toWai :: OutObj -> Wai.Response
toWai out = case out.outObjBody of
  OutBodyNone -> Wai.responseLBS status headers ""
  OutBodyBuilder builder -> Wai.responseBuilder status headers builder
  OutBodyStreaming stream -> Wai.responseStream status headers \write flush -> void (try @SomeException (stream write flush))
  OutBodyStreamingIface stream ->
    Wai.responseStream status headers \write flush ->
      void . try @SomeException . stream $
        OutBodyIface
          { outBodyUnmask = id
          , outBodyPush = write
          , outBodyPushFinal = \chunk -> write chunk *> flush
          , outBodyCancel = const (pure ())
          , outBodyFlush = flush
          }
  OutBodyFile _ -> Wai.responseLBS status500 [] "file bodies are not served"
 where
  status = statusOf out
  headers = filter ((/= ":status") . CI.foldedCase . fst) out.outObjHeaders

statusOf :: OutObj -> Status
statusOf out = case lookup ":status" [(CI.foldedCase name, value) | (name, value) <- out.outObjHeaders] >>= BC.readInt of
  Just (code, rest) | BS.null rest -> mkStatus code ""
  _ -> status500
