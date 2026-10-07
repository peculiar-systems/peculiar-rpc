module Peculiar.Rpc.Web.Http
  ( Respond
  , build
  , fromReply
  , toHttp
  , fromHttp
  , withHeaders
  , rebuilt
  , headerList
  , statusOf
  , withoutStatus
  , strict
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.IORef (newIORef)
import Network.HTTP.Semantics
  ( InpObj (..)
  , OutBody (..)
  , OutObj (..)
  , defaultTrailersMaker
  , getFieldValue
  , tokenAuthority
  , tokenFoldedKey
  , tokenMethod
  , tokenPath
  , tokenScheme
  )
import Network.HTTP.Semantics.Server (PushPromise, requestHeaders)
import Network.HTTP.Semantics.Server.Internal (Request (..), Response (..))
import Network.HTTP.Types (Header)
import Peculiar.Rpc.Core.Response (Reply (..))
import Peculiar.Rpc.Core.Trailers qualified as Core
import Peculiar.Rpc.Web.Table (table)

type Respond = Response -> [PushPromise] -> IO ()

build :: Int -> [Core.Header] -> BS.ByteString -> Response
build status headers body =
  Response
    OutObj
      { outObjHeaders = (":status", BC.pack (show status)) : fmap toHttp headers
      , outObjBody = if BS.null body then OutBodyNone else OutBodyBuilder (byteString body)
      , outObjTrailers = defaultTrailersMaker
      }

fromReply :: Reply -> Response
fromReply reply = build reply.httpStatus reply.headers reply.body

toHttp :: Core.Header -> Header
toHttp (name, value) = (CI.mk name, value)

fromHttp :: Header -> Core.Header
fromHttp (name, value) = (CI.foldedCase name, value)

withHeaders :: [Core.Header] -> Response -> Response
withHeaders extra (Response out) = Response out{outObjHeaders = out.outObjHeaders <> fmap toHttp extra}

rebuilt :: [Core.Header] -> IO (BS.ByteString, Bool) -> IO Request
rebuilt headers body = do
  trailers <- newIORef Nothing
  pure (Request InpObj{inpObjHeaders = table headers, inpObjBodySize = Nothing, inpObjBody = body, inpObjTrailers = trailers})

headerList :: Request -> [Core.Header]
headerList request =
  [ (tokenFoldedKey token, value)
  | token <- [tokenMethod, tokenPath, tokenAuthority, tokenScheme]
  , Just value <- [getFieldValue token values]
  ]
    <> [ (name, value)
       | (token, value) <- listed
       , let name = tokenFoldedKey token
       , not (":" `BS.isPrefixOf` name)
       ]
 where
  (listed, values) = requestHeaders request

statusOf :: OutObj -> Maybe BS.ByteString
statusOf out = lookup ":status" (fmap fromHttp out.outObjHeaders)

withoutStatus :: OutObj -> [Core.Header]
withoutStatus out = filter ((/= ":status") . fst) (fmap fromHttp out.outObjHeaders)

strict :: Builder -> BS.ByteString
strict = LBS.toStrict . toLazyByteString
