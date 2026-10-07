module Peculiar.Rpc.Client
  ( Calling
  , Options (..)
  , defaultOptions
  , Transport (..)
  , Security (..)
  , Connection
  , withConnection
  , Address (..)
  , Dialing (..)
  , defaultDialing
  , connect
  , Keepalive (..)
  , withNativeClient
  , Target
  , Protocol (..)
  , Codec (..)
  , Registry
  , register
  , wellKnown
  , web
  , Compression (..)
  , Retry (..)
  , defaultRetry
  , ClientInterceptor (..)
  , Route (..)
  , RpcError (..)
  , Code (..)
  , codeNumber
  , codeFromNumber
  , connectName
  , codeFromConnectName
  , connectHttpStatus
  , Status (..)
  , Detail (..)
  , rpcError
  , throwRpc
  , withMetadata
  , Metadata
  , header
  , headers
  , lookupHeader
  , lookupHeaders
  , Timeout
  , microseconds
  , milliseconds
  , seconds
  , deriveClient
  , Client (..)
  , Http2Settings (..)
  , defaultHttp2
  , ClientTls (..)
  , Trust (..)
  , systemTls
  , webManager
  ) where

import Data.Text qualified as T
import Peculiar.Rpc.Calling (Calling, ClientInterceptor (..), Options (..), Transport (..), defaultOptions)
import Peculiar.Rpc.Client.Http1 (Target, web)
import Peculiar.Rpc.Client.Http2 (Address (..), Connection, Dialing (..), Security (..), connect, defaultDialing, withConnection)
import Peculiar.Rpc.Compression (Compression (..))
import Peculiar.Rpc.Core.Client (Protocol (..))
import Peculiar.Rpc.Core.Code (codeFromConnectName, codeFromNumber, codeNumber, connectHttpStatus, connectName)
import Peculiar.Rpc.Core.Codec (Codec (..))
import Peculiar.Rpc.Core.Timeout (Timeout, microseconds, milliseconds, seconds)
import Peculiar.Rpc.Derive.Client (deriveClient)
import Peculiar.Rpc.Error (Code (..), Detail (..), RpcError (..), Status (..), rpcError, throwRpc, withMetadata)
import Peculiar.Rpc.Http2Settings (Http2Settings (..), defaultHttp2)
import Peculiar.Rpc.Json (Registry, register, wellKnown)
import Peculiar.Rpc.Liveness (Keepalive (..))
import Peculiar.Rpc.Metadata (Metadata, header, headers, lookupHeader, lookupHeaders)
import Peculiar.Rpc.Remote (Client (..))
import Peculiar.Rpc.Retry (Retry (..), defaultRetry)
import Peculiar.Rpc.Route (Route (..))
import Peculiar.Rpc.Tls.Client (ClientTls (..), Trust (..), systemTls, webManager)

withNativeClient :: Security -> T.Text -> Int -> (Transport -> IO a) -> IO a
withNativeClient security host port use = withConnection security host port (use . (`Http2` Grpc))
