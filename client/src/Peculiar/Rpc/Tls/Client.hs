module Peculiar.Rpc.Tls.Client
  ( ClientTls (..)
  , Trust (..)
  , systemTls
  , clientParams
  , webManager
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.X509.CertificateStore (makeCertificateStore)
import Data.X509.Memory (readSignedObjectFromMemory)
import Network.Connection qualified as Connection
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher (ciphersuite_strong)
import System.X509 (getSystemCertificateStore)

data Trust
  = SystemTrust
  | TrustPem BS.ByteString

data ClientTls = ClientTls
  { trust :: Trust
  , identity :: Maybe (BS.ByteString, BS.ByteString)
  }

systemTls :: ClientTls
systemTls = ClientTls{trust = SystemTrust, identity = Nothing}

clientParams :: ClientTls -> T.Text -> [BS.ByteString] -> IO (Either T.Text TLS.ClientParams)
clientParams tls host protocols = do
  authorities <- case tls.trust of
    SystemTrust -> getSystemCertificateStore
    TrustPem pem -> pure (makeCertificateStore (readSignedObjectFromMemory pem))
  pure case traverse (uncurry TLS.credentialLoadX509FromMemory) tls.identity of
    Left failure -> Left (T.pack failure)
    Right credential ->
      Right
        (TLS.defaultParamsClient (T.unpack host) "")
          { TLS.clientShared = TLS.defaultShared{TLS.sharedCAStore = authorities}
          , TLS.clientSupported = TLS.defaultSupported{TLS.supportedCiphers = ciphersuite_strong}
          , TLS.clientHooks =
              TLS.defaultClientHooks
                { TLS.onCertificateRequest = const (pure credential)
                , TLS.onSuggestALPN = pure (Just protocols)
                }
          }

webManager :: ClientTls -> IO (Either T.Text Http.Manager)
webManager tls =
  clientParams tls "" ["http/1.1"] >>= \case
    Left failure -> pure (Left failure)
    Right parameters ->
      Right
        <$> Http.newManager
          (mkManagerSettings (Connection.TLSSettings parameters) Nothing)
            { Http.managerResponseTimeout = Http.responseTimeoutNone
            }
