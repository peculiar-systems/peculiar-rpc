module Peculiar.Rpc.Tls
  ( Tls (..)
  , requireClients
  , tlsFromPem
  , tlsFromFiles
  , Secured (..)
  , secure
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.X509 (CertificateChain, HashALG (..))
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Data.X509.Memory (readSignedObjectFromMemory)
import Data.X509.Validation (ValidationChecks (..), defaultChecks, defaultHooks, exceptionValidationCache, validate)
import Network.Socket (Socket)
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher (ciphersuite_strong)

trusted :: CertificateStore -> CertificateChain -> IO TLS.CertificateUsage
trusted store chain = do
  failures <- validate HashSHA256 defaultHooks defaultChecks{checkFQHN = False} store (exceptionValidationCache []) ("", "") chain
  pure if null failures then TLS.CertificateUsageAccept else TLS.CertificateUsageReject (TLS.CertificateRejectOther (show failures))

data Tls = Tls
  { credentials :: IO TLS.Credentials
  , clients :: Maybe CertificateStore
  }

requireClients :: BS.ByteString -> Tls -> Tls
requireClients authorities tls =
  tls{clients = Just (makeCertificateStore (readSignedObjectFromMemory authorities))}

tlsFromPem :: BS.ByteString -> BS.ByteString -> Either T.Text Tls
tlsFromPem certificate key = case TLS.credentialLoadX509FromMemory certificate key of
  Left failure -> Left (T.pack failure)
  Right credential -> Right Tls{credentials = pure (TLS.Credentials [credential]), clients = Nothing}

tlsFromFiles :: FilePath -> FilePath -> IO (Either T.Text Tls)
tlsFromFiles certificate key =
  TLS.credentialLoadX509 certificate key >>= \case
    Left failure -> pure (Left (T.pack failure))
    Right credential -> pure (Right Tls{credentials = pure (TLS.Credentials [credential]), clients = Nothing})

data Secured = Secured
  { context :: TLS.Context
  , protocol :: Maybe BS.ByteString
  }

secure :: Tls -> Socket -> IO (Either SomeException Secured)
secure tls socket = try do
  credentials <- tls.credentials
  context <- TLS.contextNew socket (parameters credentials tls.clients)
  TLS.handshake context
  protocol <- TLS.getNegotiatedProtocol context
  pure Secured{context, protocol}

parameters :: TLS.Credentials -> Maybe CertificateStore -> TLS.ServerParams
parameters credentials clients =
  TLS.defaultParamsServer
    { TLS.serverShared = TLS.defaultShared{TLS.sharedCredentials = credentials}
    , TLS.serverWantClientCert = isJust clients
    , TLS.serverSupported =
        TLS.defaultSupported
          { TLS.supportedCiphers = ciphersuite_strong
          , TLS.supportedVersions = [TLS.TLS13, TLS.TLS12]
          }
    , TLS.serverHooks =
        TLS.defaultServerHooks
          { TLS.onALPNClientSuggest = Just (pure . choose)
          , TLS.onClientCertificate = maybe (const (pure TLS.CertificateUsageAccept)) trusted clients
          }
    }
 where
  choose offered
    | "h2" `elem` offered = "h2"
    | otherwise = "http/1.1"
