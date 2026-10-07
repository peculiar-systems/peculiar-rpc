module Peculiar.Rpc.Core.Connect
  ( ErrorJson (..)
  , DetailJson (..)
  , EndStream (..)
  , errorJson
  , statusFromErrorJson
  , endStream
  , encodeJson
  , decodeErrorJson
  , decodeEndStream
  , trailerPrefix
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, OmitNothingFields, Rename)
import GHC.Generics (Generic)
import Peculiar.Rpc.Core.Base64 qualified as Base64
import Peculiar.Rpc.Core.Code (Code (..), codeFromConnectName, connectName)
import Peculiar.Rpc.Core.Status (Detail (..), Status (..))
import Peculiar.Rpc.Core.Trailers (Header)

data DetailJson = DetailJson
  { detailType :: T.Text
  , value :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[Rename "detailType" "type"]] DetailJson

data ErrorJson = ErrorJson
  { code :: Maybe T.Text
  , message :: Maybe T.Text
  , details :: Maybe [DetailJson]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields] ErrorJson

data EndStream = EndStream
  { endError :: Maybe ErrorJson
  , metadata :: Maybe (Map.Map T.Text [T.Text])
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields, FieldLabelModifier '[Rename "endError" "error"]] EndStream

typePrefix :: T.Text
typePrefix = "type.googleapis.com/"

errorJson :: Status -> ErrorJson
errorJson status =
  ErrorJson
    { code = Just (connectName status.code)
    , message = if T.null status.message then Nothing else Just status.message
    , details = if null status.details then Nothing else Just (fmap detailJson status.details)
    }
 where
  detailJson detail =
    DetailJson
      { detailType = fromMaybe detail.typeUrl (T.stripPrefix typePrefix detail.typeUrl)
      , value = TE.decodeUtf8Lenient (Base64.encodeUnpadded detail.value)
      }

statusFromErrorJson :: Code -> ErrorJson -> Status
statusFromErrorJson fallback json =
  Status
    { code = fromMaybe fallback (json.code >>= codeFromConnectName)
    , message = fromMaybe "" json.message
    , details = foldMap (concatMap detail) json.details
    }
 where
  detail raw = case Base64.decodeEither (TE.encodeUtf8 raw.value) of
    Right value -> [Detail{typeUrl = typePrefix <> raw.detailType, value}]
    Left _ -> []

endStream :: Status -> [Header] -> EndStream
endStream status trailers =
  EndStream
    { endError = if status.code == Ok then Nothing else Just (errorJson status)
    , metadata = if null trailers then Nothing else Just (Map.fromListWith (flip (<>)) (fmap entry trailers))
    }
 where
  entry (name, value) = (TE.decodeUtf8Lenient name, [TE.decodeUtf8Lenient value])

encodeJson :: (ToJSON a) => a -> BS.ByteString
encodeJson = LBS.toStrict . encode

decodeErrorJson :: BS.ByteString -> Either T.Text ErrorJson
decodeErrorJson = first T.pack . eitherDecodeStrict

decodeEndStream :: BS.ByteString -> Either T.Text EndStream
decodeEndStream = first T.pack . eitherDecodeStrict

trailerPrefix :: BS.ByteString
trailerPrefix = "trailer-"
