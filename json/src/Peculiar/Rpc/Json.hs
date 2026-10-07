module Peculiar.Rpc.Json
  ( Codec (..)
  , codec
  , Parsing (..)
  , defaultParsing
  , toJson
  , fromJson
  , encodeJson
  , decodeJson
  , decodeValue
  , Registry
  , register
  , wellKnown
  , JsonError (..)
  , Problem (..)
  , render
  ) where

import Control.Monad ((<=<))
import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ProtoLens (Message)
import Data.ProtoLens.Message (Registry, register)
import Data.Proxy (Proxy (..))
import Peculiar.Rpc.Json.Decode (Parsing (..), decodeWith)
import Peculiar.Rpc.Json.Encode (encodeWith)
import Peculiar.Rpc.Json.Error (JsonError (..), Problem (..), render)
import Peculiar.Rpc.Json.Parse (parseJson)
import Peculiar.Rpc.Json.Plan (Plan, planOf)
import Proto.Google.Protobuf.Any (Any)
import Proto.Google.Protobuf.Duration (Duration)
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Google.Protobuf.FieldMask (FieldMask)
import Proto.Google.Protobuf.Struct (ListValue, Struct)
import Proto.Google.Protobuf.Struct qualified as Struct
import Proto.Google.Protobuf.Timestamp (Timestamp)
import Proto.Google.Protobuf.Wrappers (BoolValue, BytesValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, UInt32Value, UInt64Value)

data Codec a = Codec
  { toValue :: a -> Either JsonError Value
  , fromValue :: Value -> Either JsonError a
  , encoded :: a -> Either JsonError BS.ByteString
  , decoded :: BS.ByteString -> Either JsonError a
  }

codec :: forall a. (Message a) => Parsing -> Codec a
codec parsing =
  Codec
    { toValue = encodeWith parsing.registry plan
    , fromValue = decodeWith parsing plan
    , encoded = fmap (LBS.toStrict . encode) . encodeWith parsing.registry plan
    , decoded = decodeWith parsing plan <=< parseJson
    }
 where
  plan :: Plan a
  plan = planOf

defaultParsing :: Registry -> Parsing
defaultParsing registry = Parsing{registry, ignoreUnknown = False}

toJson :: (Message a) => Registry -> a -> Either JsonError Value
toJson registry = (codec (defaultParsing registry)).toValue

fromJson :: (Message a) => Parsing -> Value -> Either JsonError a
fromJson parsing = (codec parsing).fromValue

encodeJson :: (Message a) => Registry -> a -> Either JsonError BS.ByteString
encodeJson registry = (codec (defaultParsing registry)).encoded

decodeJson :: (Message a) => Parsing -> BS.ByteString -> Either JsonError a
decodeJson parsing = (codec parsing).decoded

decodeValue :: BS.ByteString -> Either JsonError Value
decodeValue = parseJson

wellKnown :: Registry
wellKnown =
  mconcat
    [ register (Proxy @Any)
    , register (Proxy @Duration)
    , register (Proxy @Empty)
    , register (Proxy @FieldMask)
    , register (Proxy @ListValue)
    , register (Proxy @Struct)
    , register (Proxy @Timestamp)
    , register (Proxy @Struct.Value)
    , register (Proxy @BoolValue)
    , register (Proxy @BytesValue)
    , register (Proxy @DoubleValue)
    , register (Proxy @FloatValue)
    , register (Proxy @Int32Value)
    , register (Proxy @Int64Value)
    , register (Proxy @StringValue)
    , register (Proxy @UInt32Value)
    , register (Proxy @UInt64Value)
    ]
