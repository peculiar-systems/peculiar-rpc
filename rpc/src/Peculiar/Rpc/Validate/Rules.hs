module Peculiar.Rpc.Validate.Rules
  ( Rules (..)
  , Kinded (..)
  , Numbers (..)
  , Bound (..)
  , Strings (..)
  , Format (..)
  , Sizes (..)
  , Bytes (..)
  , Ignoring (..)
  , rulesOf
  , noRules
  ) where

import Data.ByteString qualified as BS
import Data.Foldable (asum)
import Data.ProtoLens (unknownFields)
import Data.ProtoLens.Field (HasField)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Word (Word64)
import Lens.Family2 ((^.))
import Peculiar.Rpc.Catalog (extension)
import Proto.Buf.Validate.Validate
import Proto.Google.Protobuf.Descriptor (FieldDescriptorProto)

data Ignoring = Never | IfZero | Always
  deriving stock (Eq, Show)

data Bound = Inclusive Rational | Exclusive Rational
  deriving stock (Eq, Show)

data Numbers = Numbers
  { exact :: Maybe Rational
  , below :: Maybe Bound
  , above :: Maybe Bound
  , among :: [Rational]
  , outside :: [Rational]
  , finite :: Bool
  }
  deriving stock (Eq, Show)

data Format = Email | Uuid | Uri | UriReference | Hostname | Ip | Ipv4 | Ipv6
  deriving stock (Eq, Show, Enum, Bounded)

data Strings = Strings
  { exact :: Maybe T.Text
  , lengths :: Sizes
  , byteLengths :: Sizes
  , prefix :: Maybe T.Text
  , suffix :: Maybe T.Text
  , contains :: Maybe T.Text
  , excludes :: Maybe T.Text
  , among :: [T.Text]
  , outside :: [T.Text]
  , format :: Maybe Format
  }
  deriving stock (Eq, Show)

data Sizes = Sizes
  { exactly :: Maybe Word64
  , atLeast :: Maybe Word64
  , atMost :: Maybe Word64
  }
  deriving stock (Eq, Show)

data Bytes = Bytes
  { exact :: Maybe BS.ByteString
  , sizes :: Sizes
  , prefix :: Maybe BS.ByteString
  , suffix :: Maybe BS.ByteString
  , contains :: Maybe BS.ByteString
  , among :: [BS.ByteString]
  , outside :: [BS.ByteString]
  }
  deriving stock (Eq, Show)

data Kinded
  = Numeric Numbers
  | Text Strings
  | Binary Bytes
  | Boolean (Maybe Bool)
  | Enumerated Bool (Maybe Integer) [Integer] [Integer]
  | Repeated Sizes Bool (Maybe Rules)
  | Keyed Sizes (Maybe Rules) (Maybe Rules)
  | Moment Numbers
  | Span Numbers
  deriving stock (Eq, Show)

data Rules = Rules
  { required :: Bool
  , ignoring :: Ignoring
  , kinded :: Maybe Kinded
  }
  deriving stock (Eq, Show)

noRules :: Rules
noRules = Rules{required = False, ignoring = Never, kinded = Nothing}

rulesOf :: FieldDescriptorProto -> Rules
rulesOf field = maybe noRules fromFieldRules (extension @FieldRules 1159 (field ^. #options . unknownFields))

fromFieldRules :: FieldRules -> Rules
fromFieldRules rules =
  Rules
    { required = rules ^. #required
    , ignoring = case rules ^. #ignore of
        IGNORE_IF_ZERO_VALUE -> IfZero
        IGNORE_ALWAYS -> Always
        _ -> Never
    , kinded = kindedOf =<< rules ^. #maybe'type'
    }

kindedOf :: FieldRules'Type -> Maybe Kinded
kindedOf = \case
  FieldRules'Float rules -> Just (Numeric (numbers rules){finite = rules ^. #finite})
  FieldRules'Double rules -> Just (Numeric (numbers rules){finite = rules ^. #finite})
  FieldRules'Int32 rules -> Just (Numeric (numbers rules))
  FieldRules'Int64 rules -> Just (Numeric (numbers rules))
  FieldRules'Uint32 rules -> Just (Numeric (numbers rules))
  FieldRules'Uint64 rules -> Just (Numeric (numbers rules))
  FieldRules'Sint32 rules -> Just (Numeric (numbers rules))
  FieldRules'Sint64 rules -> Just (Numeric (numbers rules))
  FieldRules'Fixed32 rules -> Just (Numeric (numbers rules))
  FieldRules'Fixed64 rules -> Just (Numeric (numbers rules))
  FieldRules'Sfixed32 rules -> Just (Numeric (numbers rules))
  FieldRules'Sfixed64 rules -> Just (Numeric (numbers rules))
  FieldRules'Bool rules -> Just (Boolean (rules ^. #maybe'const))
  FieldRules'String rules -> Just (Text (strings rules))
  FieldRules'Bytes rules ->
    Just
      ( Binary
          Bytes
            { exact = rules ^. #maybe'const
            , sizes = Sizes{exactly = rules ^. #maybe'len, atLeast = rules ^. #maybe'minLen, atMost = rules ^. #maybe'maxLen}
            , prefix = rules ^. #maybe'prefix
            , suffix = rules ^. #maybe'suffix
            , contains = rules ^. #maybe'contains
            , among = rules ^. #in'
            , outside = rules ^. #notIn
            }
      )
  FieldRules'Enum rules ->
    Just (Enumerated (rules ^. #definedOnly) (toInteger <$> rules ^. #maybe'const) (toInteger <$> rules ^. #in') (toInteger <$> rules ^. #notIn))
  FieldRules'Repeated rules ->
    Just
      ( Repeated
          Sizes{exactly = Nothing, atLeast = rules ^. #maybe'minItems, atMost = rules ^. #maybe'maxItems}
          (rules ^. #unique)
          (fromFieldRules <$> rules ^. #maybe'items)
      )
  FieldRules'Map rules ->
    Just
      ( Keyed
          Sizes{exactly = Nothing, atLeast = rules ^. #maybe'minPairs, atMost = rules ^. #maybe'maxPairs}
          (fromFieldRules <$> rules ^. #maybe'keys)
          (fromFieldRules <$> rules ^. #maybe'values)
      )
  FieldRules'Timestamp rules -> Just (Moment (seconds rules))
  FieldRules'Duration rules -> Just (Span (seconds rules))
  _ -> Nothing

numbers
  :: ( Real a
     , HasField rules "maybe'const" (Maybe a)
     , HasField rules "maybe'lt" (Maybe a)
     , HasField rules "maybe'lte" (Maybe a)
     , HasField rules "maybe'gt" (Maybe a)
     , HasField rules "maybe'gte" (Maybe a)
     , HasField rules "in'" [a]
     , HasField rules "notIn" [a]
     )
  => rules
  -> Numbers
numbers rules =
  Numbers
    { exact = toRational <$> rules ^. #maybe'const
    , below = asum [Exclusive . toRational <$> rules ^. #maybe'lt, Inclusive . toRational <$> rules ^. #maybe'lte]
    , above = asum [Exclusive . toRational <$> rules ^. #maybe'gt, Inclusive . toRational <$> rules ^. #maybe'gte]
    , among = toRational <$> rules ^. #in'
    , outside = toRational <$> rules ^. #notIn
    , finite = False
    }

seconds
  :: ( HasField rules "maybe'lt" (Maybe moment)
     , HasField rules "maybe'lte" (Maybe moment)
     , HasField rules "maybe'gt" (Maybe moment)
     , HasField rules "maybe'gte" (Maybe moment)
     , HasField rules "maybe'const" (Maybe moment)
     , HasField moment "seconds" a
     , HasField moment "nanos" b
     , Integral a
     , Integral b
     )
  => rules
  -> Numbers
seconds rules =
  Numbers
    { exact = instant <$> rules ^. #maybe'const
    , below = asum [Exclusive . instant <$> rules ^. #maybe'lt, Inclusive . instant <$> rules ^. #maybe'lte]
    , above = asum [Exclusive . instant <$> rules ^. #maybe'gt, Inclusive . instant <$> rules ^. #maybe'gte]
    , among = []
    , outside = []
    , finite = False
    }
 where
  instant moment = toRational (moment ^. #seconds) + toRational (moment ^. #nanos) / 1000000000

strings :: StringRules -> Strings
strings rules =
  Strings
    { exact = rules ^. #maybe'const
    , lengths = Sizes{exactly = rules ^. #maybe'len, atLeast = rules ^. #maybe'minLen, atMost = rules ^. #maybe'maxLen}
    , byteLengths = Sizes{exactly = rules ^. #maybe'lenBytes, atLeast = rules ^. #maybe'minBytes, atMost = rules ^. #maybe'maxBytes}
    , prefix = rules ^. #maybe'prefix
    , suffix = rules ^. #maybe'suffix
    , contains = rules ^. #maybe'contains
    , excludes = rules ^. #maybe'notContains
    , among = rules ^. #in'
    , outside = rules ^. #notIn
    , format = asum [flag Email #maybe'email, flag Uuid #maybe'uuid, flag Uri #maybe'uri, flag UriReference #maybe'uriRef, flag Hostname #maybe'hostname, flag Ip #maybe'ip, flag Ipv4 #maybe'ipv4, flag Ipv6 #maybe'ipv6]
    }
 where
  flag format lens = if rules ^. lens == Just True then Just format else Nothing
