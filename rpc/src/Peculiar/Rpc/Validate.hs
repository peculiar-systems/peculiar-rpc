{-# LANGUAGE AllowAmbiguousTypes #-}

module Peculiar.Rpc.Validate
  ( Validator
  , Violation (..)
  , Failure (..)
  , validatorOf
  , violations
  ) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.ProtoLens (FieldDescriptor (..), FieldTypeDescriptor (..), Message (..), MessageEnum (..), ScalarField (..), decodeMessage)
import Data.ProtoLens.Encoding.Wire (TaggedValue (..))
import Data.ProtoLens.Encoding.Wire qualified as Wire
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Message (Tag (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Lens.Family2 ((^.))
import Peculiar.Rpc.Validate.Dynamic qualified as Dynamic
import Peculiar.Rpc.Validate.Engine (Engine, Failure (..), engineOf, renderPath)
import Peculiar.Rpc.Validate.Engine qualified as Engine
import Peculiar.Rpc.Validate.Schema (EnumType (..), Field (..), MessageType (..), Oneof (..), Schema (..), isMessageKind, schemaOf)
import Proto.Buf.Validate.Validate (FieldRules)
import Proto.Google.Protobuf.Descriptor (DescriptorProto, FileDescriptorProto)

data Violation = Violation
  { field :: T.Text
  , rule :: T.Text
  , description :: T.Text
  }
  deriving stock (Eq, Show)

data Validator msg = Validator
  { engine :: Engine
  , schema :: Schema
  , described :: Maybe MessageType
  }

data Some = forall message. (Message message) => Some (Proxy message)

validatorOf :: forall msg. (Message msg) => Validator msg
validatorOf =
  Validator
    { engine = engineOf schema
    , schema
    , described = if ruled schema root then Map.lookup root schema.messages else Nothing
    }
 where
  root = messageName (Proxy @msg)
  (files, enums) = collected [Some (Proxy @msg), Some (Proxy @FieldRules)]
  declared = schemaOf files
  schema = declared{enums = Map.union declared.enums enums}

violations :: Validator msg -> Integer -> BS.ByteString -> Either Failure [Violation]
violations validator now bytes = case validator.described of
  Nothing -> Right []
  Just described -> case Dynamic.decodeMessage validator.schema described bytes of
    Left failure -> Left (RuntimeError (T.pack (show failure)))
    Right message -> fmap reported <$> Engine.validate validator.engine now message
 where
  reported violation =
    Violation
      { field = renderPath violation.field
      , rule = violation.ruleId
      , description = violation.message
      }

collected :: [Some] -> ([FileDescriptorProto], Map.Map T.Text EnumType)
collected = go Set.empty Map.empty Map.empty
 where
  go _ files enums [] = (Map.elems files, enums)
  go seen files enums (Some (proxy :: Proxy message) : rest)
    | messageName proxy `Set.member` seen = go seen files enums rest
    | otherwise =
        let files' = case decodeMessage @FileDescriptorProto (packedFileDescriptor proxy) of
              Right file -> Map.insert (file ^. #name) file files
              Left _ -> files
            enums' = Map.union enums (enumsOf @message)
         in go (Set.insert (messageName proxy) seen) files' enums' (nestedOf @message <> rest)

nestedOf :: forall message. (Message message) => [Some]
nestedOf = concatMap fromField (Map.elems (fieldsByTag @message))
 where
  fromField (FieldDescriptor _ typed _) = case typed of
    MessageField _ -> [Some (proxyOf typed)]
    ScalarField _ -> []

proxyOf :: FieldTypeDescriptor value -> Proxy value
proxyOf _ = Proxy

enumsOf :: forall message. (Message message) => Map.Map T.Text EnumType
enumsOf =
  Map.fromList
    [ (name, EnumType{fullName = name, values, closed = False})
    | (Tag number, FieldDescriptor _ typed _) <- Map.toList (fieldsByTag @message)
    , Just values <- [valuesOf typed]
    , Just name <- [Map.lookup number typeNames]
    ]
 where
  typeNames = case decodeMessage @DescriptorProto (packedMessageDescriptor (Proxy @message)) of
    Right described -> Map.fromList [(fromIntegral (declaration ^. #number), T.dropWhile (== '.') (declaration ^. #typeName)) | declaration <- described ^. #field]
    Left _ -> Map.empty

valuesOf :: FieldTypeDescriptor value -> Maybe [(T.Text, Int32)]
valuesOf = \case
  typed@(ScalarField EnumField) -> Just (enumerated (proxyOf typed))
  _ -> Nothing

enumerated :: forall value. (MessageEnum value) => Proxy value -> [(T.Text, Int32)]
enumerated _ = [(T.pack (showEnum value), fromIntegral (fromEnum value)) | value <- [minBound .. maxBound :: value]]

ruled :: Schema -> T.Text -> Bool
ruled schema root = go Set.empty [root]
 where
  go _ [] = False
  go seen (name : rest)
    | name `Set.member` seen = go seen rest
    | otherwise = case Map.lookup name schema.messages of
        Nothing -> go (Set.insert name seen) rest
        Just described
          | carries described.options || any (\oneof -> carries oneof.options) described.oneofs || any (\field -> carries field.options) described.fields -> True
          | otherwise -> go (Set.insert name seen) ([field.typeName | field <- described.fields, isMessageKind field.kind] <> rest)
  carries options = not (null [() | TaggedValue (Wire.Tag 1159) _ <- options])
