module Peculiar.Rpc.Json.Plan
  ( Plan (..)
  , Slot (..)
  , Kind (..)
  , Names (..)
  , planOf
  , slotAt
  , jsonNameOf
  ) where

import Data.Char (toUpper)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.ProtoLens (FieldDescriptor (..), FieldTypeDescriptor (..), Message (..), ScalarField, decodeMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Message (FieldAccessor, Tag (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Lens.Family2 ((^.))
import Proto.Google.Protobuf.Descriptor (DescriptorProto, FieldDescriptorProto)

data Names = Names
  { json :: T.Text
  , proto :: T.Text
  , tag :: Int
  , oneof :: Maybe T.Text
  , nullValue :: Bool
  }

data Kind v where
  Scalar :: ScalarField v -> Kind v
  Nested :: (Message v) => Plan v -> Kind v

data Slot msg = forall v. Slot Names (Kind v) (FieldAccessor msg v)

data Plan msg = Plan
  { name :: T.Text
  , slots :: [Slot msg]
  , byName :: Map.Map T.Text (Slot msg)
  }

planOf :: forall msg. (Message msg) => Plan msg
planOf = Plan{name = messageName (Proxy @msg), slots, byName}
 where
  descriptor = case decodeMessage @DescriptorProto (packedMessageDescriptor (Proxy @msg)) of
    Right described -> Just described
    Left _ -> Nothing
  declared = Map.fromList [(fromIntegral (field ^. #number) :: Int, field) | Just described <- [descriptor], field <- described ^. #field]
  oneofNames = [oneof ^. #name | Just described <- [descriptor], oneof <- described ^. #oneofDecl]
  slots = [slotFor (unTag tag) field | (tag, field) <- Map.toList (fieldsByTag @msg)]
  byName = Map.fromList (concat [[(names.json, slot), (names.proto, slot)] | slot@(Slot names _ _) <- slots])
  slotFor number (FieldDescriptor protoName kind accessor) =
    Slot (namesFor number (T.pack protoName) (Map.lookup number declared)) (kindOf kind) accessor
  namesFor number protoName declaration =
    Names
      { json = fromMaybe (jsonNameOf protoName) (nonEmpty . (^. #jsonName) =<< declaration)
      , proto = protoName
      , tag = number
      , oneof = oneofOf =<< declaration
      , nullValue = any ((== ".google.protobuf.NullValue") . (^. #typeName)) declaration
      }
  oneofOf :: FieldDescriptorProto -> Maybe T.Text
  oneofOf declaration = case declaration ^. #maybe'oneofIndex of
    Just (index :: Int32) -> lookupIndex (fromIntegral index) oneofNames
    Nothing -> Nothing
  nonEmpty text = if T.null text then Nothing else Just text

kindOf :: FieldTypeDescriptor v -> Kind v
kindOf = \case
  ScalarField scalar -> Scalar scalar
  MessageField _ -> Nested planOf

lookupIndex :: Int -> [a] -> Maybe a
lookupIndex index = \case
  [] -> Nothing
  item : rest -> if index == 0 then Just item else lookupIndex (index - 1) rest

slotAt :: Int -> Plan msg -> Maybe (Slot msg)
slotAt number plan = case [slot | slot@(Slot names _ _) <- plan.slots, names.tag == number] of
  slot : _ -> Just slot
  [] -> Nothing

jsonNameOf :: T.Text -> T.Text
jsonNameOf = T.pack . go False . T.unpack
 where
  go upper = \case
    [] -> []
    '_' : rest -> go True rest
    c : rest -> (if upper then toUpper c else c) : go False rest
