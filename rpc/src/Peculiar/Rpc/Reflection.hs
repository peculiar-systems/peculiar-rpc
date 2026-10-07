module Peculiar.Rpc.Reflection
  ( withReflection
  , withReflectionFrom
  ) where

import Data.ByteString qualified as BS
import Peculiar.Rpc.Reflection.Current (current)
import Peculiar.Rpc.Reflection.Legacy (legacy)
import Peculiar.Rpc.Reflection.Registry (registryWith)
import Peculiar.Rpc.Service (Endpoint)

withReflection :: [Endpoint] -> [Endpoint]
withReflection = withReflectionFrom []

withReflectionFrom :: [BS.ByteString] -> [Endpoint] -> [Endpoint]
withReflectionFrom sets endpoints = served
 where
  served = endpoints <> [current known, legacy known]
  known = registryWith sets served
