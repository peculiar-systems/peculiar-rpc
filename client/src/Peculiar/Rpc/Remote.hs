module Peculiar.Rpc.Remote
  ( Client (..)
  ) where

import Data.Kind (Type)
import Peculiar.Rpc.Calling (Options, Transport)

class Client client where
  type Consuming client :: Type
  client :: Options -> Transport -> client
