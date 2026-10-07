{-# LANGUAGE RankNTypes #-}

module Peculiar.Rpc.Intercept
  ( Route (..)
  , routeOf
  , Interceptor (..)
  ) where

import Peculiar.Rpc.Context (Context)
import Peculiar.Rpc.Route (Route (..), routeOf)

newtype Interceptor = Interceptor (forall a. Route -> Context -> (Context -> IO a) -> IO a)

instance Semigroup Interceptor where
  Interceptor outer <> Interceptor inner =
    Interceptor \route context continue -> outer route context \context' -> inner route context' continue

instance Monoid Interceptor where
  mempty = Interceptor \_ context continue -> continue context
