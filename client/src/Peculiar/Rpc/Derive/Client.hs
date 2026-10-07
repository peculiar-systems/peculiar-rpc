{-# LANGUAGE TemplateHaskellQuotes #-}

module Peculiar.Rpc.Derive.Client
  ( deriveClient
  , methodsOf
  ) where

import Data.ProtoLens.Service.Types (ServiceMethods)
import Data.Proxy (Proxy (..))
import Language.Haskell.TH
import Peculiar.Rpc.Calling (Calling, calling)
import Peculiar.Rpc.Remote (Client (..))

deriveClient :: Name -> Q [Dec]
deriveClient service = do
  methods <- methodsOf service
  options <- newName "options"
  connection <- newName "transport"
  let clientName = mkName (nameBase service <> "Client")
      field method = (mkName method, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Calling `AppT` ConT service `AppT` LitT (StrTyLit method))
      proxied method = [SigE (ConE 'Proxy) (ConT ''Proxy `AppT` ConT service), SigE (ConE 'Proxy) (ConT ''Proxy `AppT` LitT (StrTyLit method))]
      clientRecord = DataD [] clientName [] Nothing [RecC clientName (fmap field methods)] []
      clientInstance =
        InstanceD
          Nothing
          []
          (ConT ''Client `AppT` ConT clientName)
          [ TySynInstD (TySynEqn Nothing (ConT ''Consuming `AppT` ConT clientName) (ConT service))
          , FunD
              'client
              [ Clause
                  [VarP options, VarP connection]
                  (NormalB (foldl' AppE (ConE clientName) [foldl' AppE (VarE 'calling) (proxied method <> [VarE options, VarE connection]) | method <- methods]))
                  []
              ]
          ]
  pure [clientRecord, clientInstance]

methodsOf :: Name -> Q [String]
methodsOf service = do
  info <- reify ''ServiceMethods
  instances <- case info of
    FamilyI _ found -> pure found
    _ -> fail "ServiceMethods is not a type family"
  case [rhs | TySynInstD (TySynEqn _ lhs rhs) <- instances, applies lhs] of
    [list] -> maybe (fail ("cannot read the methods of " <> show service)) pure (symbols list)
    _ -> fail (show service <> " is not a proto-lens service")
 where
  applies = \case
    AppT (ConT family) (ConT name) -> family == ''ServiceMethods && name == service
    AppT (ConT family) (SigT (ConT name) _) -> family == ''ServiceMethods && name == service
    _ -> False

symbols :: Type -> Maybe [String]
symbols = \case
  SigT inner _ -> symbols inner
  PromotedNilT -> Just []
  AppT (AppT PromotedConsT (LitT (StrTyLit name))) rest -> (name :) <$> symbols rest
  AppT (AppT PromotedConsT (SigT (LitT (StrTyLit name)) _)) rest -> (name :) <$> symbols rest
  AppT (AppT (AppT PromotedConsT _) (LitT (StrTyLit name))) rest -> (name :) <$> symbols rest
  _ -> Nothing
