{-# LANGUAGE TemplateHaskellQuotes #-}

module Peculiar.Rpc.Derive
  ( deriveService
  , deriveServer
  , deriveClient
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.ProtoLens (Message)
import Data.Proxy (Proxy (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName (..), Module (..), PkgName (..))
import Peculiar.Rpc.Derive.Client (deriveClient, methodsOf)
import Peculiar.Rpc.Descriptor (SomeMessage (..))
import Peculiar.Rpc.Handler (Handler, handler)
import Peculiar.Rpc.Service (Implementation (..))

deriveService :: Name -> Q [Dec]
deriveService service = (<>) <$> deriveServer service <*> deriveClient service

deriveServer :: Name -> Q [Dec]
deriveServer service = do
  methods <- methodsOf service
  related <- messagesNear service
  let serverName = mkName (nameBase service <> "Server")
      field method = (mkName method, Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Handler `AppT` ConT service `AppT` LitT (StrTyLit method))
      proxied method = [SigE (ConE 'Proxy) (ConT ''Proxy `AppT` ConT service), SigE (ConE 'Proxy) (ConT ''Proxy `AppT` LitT (StrTyLit method))]
  bound <- traverse (newName . fmap toLower) methods
  let serverRecord = DataD [] serverName [] Nothing [RecC serverName (fmap field methods)] []
      serverInstance =
        InstanceD
          Nothing
          []
          (ConT ''Implementation `AppT` ConT serverName)
          [ TySynInstD (TySynEqn Nothing (ConT ''Implementing `AppT` ConT serverName) (ConT service))
          , FunD
              'handlers
              [ Clause
                  [ConP serverName [] (fmap VarP bound)]
                  (NormalB (ListE [foldl' AppE (VarE 'handler) (proxied method <> [VarE name]) | (method, name) <- zip methods bound]))
                  []
              ]
          , FunD
              'messages
              [ Clause
                  [WildP]
                  (NormalB (ListE [ConE 'SomeMessage `AppE` SigE (ConE 'Proxy) (ConT ''Proxy `AppT` ConT message) | message <- related]))
                  []
              ]
          ]
  pure [serverRecord, serverInstance]

messagesNear :: Name -> Q [Name]
messagesNear service = case (namePackage service, nameModule service) of
  (Just package, Just home) -> do
    modules <- closure [Module (PkgName package) (ModName home)] []
    info <- reify ''Message
    pure case info of
      ClassI _ instances ->
        [ name
        | InstanceD _ _ (AppT (ConT _) (ConT name)) _ <- instances
        , Just owner <- [nameModule name]
        , owner `elem` fmap moduleName modules
        ]
      _ -> []
  _ -> pure []
 where
  moduleName (Module _ (ModName name)) = name
  closure [] seen = pure seen
  closure (next : rest) seen
    | moduleName next `elem` fmap moduleName seen = closure rest seen
    | otherwise = do
        ModuleInfo imports <- reifyModule next
        let generated = [imported | imported <- imports, "Proto." `isPrefixOf` moduleName imported]
        closure (rest <> generated) (next : seen)
