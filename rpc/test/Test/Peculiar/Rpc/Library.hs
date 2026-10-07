{-# LANGUAGE TemplateHaskell #-}

module Test.Peculiar.Rpc.Library
  ( LibraryServer (..)
  , LibraryClient (..)
  , library
  , withLibrary
  ) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Peculiar.Rpc.Derive (deriveService)
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Peculiar.Test.V1.Library

deriveService ''Library

library :: LibraryServer
library =
  LibraryServer
    { getBook = \_ request -> pure (book (request ^. #name) ("Title of " <> request ^. #name))
    , listBooks = \_ request ->
        pure $
          defMessage
            & #books
              .~ [ book (request ^. #shelf <> "/" <> tag) tag & #pages .~ fromIntegral (request ^. #pageSize) & #available .~ request ^. #availableOnly
                 | tag <- request ^. #tags
                 ]
    , createBook = \_ request -> pure (request ^. #book & #name .~ "shelves/" <> request ^. #shelf <> "/books/1")
    , renameBook = \_ request -> pure (book (request ^. #name) (request ^. #title))
    , deleteBook = \_ request ->
        if
          | "missing" `T.isSuffixOf` (request ^. #name) -> Rpc.throwRpc Rpc.NotFound ("no book " <> request ^. #name)
          | "busy" `T.isSuffixOf` (request ^. #name) -> throwIO (Rpc.withMetadata (Rpc.pushback (Rpc.milliseconds 1500)) (Rpc.rpcError Rpc.Unavailable "try later"))
          | otherwise -> pure (defMessage :: Empty)
    }
 where
  book name title = defMessage & #name .~ name & #title .~ title :: Book

withLibrary :: LibraryServer -> (Int -> IO a) -> IO a
withLibrary server use = do
  bound <- newEmptyMVar
  let settings =
        Rpc.defaultSettings
          { Rpc.port = 0
          , Rpc.onListening = putMVar bound
          , Rpc.calls = Rpc.defaultCalls{Rpc.report = const (pure ()), Rpc.openApi = Just Rpc.Info{title = "Library", version = "1.0.0", description = Just "Books on shelves."}}
          }
  withAsync (Rpc.serveEndpoints settings [Rpc.endpoint server]) \_ -> takeMVar bound >>= use
