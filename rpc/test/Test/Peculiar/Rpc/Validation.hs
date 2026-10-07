module Test.Peculiar.Rpc.Validation (tests) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ProtoLens (decodeMessage, defMessage)
import Data.ProtoLens.Labels ()
import Data.String (fromString)
import Data.Text qualified as T
import Hedgehog (Group (..), Property, annotateShow, evalEither, failure, property, withTests, (===))
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Proto.Google.Rpc.ErrorDetails (BadRequest)
import Proto.Peculiar.Test.V1.Library (Book, CreateBookRequest)
import Test.Peculiar.Rpc.Library (LibraryServer (..), library, withLibrary)
import Test.Peculiar.Rpc.Library qualified as Client (LibraryClient (..))

tests :: Int -> Group
tests port =
  Group "Validation" $
    [ (name, withTests 1 (refused port protocol request field rule))
    | protocol <- [minBound ..]
    , (case', request, field, rule) <- cases
    , let name = fromString (show protocol <> ": " <> T.unpack case')
    ]
      <> [ (fromString (show protocol <> ": a CEL rule erroring at runtime"), withTests 1 (unrunnable protocol))
         | protocol <- [minBound ..]
         ]

cases :: [(T.Text, CreateBookRequest, T.Text, T.Text)]
cases =
  [ ("a string breaking its pattern", creating (titled & #isbn .~ "not-an-isbn"), "book.isbn", "string.pattern")
  , ("a message breaking its CEL rule", creating (titled & #pages .~ 1 & #tags .~ ["one", "two"]), "book", "book.tags_within_pages")
  , ("a timestamp not after now", creating (titled & #due .~ (defMessage & #seconds .~ 978307200)), "book.due", "timestamp.gt_now")
  ]

titled :: Book
titled = defMessage & #title .~ "Odes"

creating :: Book -> CreateBookRequest
creating book = defMessage & #shelf .~ "poetry" & #book .~ book

answer :: Rpc.Protocol -> CreateBookRequest -> Int -> IO (Either Rpc.RpcError Book)
answer protocol request port =
  Rpc.withConnection Rpc.Plaintext "127.0.0.1" port \connection ->
    try @Rpc.RpcError ((Rpc.client Rpc.defaultOptions (Rpc.Http2 connection protocol) :: Client.LibraryClient).createBook request)

refused :: Int -> Rpc.Protocol -> CreateBookRequest -> T.Text -> T.Text -> Property
refused port protocol request field rule = property do
  outcome <- liftIO (answer protocol request port)
  case outcome of
    Right answered -> annotateShow answered >> failure
    Left failed -> do
      failed.status.code === Rpc.InvalidArgument
      case badRequests failed of
        [bytes] -> do
          badRequest <- evalEither (decodeMessage @BadRequest bytes)
          [(violation ^. #field, violation ^. #reason) | violation <- badRequest ^. #fieldViolations] === [(field, rule)]
        other -> annotateShow (length other) >> failure

unrunnable :: Rpc.Protocol -> Property
unrunnable protocol = property do
  outcome <- liftIO (withLibrary unreachable (answer protocol (creating titled & #shelf .~ "vault")))
  case outcome of
    Right answered -> annotateShow answered >> failure
    Left failed ->
      (failed.status.code, failed.status.message, length (badRequests failed))
        === (Rpc.InvalidArgument, "runtime error: error evaluating create_book.vault_divisor: divide by zero", 0)
 where
  unreachable = library{createBook = \_ _ -> Rpc.throwRpc Rpc.Internal "the handler ran"}

badRequests :: Rpc.RpcError -> [BS.ByteString]
badRequests failed = [detail.value | detail <- failed.status.details, detail.typeUrl == "type.googleapis.com/google.rpc.BadRequest"]
