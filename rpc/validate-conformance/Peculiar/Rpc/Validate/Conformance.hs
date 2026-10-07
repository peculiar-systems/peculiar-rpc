module Peculiar.Rpc.Validate.Conformance
  ( runTestee
  , respond
  , TesteeError (..)
  ) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.ProtoLens (MessageEnum (..), decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc.Validate.Dynamic qualified as Dynamic
import Peculiar.Rpc.Validate.Engine (Engine, Failure (..), PathElement (..), Subscript (..), Violation (..), engineOf, validate)
import Peculiar.Rpc.Validate.Schema (Kind, Schema (..), kindNumber, schemaOf)
import Proto.Buf.Validate.Conformance.Harness.Harness (TestConformanceRequest, TestConformanceResponse, TestResult)
import Proto.Buf.Validate.Validate qualified as Validate
import Proto.Google.Protobuf.Any (Any)
import System.IO (hPutStrLn, stderr)

runTestee :: IO ()
runTestee = do
  input <- BS.getContents
  elapsed <- getPOSIXTime
  case respond (floor (toRational (nominalDiffTimeToSeconds elapsed) * 1000000000)) input of
    Left (UndecodableRequest reason) -> hPutStrLn stderr ("failed to decode the conformance request: " <> T.unpack reason)
    Right output -> BS.putStr output

newtype TesteeError = UndecodableRequest T.Text
  deriving stock (Eq, Show)

respond :: Integer -> BS.ByteString -> Either TesteeError BS.ByteString
respond now input = case decodeMessage @TestConformanceRequest input of
  Left failure -> Left (UndecodableRequest (T.pack failure))
  Right request ->
    let schema = schemaOf (request ^. #fdset . #file)
        engine = engineOf schema
     in Right (encodeMessage (defMessage & #results .~ fmap (tested schema engine now) (request ^. #cases) :: TestConformanceResponse))

tested :: Schema -> Engine -> Integer -> Any -> TestResult
tested schema engine now packed = case Map.lookup (Dynamic.typeUrlName (packed ^. #typeUrl)) schema.messages of
  Nothing -> defMessage & #unexpectedError .~ "unable to find descriptor: " <> packed ^. #typeUrl
  Just described -> case Dynamic.decodeMessage schema described (packed ^. #value) of
    Left failure -> defMessage & #unexpectedError .~ "unable to unmarshal test case: " <> T.pack (show failure)
    Right message -> case validate engine now message of
      Right [] -> defMessage & #success .~ True
      Right found -> defMessage & #validationError .~ (defMessage & #violations .~ fmap violationProto found)
      Left (CompilationError reason) -> defMessage & #compilationError .~ "compilation error: " <> reason
      Left (RuntimeError reason) -> defMessage & #runtimeError .~ "runtime error: " <> reason

violationProto :: Violation -> Validate.Violation
violationProto violation =
  defMessage
    & #maybe'field .~ pathProto violation.field
    & #maybe'rule .~ pathProto violation.rule
    & #ruleId .~ violation.ruleId
    & #message .~ violation.message
    & #maybe'forKey .~ (if violation.forKey then Just True else Nothing)

pathProto :: [PathElement] -> Maybe Validate.FieldPath
pathProto = \case
  [] -> Nothing
  elements -> Just (defMessage & #elements .~ fmap elementProto elements)

elementProto :: PathElement -> Validate.FieldPathElement
elementProto element =
  defMessage
    & #maybe'fieldNumber .~ element.number
    & #fieldName .~ element.name
    & #maybe'fieldType .~ (kindProto =<< element.fieldType)
    & #maybe'keyType .~ (kindProto =<< element.keyType)
    & #maybe'valueType .~ (kindProto =<< element.valueType)
    & #maybe'subscript .~ fmap subscriptProto element.subscript

kindProto :: (MessageEnum kind) => Kind -> Maybe kind
kindProto = maybeToEnum . kindNumber

subscriptProto :: Subscript -> Validate.FieldPathElement'Subscript
subscriptProto = \case
  IndexAt position -> Validate.FieldPathElement'Index position
  BoolKey flag -> Validate.FieldPathElement'BoolKey flag
  IntKey number -> Validate.FieldPathElement'IntKey number
  UintKey number -> Validate.FieldPathElement'UintKey number
  StringKey text -> Validate.FieldPathElement'StringKey text
