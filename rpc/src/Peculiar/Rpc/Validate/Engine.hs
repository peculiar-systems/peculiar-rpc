module Peculiar.Rpc.Validate.Engine
  ( Engine
  , Failure (..)
  , Violation (..)
  , PathElement (..)
  , Subscript (..)
  , engineOf
  , validate
  , renderPath
  ) where

import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.ProtoLens.Encoding.Wire (Tag (..), TaggedValue (..), WireValue (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word64)
import Peculiar.Rpc.Validate.Cel.Check (Type (..), elementType, fieldType)
import Peculiar.Rpc.Validate.Cel.Compile (Outcome (..), compile, specialize)
import Peculiar.Rpc.Validate.Cel.Eval (Activation (..), evaluate)
import Peculiar.Rpc.Validate.Cel.Syntax (Expr (..))
import Peculiar.Rpc.Validate.Cel.Value
import Peculiar.Rpc.Validate.Dynamic
import Peculiar.Rpc.Validate.Schema

data Subscript
  = IndexAt Word64
  | BoolKey Bool
  | IntKey Int64
  | UintKey Word64
  | StringKey T.Text
  deriving stock (Eq, Show)

data PathElement = PathElement
  { number :: Maybe Int32
  , name :: T.Text
  , fieldType :: Maybe Kind
  , keyType :: Maybe Kind
  , valueType :: Maybe Kind
  , subscript :: Maybe Subscript
  }
  deriving stock (Eq, Show)

data Violation = Violation
  { field :: [PathElement]
  , rule :: [PathElement]
  , ruleId :: T.Text
  , message :: T.Text
  , forKey :: Bool
  }
  deriving stock (Eq, Show)

data Failure
  = CompilationError T.Text
  | RuntimeError T.Text
  deriving stock (Eq, Show)

type Result = Either Failure [Violation]

data Context = Context
  { schema :: Schema
  , now :: Integer
  }

type Check = Context -> Value -> Result

type MessageCheck = Context -> Message -> Result

data MessagePlan = MessagePlan
  { failure :: Maybe Failure
  , checks :: [MessageCheck]
  }

data ValuePlan = ValuePlan
  { rules :: [Check]
  , nested :: [Check]
  , ignoreZero :: Maybe Value
  }

data Engine = Engine
  { schema :: Schema
  , plans :: Map.Map T.Text MessagePlan
  }

data Site = Site
  { base :: Maybe Field
  , prefix :: [PathElement]
  , forItems :: Bool
  }

data Program = Program
  { expression :: Expr
  , ruleId :: T.Text
  , message :: T.Text
  , source :: T.Text
  , path :: [PathElement]
  , bindings :: Map.Map T.Text Value
  }

data Declared = Declared
  { ruleId :: T.Text
  , message :: T.Text
  , expression :: T.Text
  }

data Ignore = Unspecified | IfZero | Always
  deriving stock (Eq)

engineOf :: Schema -> Engine
engineOf schema = engine
 where
  engine = Engine{schema, plans = Map.fromList [(name, messagePlan engine described) | (name, described) <- Map.toList schema.messages]}

validate :: Engine -> Integer -> Message -> Result
validate engine now message = evaluateMessage (planOf engine message.described.fullName) Context{schema = engine.schema, now} message

planOf :: Engine -> T.Text -> MessagePlan
planOf engine name = fromMaybe MessagePlan{failure = Nothing, checks = []} (Map.lookup name engine.plans)

combine :: [Result] -> Result
combine = go []
 where
  go found = \case
    [] -> Right (concat (reverse found))
    Left failure : _ -> Left failure
    Right violations : rest -> go (violations : found) rest

evaluateMessage :: MessagePlan -> MessageCheck
evaluateMessage plan context message = case plan.failure of
  Just failure -> Left failure
  Nothing -> combine [check context message | check <- plan.checks]

elementOf :: Field -> PathElement
elementOf field = PathElement{number = Just field.number, name = field.textName, fieldType = Just field.kind, keyType = Nothing, valueType = Nothing, subscript = Nothing}

ruleElement :: Schema -> T.Text -> T.Text -> [PathElement]
ruleElement schema owner name = foldMap (pure . elementOf) (rulesType schema owner >>= fieldNamed name)

rulesType :: Schema -> T.Text -> Maybe MessageType
rulesType schema name = Map.lookup ("buf.validate." <> name) schema.messages

extensionRules :: Schema -> T.Text -> Int -> [TaggedValue] -> Maybe Message
extensionRules schema owner number options = case [bytes | TaggedValue (Tag tag) (Lengthy bytes) <- options, tag == number] of
  [] -> Nothing
  parts -> rulesType schema owner >>= \described -> either (const Nothing) Just (decodeMessage schema described (BS.concat parts))

ruleValue :: Schema -> T.Text -> Message -> Value
ruleValue schema name rules = maybe NullV (\field -> get schema field rules) (fieldNamed name rules.described)

subRules :: Schema -> T.Text -> Maybe Message -> Maybe Message
subRules schema name = \case
  Just rules | hasRule name rules, MessageV inner <- ruleValue schema name rules -> Just inner
  _ -> Nothing

hasRule :: T.Text -> Message -> Bool
hasRule name rules = any (`has` rules) (fieldNamed name rules.described)

texts :: Value -> [T.Text]
texts = \case
  ListV values -> [text | StringV text <- values]
  _ -> []

messages :: Value -> [Message]
messages = \case
  ListV values -> [message | MessageV message <- values]
  _ -> []

boolean :: Value -> Bool
boolean = \case
  BoolV flag -> flag
  _ -> False

declaredRules :: Schema -> Value -> [Declared]
declaredRules schema value =
  [ Declared{ruleId = text "id" rule, message = text "message" rule, expression = text "expression" rule}
  | rule <- messages value
  ]
 where
  text name rule = case ruleValue schema name rule of
    StringV written -> written
    _ -> ""

writtenRules :: Schema -> Message -> [Declared]
writtenRules schema rules =
  [Declared{ruleId = written, message = "", expression = written} | written <- texts (ruleValue schema "cel_expression" rules)]

ignoreOf :: Schema -> Maybe Message -> Ignore
ignoreOf schema = \case
  Just rules -> case ruleValue schema "ignore" rules of
    IntV 1 -> IfZero
    IntV 3 -> Always
    _ -> Unspecified
  Nothing -> Unspecified

messagePlan :: Engine -> MessageType -> MessagePlan
messagePlan engine described =
  MessagePlan
    { failure = case oneofFailure of
        Just failure -> Just failure
        Nothing -> either Just (const Nothing) compiled
    , checks = expressionChecks <> oneofRuleChecks <> oneofChecks <> fieldChecks
    }
 where
  schema = engine.schema
  messageRules = extensionRules schema "MessageRules" 1159 described.options
  declared = foldMap (\rules -> writtenRules schema rules <> declaredRules schema (ruleValue schema "cel" rules)) messageRules
  compiled = traverse (\rule -> programOf schema (Map.fromList [("this", Primitive (MessageT described.fullName)), ("now", Primitive TimestampT)]) rule [] Map.empty) declared
  expressionChecks = case compiled of
    Right programs | not (null programs) -> [\context message -> runPrograms context (MessageV message) programs]
    _ -> []
  oneofDeclarations = [(texts (ruleValue schema "fields" rule), boolean (ruleValue schema "required" rule)) | rule <- foldMap (messages . ruleValue schema "oneof") messageRules]
  oneofFailure = case mapMaybe oneofProblem oneofDeclarations of
    problem : _ -> Just (CompilationError problem)
    [] -> Nothing
  oneofProblem (names, _)
    | null names = Just ("at least one field must be specified in oneof rule for the message " <> described.fullName)
    | Just duplicate <- firstDuplicate names = Just ("duplicate " <> duplicate <> " in oneof rule for the message " <> described.fullName)
    | Just missing <- find (\name -> isNothing (fieldNamed name described)) names = Just ("field " <> missing <> " not found in message " <> described.fullName)
    | otherwise = Nothing
  oneofRuleChecks = [messageOneofCheck (mapMaybe (`fieldNamed` described) names) required | (names, required) <- oneofDeclarations]
  inMessageOneof field = any (\(names, _) -> field.name `elem` names) oneofDeclarations
  oneofChecks =
    [ \_ message ->
        Right
          [ Violation
              { field = [PathElement{number = Nothing, name = oneof.name, fieldType = Nothing, keyType = Nothing, valueType = Nothing, subscript = Nothing}]
              , rule = []
              , ruleId = "required"
              , message = "exactly one field is required in oneof"
              , forKey = False
              }
          | not (any (\field -> field.oneof == Just oneof.name && has field message) described.fields)
          ]
    | oneof <- described.oneofs
    , Just rules <- [extensionRules schema "OneofRules" 1159 oneof.options]
    , boolean (ruleValue schema "required" rules)
    ]
  fieldChecks = [fieldPlan engine field (inMessageOneof field) | field <- described.fields]

firstDuplicate :: [T.Text] -> Maybe T.Text
firstDuplicate = go Set.empty
 where
  go _ [] = Nothing
  go seen (name : rest)
    | name `Set.member` seen = Just name
    | otherwise = go (Set.insert name seen) rest

messageOneofCheck :: [Field] -> Bool -> MessageCheck
messageOneofCheck fields required _ message
  | count > 1 = Right [violation ("only one of " <> names <> " can be set")]
  | required && count /= 1 = Right [violation ("one of " <> names <> " must be set")]
  | otherwise = Right []
 where
  count = length (filter (`has` message) fields)
  names = T.intercalate ", " [field.name | field <- fields]
  violation text = Violation{field = [], rule = [], ruleId = "message.oneof", message = text, forKey = False}

fieldPlan :: Engine -> Field -> Bool -> MessageCheck
fieldPlan engine field inMessageOneof = check
 where
  schema = engine.schema
  rules = extensionRules schema "FieldRules" 1159 field.options
  ignore = case ignoreOf schema rules of
    Unspecified | isJust rules && inMessageOneof && not (any (hasRule "ignore") rules) -> IfZero
    other -> other
  required = any (boolean . ruleValue schema "required") rules
  built = valuePlan engine field rules ignore Site{base = Just field, prefix = [], forItems = False}
  check context message
    | ignore == Always = Right []
    | otherwise = case built of
        Left failure -> Left failure
        Right plan
          | required && not (has field message) ->
              Right [Violation{field = [elementOf field], rule = ruleElement schema "FieldRules" "required", ruleId = "required", message = "value is required", forKey = False}]
          | (field.presence || ignore == IfZero) && not (has field message) -> Right []
          | otherwise -> evaluateValue plan context (get schema field message)

evaluateValue :: ValuePlan -> Check
evaluateValue plan context value = case plan.ignoreZero of
  Just zero | equal value zero -> Right []
  _ -> combine [check context value | check <- plan.rules <> plan.nested]

wrappers :: [(T.Text, T.Text)]
wrappers =
  [ ("google.protobuf.BoolValue", "bool")
  , ("google.protobuf.BytesValue", "bytes")
  , ("google.protobuf.DoubleValue", "double")
  , ("google.protobuf.FloatValue", "float")
  , ("google.protobuf.Int32Value", "int32")
  , ("google.protobuf.Int64Value", "int64")
  , ("google.protobuf.StringValue", "string")
  , ("google.protobuf.UInt32Value", "uint32")
  , ("google.protobuf.UInt64Value", "uint64")
  ]

wellKnownRules :: [(T.Text, T.Text)]
wellKnownRules =
  [ ("google.protobuf.Any", "any")
  , ("google.protobuf.Duration", "duration")
  , ("google.protobuf.FieldMask", "field_mask")
  , ("google.protobuf.Timestamp", "timestamp")
  ]

scalarRules :: Kind -> Maybe T.Text
scalarRules = \case
  FloatKind -> Just "float"
  DoubleKind -> Just "double"
  Int32Kind -> Just "int32"
  Int64Kind -> Just "int64"
  Uint32Kind -> Just "uint32"
  Uint64Kind -> Just "uint64"
  Sint32Kind -> Just "sint32"
  Sint64Kind -> Just "sint64"
  Fixed32Kind -> Just "fixed32"
  Fixed64Kind -> Just "fixed64"
  Sfixed32Kind -> Just "sfixed32"
  Sfixed64Kind -> Just "sfixed64"
  BoolKind -> Just "bool"
  StringKind -> Just "string"
  BytesKind -> Just "bytes"
  EnumKind -> Just "enum"
  _ -> Nothing

mismatch :: T.Text -> Field -> Field -> Failure
mismatch expected set field =
  CompilationError ("expected rule \"buf.validate.FieldRules." <> expected <> "\", got \"" <> set.fullName <> "\" on field \"" <> field.fullName <> "\"")

data Target = Target
  { engine :: Engine
  , subject :: Field
  , rules :: Maybe Message
  , ignore :: Ignore
  , site :: Site
  }

valuePlan :: Engine -> Field -> Maybe Message -> Ignore -> Site -> Either Failure ValuePlan
valuePlan engine field rules ignore site
  | ignore == Always = Right ValuePlan{rules = [], nested = [], ignoreZero = Nothing}
  | otherwise = do
      expressions <- fieldExpressions engine.schema field rules site
      embedded <- embeddedChecks target
      unwrapped <- wrapperChecks target
      standard <- standardRules engine.schema field rules site
      keyed <- mapChecks target
      items <- itemChecks target
      pure
        ValuePlan
          { rules = expressions <> unwrapped <> standard <> anyChecks target <> enumChecks target <> keyed <> items
          , nested = embedded
          , ignoreZero = if site.forItems && ignore == IfZero then Just (zeroValue engine.schema field) else Nothing
          }
 where
  target = Target{engine, subject = field, rules, ignore, site}

singularMessage :: Target -> Bool
singularMessage target = isMessageKind target.subject.kind && not (isMap target.engine.schema target.subject) && (not target.subject.repeated || target.site.forItems)

embeddedChecks :: Target -> Either Failure [Check]
embeddedChecks target
  | singularMessage target = case nestedPlan.failure of
      Just failure -> Left (wrapped ("failed to compile embedded type " <> target.subject.typeName <> " for " <> target.subject.fullName) failure)
      Nothing ->
        Right
          [ \context value -> case value of
              MessageV message -> fmap (prefixField target.site.base) <$> evaluateMessage nestedPlan context message
              _ -> Right []
          ]
  | otherwise = Right []
 where
  nestedPlan = planOf target.engine target.subject.typeName

wrapperChecks :: Target -> Either Failure [Check]
wrapperChecks target = case (singularMessage target, target.rules >>= whichOneof "type", lookup target.subject.typeName wrappers) of
  (True, Just set, Just expected)
    | set.name /= expected -> Left (mismatch expected set target.subject)
    | Just wrapperType <- Map.lookup target.subject.typeName target.engine.schema.messages
    , Just inner <- fieldNamed "value" wrapperType ->
        (.rules) <$> valuePlan target.engine inner target.rules target.ignore target.site
  _ -> Right []

anyChecks :: Target -> [Check]
anyChecks target
  | singularMessage target && target.subject.typeName == "google.protobuf.Any" && not (null allowed && null blocked) =
      [ \_ value ->
          let url = case value of
                MessageV message | StringV text <- ruleValue schema "type_url" message -> text
                _ -> ""
           in Right
                ( [violation "in" "type URL must be in the allow list" | not (null allowed), url `notElem` allowed]
                    <> [violation "not_in" "type URL must not be in the block list" | url `elem` blocked]
                )
      ]
  | otherwise = []
 where
  schema = target.engine.schema
  anyMessage = subRules schema "any" target.rules
  allowed = foldMap (texts . ruleValue schema "in") anyMessage
  blocked = foldMap (texts . ruleValue schema "not_in") anyMessage
  violation name text =
    Violation
      { field = baseElements target.site
      , rule = target.site.prefix <> ruleElement schema "FieldRules" "any" <> ruleElement schema "AnyRules" name
      , ruleId = "any." <> name
      , message = text
      , forKey = False
      }

enumChecks :: Target -> [Check]
enumChecks target
  | target.subject.kind == EnumKind
  , Just enumMessage <- subRules schema "enum" target.rules
  , boolean (ruleValue schema "defined_only" enumMessage) =
      [ \_ value -> case value of
          IntV number
            | fromIntegral number `notElem` defined ->
                Right
                  [ Violation
                      { field = baseElements target.site
                      , rule = target.site.prefix <> ruleElement schema "FieldRules" "enum" <> ruleElement schema "EnumRules" "defined_only"
                      , ruleId = "enum.defined_only"
                      , message = "value must be one of the defined enum values"
                      , forKey = False
                      }
                  ]
          _ -> Right []
      ]
  | otherwise = []
 where
  schema = target.engine.schema
  defined = foldMap (fmap snd . (.values)) (enumNamed schema target.subject.typeName)

mapChecks :: Target -> Either Failure [Check]
mapChecks target = case mapEntryOf schema target.subject of
  Just entry
    | not target.site.forItems
    , Just keyField <- fieldNamed "key" entry
    , Just valueField <- fieldNamed "value" entry -> do
        keyPlan <- first (wrapped ("failed to compile key rules for map " <> target.subject.fullName)) (valuePlan target.engine keyField keysRules (ignoreOf schema keysRules) (nestedSite "keys"))
        valuesPlan <- first (wrapped ("failed to compile value rules for map " <> target.subject.fullName)) (valuePlan target.engine valueField valuesRules (ignoreOf schema valuesRules) (nestedSite "values"))
        pure
          [ \context value -> case value of
              MapV pairs ->
                combine
                  [ fmap (located keyField valueField key) <$> do
                      keyViolations <- evaluateValue keyPlan context (keyValue key)
                      valueViolations <- evaluateValue valuesPlan context entryValue
                      pure (fmap (\violation -> violation{forKey = True}) keyViolations <> valueViolations)
                  | (key, entryValue) <- Map.toList pairs
                  ]
              _ -> Right []
          ]
  _ -> Right []
 where
  schema = target.engine.schema
  mapRules = subRules schema "map" target.rules
  keysRules = subRules schema "keys" mapRules
  valuesRules = subRules schema "values" mapRules
  nestedSite name = Site{base = Nothing, prefix = ruleElement schema "FieldRules" "map" <> ruleElement schema "MapRules" name, forItems = True}
  located keyField valueField key violation =
    violation
      { field = (elementOf target.subject){keyType = Just keyField.kind, valueType = Just valueField.kind, subscript = Just (keySubscript key)} : violation.field
      , rule = target.site.prefix <> violation.rule
      }

keySubscript :: Key -> Subscript
keySubscript = \case
  BoolK flag -> BoolKey flag
  IntK number -> IntKey number
  UintK number -> UintKey number
  StringK text -> StringKey text

itemChecks :: Target -> Either Failure [Check]
itemChecks target
  | target.subject.repeated && not (isMap schema target.subject) && not target.site.forItems = do
      itemPlan <- first (wrapped ("failed to compile items rules for repeated " <> target.subject.fullName)) (valuePlan target.engine target.subject itemRules (ignoreOf schema itemRules) itemSite)
      pure
        [ \context value -> case value of
            ListV values ->
              combine
                [ fmap (\violation -> violation{field = (elementOf target.subject){subscript = Just (IndexAt position)} : violation.field, rule = target.site.prefix <> violation.rule})
                    <$> evaluateValue itemPlan context item
                | (position, item) <- zip [0 ..] values
                ]
            _ -> Right []
        ]
  | otherwise = Right []
 where
  schema = target.engine.schema
  itemRules = subRules schema "items" (subRules schema "repeated" target.rules)
  itemSite = Site{base = Nothing, prefix = ruleElement schema "FieldRules" "repeated" <> ruleElement schema "RepeatedRules" "items", forItems = True}

wrapped :: T.Text -> Failure -> Failure
wrapped context = \case
  CompilationError reason -> CompilationError (context <> ": " <> reason)
  other -> other

baseElements :: Site -> [PathElement]
baseElements site = foldMap (pure . elementOf) site.base

prefixField :: Maybe Field -> Violation -> Violation
prefixField base violation = case base of
  Just field -> violation{field = elementOf field : violation.field}
  Nothing -> violation

zeroValue :: Schema -> Field -> Value
zeroValue schema field
  | field.repeated = zeroOf schema field
  | isMessageKind field.kind = MessageV (emptyMessage schema field.typeName)
  | otherwise = defaultOf schema field

fieldExpressions :: Schema -> Field -> Maybe Message -> Site -> Either Failure [Check]
fieldExpressions schema field rules site = case rules of
  Nothing -> Right []
  Just fieldRules -> do
    let indexed name position = [element{subscript = Just (IndexAt position)} | element <- ruleElement schema "FieldRules" name]
        environment = Map.fromList [("this", if site.forItems then elementType schema field else fieldType schema field), ("now", Primitive TimestampT)]
        declare name declared = [programOf schema environment rule (indexed name position) Map.empty | (position, rule) <- zip [0 ..] declared]
    programs <- sequence (declare "cel_expression" (writtenRules schema fieldRules) <> declare "cel" (declaredRules schema (ruleValue schema "cel" fieldRules)))
    pure [fieldPrograms schema field site programs | not (null programs)]

fieldPrograms :: Schema -> Field -> Site -> [Program] -> Check
fieldPrograms schema field site programs context value = case thisOf schema field site value of
  Left failure -> Left (RuntimeError (renderError failure))
  Right this -> fmap (fmap (\violation -> violation{field = baseElements site, rule = site.prefix <> violation.rule})) (runPrograms context this programs)

thisOf :: Schema -> Field -> Site -> Value -> Either CelError Value
thisOf schema field site value = case value of
  ListV values | field.repeated && not site.forItems -> ListV <$> traverse (celValue schema) values
  MapV pairs -> MapV <$> traverse (celValue schema) pairs
  other -> celValue schema other

programOf :: Schema -> Map.Map T.Text Type -> Declared -> [PathElement] -> Map.Map T.Text Value -> Either Failure Program
programOf schema declared rule path bindings = case compile schema declared rule.expression of
  Left reason -> Left (CompilationError ("failed to compile expression " <> rule.ruleId <> ": " <> renderCompileError reason))
  Right compiled -> Right Program{expression = compiled, ruleId = rule.ruleId, message = rule.message, source = rule.expression, path, bindings}

runPrograms :: Context -> Value -> [Program] -> Result
runPrograms context this programs = combine (fmap run programs)
 where
  run program =
    let activation = Activation{schema = context.schema, variables = Map.insert "this" this (Map.insert "now" (TimestampV context.now) program.bindings)}
     in case evaluate activation program.expression of
          Left failure -> Left (RuntimeError ("error evaluating " <> program.ruleId <> ": " <> renderError failure))
          Right (StringV "") -> Right []
          Right (StringV text) -> Right [violationOf program text]
          Right (BoolV True) -> Right []
          Right (BoolV False) -> Right [violationOf program (if T.null program.message then "\"" <> program.source <> "\" returned false" else program.message)]
          Right other -> Left (RuntimeError ("resolved to an unexpected type " <> typeName (typeOf other)))
  violationOf program text = Violation{field = [], rule = program.path, ruleId = program.ruleId, message = text, forKey = False}

standardRules :: Schema -> Field -> Maybe Message -> Site -> Either Failure [Check]
standardRules schema field rules site
  | isMessageKind field.kind && isJust (lookup field.typeName wrappers) = Right []
  | Just fieldRules <- rules
  , Just set <- whichOneof "type" fieldRules =
      case expected of
        Just name | name /= set.name -> Left (mismatch name set field)
        Nothing | field.kind == MessageKind -> Left (CompilationError ("mismatched message rules, \"" <> set.fullName <> "\" is not a valid rule for message field \"" <> field.fullName <> "\""))
        _ -> case get schema set fieldRules of
          MessageV typed -> do
            programs <- concat <$> traverse (rulePrograms typed set) (populated typed)
            pure [fieldPrograms schema field site programs | not (null programs)]
          _ -> Right []
  | otherwise = Right []
 where
  expected
    | isMap schema field = Just "map"
    | field.repeated && not site.forItems = Just "repeated"
    | isMessageKind field.kind = lookup field.typeName wellKnownRules
    | otherwise = scalarRules field.kind
  environment typed ruleField =
    Map.fromList
      [ ("this", genericThis)
      , ("now", Primitive TimestampT)
      , ("rules", Primitive (MessageT typed.described.fullName))
      , ("rule", fieldType schema ruleField)
      ]
  genericThis
    | site.forItems = elementType schema field
    | isMap schema field = MapOf Dyn Dyn
    | field.repeated = ListOf Dyn
    | otherwise = elementType schema field
  populated typed = [ruleField | ruleField <- typed.described.fields <> extensionsOf schema typed.described.fullName, has ruleField typed]
  rulePrograms typed set ruleField = do
    ruleBinding <- first (CompilationError . renderError) (fieldValue schema ruleField typed)
    let bindings = Map.fromList [("rules", MessageV typed), ("rule", ruleBinding)]
        declared = foldMap (declaredRules schema . ruleValue schema "cel") (extensionRules schema "PredefinedRules" 1160 ruleField.options)
        path = [elementOf set, elementOf ruleField]
    concat <$> traverse (ruleProgram typed bindings path ruleField) declared
  ruleProgram typed bindings path ruleField rule = case compile schema (environment typed ruleField) rule.expression of
    Left reason -> Left (CompilationError ("failed to compile standard rule " <> ruleField.fullName <> ": " <> renderCompileError reason))
    Right parsed -> case specialize schema bindings parsed of
      Left reason -> Left (CompilationError ("failed to compile program " <> rule.ruleId <> ": " <> renderCompileError reason))
      Right (Constant (BoolV True)) -> Right []
      Right (Constant (StringV "")) -> Right []
      Right (Constant value) -> Right [Program{expression = Literal value, ruleId = rule.ruleId, message = rule.message, source = rule.expression, path, bindings}]
      Right (Residual residual) -> Right [Program{expression = residual, ruleId = rule.ruleId, message = rule.message, source = rule.expression, path, bindings}]

renderPath :: [PathElement] -> T.Text
renderPath = T.intercalate "." . fmap rendered
 where
  rendered element = element.name <> maybe "" subscripted element.subscript
  subscripted = \case
    IndexAt position -> "[" <> T.pack (show position) <> "]"
    BoolKey flag -> if flag then "[true]" else "[false]"
    IntKey number -> "[" <> T.pack (show number) <> "]"
    UintKey number -> "[" <> T.pack (show number) <> "]"
    StringKey text -> "[" <> T.pack (show text) <> "]"
