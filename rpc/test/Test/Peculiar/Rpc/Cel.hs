module Test.Peculiar.Rpc.Cel (tests) where

import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog (Group (..), Property, annotate, assert, failure, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Validate.Address (isHostname, isIp, isIpPrefix)
import Peculiar.Rpc.Validate.Cel.Compile (expressionOf)
import Peculiar.Rpc.Validate.Cel.Eval (Activation (..), evaluate)
import Peculiar.Rpc.Validate.Cel.Time (maximumSeconds, minimumSeconds, parseTimestamp, renderTimestamp)
import Peculiar.Rpc.Validate.Cel.Value (Value (..), renderCompileError, renderError)
import Peculiar.Rpc.Validate.Dynamic (equal)
import Peculiar.Rpc.Validate.Uri (isUri)

tests :: Group
tests =
  Group
    "CEL"
    [ ("a timestamp survives rendering and parsing", timestampRoundTrip)
    , ("expressions evaluate to CEL's results", withTests 1 evaluations)
    , ("expressions CEL rejects are errors", withTests 1 rejections)
    , ("addresses follow protovalidate's grammar", withTests 1 addresses)
    ]

evaluated :: T.Text -> Either T.Text Value
evaluated source = do
  (expression, _) <- first renderCompileError (expressionOf mempty Map.empty source)
  first renderError (evaluate Activation{schema = mempty, variables = Map.empty} expression)

timestampRoundTrip :: Property
timestampRoundTrip = property do
  nanos <- forAll (Gen.integral (Range.linearFrom 0 (minimumSeconds * 1000000000) (maximumSeconds * 1000000000 + 999999999)))
  parseTimestamp (renderTimestamp nanos) === Just nanos

evaluations :: Property
evaluations = property do
  let cases =
        [ ("'%s has %d pages'.format(['Odes', 80])", StringV "Odes has 80 pages")
        , ("'%.2f'.format([2.675])", StringV "2.67")
        , ("'%e'.format([1234.5])", StringV "1.234500e+03")
        , ("'%x'.format([b'\\x01\\xff'])", StringV "01ff")
        , ("'%s'.format([[1, 2.5, 'a']])", StringV "[1, 2.5, a]")
        , ("size('h\x00e9llo')", IntV 5)
        , ("'tacocat'.substring(1, 4)", StringV "aco")
        , ("'a,b,c'.split(',', 2)", ListV [StringV "a", StringV "b,c"])
        , ("[1, 2, 3].all(x, x > 0) && [1, 2, 3].exists_one(x, x == 2)", BoolV True)
        , ("[1, 2, 3].map(x, x * 2).filter(x, x > 2)", ListV [IntV 4, IntV 6])
        , ("{'a': 1, 'b': 2}['b']", IntV 2)
        , ("1 == 1.0 && 2u == 2 && 1 < 1.5", BoolV True)
        , ("-9223372036854775808", IntV minBound)
        , ("'abc'.matches('^a[bc]+$')", BoolV True)
        , ("duration('1h30m') == duration('5400s')", BoolV True)
        , ("timestamp('2024-01-01T00:00:00Z').getDayOfWeek()", IntV 1)
        , ("timestamp('2024-03-01T00:30:00Z').getDate('-01:00')", IntV 29)
        , ("string(timestamp('2024-01-01T12:00:00.5+02:00'))", StringV "2024-01-01T10:00:00.5Z")
        , ("string(duration('1.5s'))", StringV "1.5s")
        , ("int('42') + int(3.9) + int(-3.9)", IntV 42)
        , ("type(1u) == uint && type('a') == string", BoolV True)
        , ("false && (1 / 0 == 0)", BoolV False)
        , ("(1 / 0 == 0) || true", BoolV True)
        , ("[1, 'a'].exists(x, x == 'a')", BoolV True)
        , ("'TacoCat'.lowerAscii() + 'x'.upperAscii()", StringV "tacocatX")
        , ("[1.0, 2.0, 1.0].unique()", BoolV False)
        , ("'::ffff:1.2.3.4'.isIp(6) && '1.2.3.0/24'.isIpPrefix(4, true)", BoolV True)
        ]
  forM_ cases \(source, expected) -> case evaluated source of
    Right actual -> do
      annotate (T.unpack source)
      assert (equal expected actual)
    Left problem -> annotate (T.unpack (source <> ": " <> problem)) >> failure

rejections :: Property
rejections = property do
  let sources =
        [ "9223372036854775807 + 1"
        , "1 + 1u"
        , "1 / 0"
        , "{'a': 1}['b']"
        , "[1][1]"
        , "'abc'.matches('(')"
        , "1.5 < 'a'"
        , "uint(-1)"
        , "int('4x')"
        , "timestamp('9999-12-31T23:59:59Z') + duration('1s')"
        , "nothing"
        , "this.missing"
        ]
  mapM_ (\source -> annotate (T.unpack source) >> assert (isLeft (evaluated source))) sources

addresses :: Property
addresses = property do
  assert (isHostname "example.com" && not (isHostname "-example.com") && not (isHostname "123.456"))
  assert (isIp "2001:db8::1" 0 && not (isIp "2001:db8:::1" 6) && not (isIp "1.2.3.04" 4))
  assert (isIpPrefix "2001:db8::/32" 6 True && not (isIpPrefix "2001:db8::1/32" 6 True))
  assert (isUri "https://example.com:8080/a?b#c" && not (isUri "//example.com"))
