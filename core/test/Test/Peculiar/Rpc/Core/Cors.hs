module Test.Peculiar.Rpc.Core.Cors (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Hedgehog (Gen, Group (..), Property, assert, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Peculiar.Rpc.Core.Cors (Cors (..), Origins (..), preflightHeaders, responseHeaders)

tests :: Group
tests =
  Group
    "Cors"
    [ ("a wildcard entry allows every subdomain", subdomains)
    , ("a wildcard entry allows nothing but subdomains", strangers)
    , ("a wildcard entry keeps to its port", ports)
    , ("a star anywhere else is only itself", literalStars)
    , ("an allowed origin is echoed and varied on", echoed)
    , ("any labels before the suffix are allowed", anyLabels)
    , ("an entry without a star is equality", equality)
    ]

only :: [BS.ByteString] -> Cors
only entries = Cors{origins = OnlyOrigins entries, credentials = False, maxAgeSeconds = 60, expose = []}

answer :: [BS.ByteString] -> BS.ByteString -> Maybe BS.ByteString
answer entries origin = lookup "access-control-allow-origin" (responseHeaders (only entries) [("origin", origin)])

allows :: BS.ByteString -> BS.ByteString -> Bool
allows entry origin = answer [entry] origin == Just origin

subdomains :: Property
subdomains = withTests 1 $ property do
  assert (allows "https://*.example.org" "https://a.example.org")
  assert (allows "https://*.example.org" "https://a.b.example.org")
  assert (allows "https://*.example.org" "https://a-1.example.org")
  answer ["https://example.com", "https://*.example.org"] "https://a.example.org" === Just "https://a.example.org"

strangers :: Property
strangers = withTests 1 $ property do
  assert (not (allows "https://*.example.org" "https://example.org"))
  assert (not (allows "https://*.example.org" "http://a.example.org"))
  assert (not (allows "https://*.example.org" "https://evilexample.org"))
  assert (not (allows "https://*.example.org" "https://example.org.evil.test"))
  assert (not (allows "https://*.example.org" "https://a.example.org.evil.test"))
  assert (not (allows "https://*.example.org" "https://.example.org"))
  assert (not (allows "https://*.example.org" "https://a..example.org"))
  assert (not (allows "https://*.example.org" "https://evil.test/.example.org"))
  assert (not (allows "https://*.example.org" "https://evil.test:1.example.org"))
  assert (not (allows "https://*.example.org" "https://*.example.org"))

ports :: Property
ports = withTests 1 $ property do
  assert (allows "https://*.example.org:8443" "https://a.example.org:8443")
  assert (not (allows "https://*.example.org:8443" "https://a.example.org"))
  assert (not (allows "https://*.example.org:8443" "https://a.example.org:8444"))
  assert (not (allows "https://*.example.org" "https://a.example.org:8443"))

literalStars :: Property
literalStars = withTests 1 $ property do
  assert (not (allows "https://a*.example.org" "https://ab.example.org"))
  assert (allows "https://a*.example.org" "https://a*.example.org")
  assert (not (allows "https://a.*.example.org" "https://a.b.example.org"))
  assert (not (allows "https://*.*.example.org" "https://a.b.example.org"))
  assert (allows "https://*.*.example.org" "https://*.*.example.org")
  assert (not (allows "https://*example.org" "https://a.example.org"))
  assert (not (allows "https://*" "https://a.example.org"))
  assert (not (allows "*" "https://a.example.org"))

echoed :: Property
echoed = withTests 1 $ property do
  let policy = only ["https://*.example.org"]
      request origin = [("origin", origin), ("access-control-request-method", "POST")]
  lookup "access-control-allow-origin" (preflightHeaders policy (request "https://a.example.org")) === Just "https://a.example.org"
  lookup "vary" (preflightHeaders policy (request "https://a.example.org")) === Just "origin"
  preflightHeaders policy (request "https://example.org") === [("vary", "origin")]
  responseHeaders policy [("origin", "https://example.org")] === [("vary", "origin")]

anyLabels :: Property
anyLabels = property do
  prefix <- forAll (BS.intercalate "." <$> Gen.list (Range.linear 1 4) genLabel)
  suffix <- forAll (BS.intercalate "." <$> Gen.list (Range.linear 1 3) genLabel)
  assert (allows ("https://*." <> suffix) ("https://" <> prefix <> "." <> suffix))
  assert (not (allows ("https://*." <> suffix) ("https://" <> suffix)))
  assert (not (allows ("https://*." <> suffix) ("https://" <> prefix <> suffix)))
  assert (not (allows ("https://*." <> suffix) ("http://" <> prefix <> "." <> suffix)))

equality :: Property
equality = property do
  entry <- forAll genOrigin
  other <- forAll (Gen.choice [genOrigin, pure entry])
  allows entry other === (entry == other)

genOrigin :: Gen BS.ByteString
genOrigin = do
  scheme <- Gen.element ["http://", "https://"]
  host <- BS.intercalate "." <$> Gen.list (Range.linear 1 3) genLabel
  port <- Gen.element ["", ":8443"]
  pure (scheme <> host <> port)

genLabel :: Gen BS.ByteString
genLabel = BC.pack <$> Gen.list (Range.linear 1 6) (Gen.element (['a' .. 'c'] <> ['0' .. '1'] <> "-"))
