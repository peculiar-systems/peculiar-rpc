module Peculiar.Rpc.Compression
  ( Compression (..)
  , compressionName
  , compressionNamed
  , compress
  , decompress
  , acceptedNames
  ) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.SnappyC.Framed qualified as Snappy
import Codec.Compression.Zlib qualified as Zlib
import Codec.Compression.Zstd.Lazy qualified as Zstd
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.List (find)
import Data.Text qualified as T

data Compression = Gzip | Deflate | Snappy | Zstd | Brotli
  deriving stock (Eq, Show, Enum, Bounded)

compressionName :: Compression -> BS.ByteString
compressionName = \case
  Gzip -> "gzip"
  Deflate -> "deflate"
  Snappy -> "snappy"
  Zstd -> "zstd"
  Brotli -> "br"

compressionNamed :: BS.ByteString -> Maybe Compression
compressionNamed name = find ((== BC.map toLower (BC.strip name)) . compressionName) [minBound ..]

acceptedNames :: BS.ByteString
acceptedNames = BS.intercalate "," (fmap compressionName [minBound ..] <> ["identity"])

compress :: Compression -> BS.ByteString -> BS.ByteString
compress compression = LBS.toStrict . squeeze . LBS.fromStrict
 where
  squeeze = case compression of
    Gzip -> GZip.compress
    Deflate -> Zlib.compress
    Snappy -> Snappy.compress
    Zstd -> Zstd.compress 3
    Brotli -> Brotli.compressWith Brotli.defaultCompressParams{Brotli.compressLevel = Brotli.CompressionLevel4}

decompress :: Compression -> Maybe Int -> BS.ByteString -> IO (Either T.Text BS.ByteString)
decompress compression limit payload =
  first (T.pack . displayException)
    <$> try @SomeException (evaluate (LBS.toStrict (capped (expand (LBS.fromStrict payload)))))
 where
  capped = maybe id (LBS.take . fromIntegral . (+ 1)) limit
  expand = case compression of
    Gzip -> GZip.decompress
    Deflate -> Zlib.decompress
    Snappy -> Snappy.decompress
    Zstd -> Zstd.decompress
    Brotli -> Brotli.decompress
