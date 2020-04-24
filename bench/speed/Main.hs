{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.URI (URI)
import qualified Text.URI as URI

main :: IO ()
main =
  defaultMain
    [ bparser turiStr,
      bparserBs turiStr,
      brender turi,
      brenderBs turi,
      brenderStr turi
    ]

----------------------------------------------------------------------------
-- Helpers

-- | Test 'URI' as a polymorphic string.
turiStr :: IsString a => a
turiStr = "https://mark:secret@github.com:443/mrkkrp/modern-uri?foo=bar#fragment"

-- | Test 'URI' in parsed form.
turi :: URI
turi = fromJust (URI.mkURI turiStr)

-- | Benchmark speed of the 'URI' parser with given input.
bparser :: Text -> Benchmark
bparser txt = env (return txt) (bench "text parser" . nf p)
  where
    p = parse (URI.parser <* eof :: Parsec Void Text URI) ""

-- | Like 'bparser' but accepts a 'ByteString'.
bparserBs :: ByteString -> Benchmark
bparserBs bs = env (return bs) (bench "bs parser" . nf p)
  where
    p = parse (URI.parserBs <* eof :: Parsec Void ByteString URI) ""

-- | Benchmark speed of the 'URI' render with given input.
brender :: URI -> Benchmark
brender uri = env (return uri) (bench "text render" . nf URI.render)

-- | The same as 'brender' but for the 'URI.renderBs' render.
brenderBs :: URI -> Benchmark
brenderBs uri = env (return uri) (bench "bs render" . nf URI.renderBs)

-- | The same as 'brender' but for the 'URI.renderString' render.
brenderStr :: URI -> Benchmark
brenderStr uri = env (return uri) (bench "str render" . nf URI.renderStr)
