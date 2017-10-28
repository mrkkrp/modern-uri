{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.URI (URI)
import qualified Data.Text as T
import qualified Text.URI  as URI

main :: IO ()
main = defaultMain
  [ bparser   turiText
  , brender   turi
  , brenderBs turi ]

----------------------------------------------------------------------------
-- Helpers

-- | Test 'URI' as 'Text'.

turiText :: Text
turiText = "https://mark:secret@github.com:443/mrkkrp/modern-uri?foo=bar#fragment"

-- | Test 'URI' in parsed form.

turi :: URI
turi = fromJust (URI.mkURI turiText)

-- | Benchmark speed of the 'URI' parser with given input.

bparser :: Text -> Benchmark
bparser txt = env (return txt) (bench name . nf p)
  where
    name = "parser: " ++ T.unpack txt
    p    = parse (URI.parser <* eof :: Parsec Void Text URI) ""

-- | Benchmark speed of the 'URI' render with given input.

brender :: URI -> Benchmark
brender uri = env (return uri) (bench name . nf URI.render)
  where
    name = "text render: " ++ T.unpack (URI.render uri)

-- | The same as 'brender' but for the 'ByteString' render.

brenderBs :: URI -> Benchmark
brenderBs uri = env (return uri) (bench name . nf URI.renderBs)
  where
    name = "bs render: " ++ T.unpack (URI.render uri)
