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
  [ -- parser
    bparser "https://github.com/mrkkrp/modern-uri"
    -- renders
  , brender   (prepareURI "https://github.com/mrkkrp/modern-uri")
  , brenderBs (prepareURI "https://github.com/mrkkrp/modern-uri")
  ]

----------------------------------------------------------------------------
-- Helpers

-- | Benchmark speed of the 'URI' parser with given input.

bparser :: Text -> Benchmark
bparser txt = env (return txt) (bench name . nf p)
  where
    name = "parser: " ++ T.unpack txt
    p    = parse (URI.parser <* eof :: Parsec Void Text URI) ""

-- | Parse a 'URI' from 'Text' assuming that the 'Text' is a valid 'URI'.

prepareURI :: Text -> URI
prepareURI = fromJust . URI.mkURI

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
