{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.URI (URI)
import Weigh
import qualified Data.Text as T
import qualified Text.URI  as URI

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  -- parser
  bparser "https://github.com/mrkkrp/modern-uri"
  -- renders
  brender   (prepareURI "https://github.com/mrkkrp/modern-uri")
  brenderBs (prepareURI "https://github.com/mrkkrp/modern-uri")

----------------------------------------------------------------------------
-- Helpers

-- | Benchmark memory usage of the 'URI' parser with given input.

bparser :: Text -> Weigh ()
bparser txt = func name (parse p "") txt
  where
    name = "parser: " ++ T.unpack txt
    p    = URI.parser <* eof :: Parsec Void Text URI

-- | Parse a 'URI' from 'Text' assuming that the 'Text' is a valid 'URI'.

prepareURI :: Text -> URI
prepareURI = fromJust . URI.mkURI

-- | Benchmark memory usage of the 'URI' render with given input.

brender :: URI -> Weigh ()
brender uri = func name URI.render uri
  where
    name = "text render: " ++ T.unpack (URI.render uri)

-- | The same as 'brender' but for the 'ByteString' render.

brenderBs :: URI -> Weigh ()
brenderBs uri = func name URI.renderBs uri
  where
    name = "bs render: " ++ T.unpack (URI.render uri)
