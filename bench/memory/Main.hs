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
  bparser   turiText
  brender   turi
  brenderBs turi

----------------------------------------------------------------------------
-- Helpers

-- | Test 'URI' as 'Text'.

turiText :: Text
turiText = "https://mark:secret@github.com:443/mrkkrp/modern-uri?foo=bar#fragment"

-- | Test 'URI' in parsed form.

turi :: URI
turi = fromJust (URI.mkURI turiText)

-- | Benchmark memory usage of the 'URI' parser with given input.

bparser :: Text -> Weigh ()
bparser txt = func name (parse p "") txt
  where
    name = "parser: " ++ T.unpack txt
    p    = URI.parser <* eof :: Parsec Void Text URI

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
