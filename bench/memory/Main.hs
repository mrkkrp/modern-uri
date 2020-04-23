{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.URI (URI)
import qualified Text.URI as URI
import Weigh

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser turiStr
  bparserBs turiStr
  brender turi
  brenderBs turi
  brenderStr turi

----------------------------------------------------------------------------
-- Helpers

-- | Test 'URI' as a polymorphic string.
turiStr :: IsString a => a
turiStr = "https://mark:secret@github.com:443/mrkkrp/modern-uri?foo=bar#fragment"

-- | Test 'URI' in parsed form.
turi :: URI
turi = fromJust (URI.mkURI turiStr)

-- | Benchmark memory usage of the 'URI' parser with given input.
bparser :: Text -> Weigh ()
bparser = func "text parser" (parse p "")
  where
    p = URI.parser <* eof :: Parsec Void Text URI

-- | Like 'bparser' but accepts a 'ByteString'.
bparserBs :: ByteString -> Weigh ()
bparserBs = func "bs parser" (parse p "")
  where
    p = URI.parserBs <* eof :: Parsec Void ByteString URI

-- | Benchmark memory usage of the 'URI' render with given input.
brender :: URI -> Weigh ()
brender = func "text render" URI.render

-- | The same as 'brender' but for the 'URI.renderBs' render.
brenderBs :: URI -> Weigh ()
brenderBs = func "bs render" URI.renderBs

-- | The same as 'brender' but for the 'URI.renderString' render.
brenderStr :: URI -> Weigh ()
brenderStr = func "str render" URI.renderStr
