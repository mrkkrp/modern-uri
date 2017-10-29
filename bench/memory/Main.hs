{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.URI (URI)
import Weigh
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Text.URI              as URI

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser    turiStr
  bparserBs  turiStr
  brender    turi
  brenderBs  turi
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
bparser txt = func name (parse p "") txt
  where
    name = "text parser: " ++ T.unpack txt
    p    = URI.parser <* eof :: Parsec Void Text URI

-- | Like 'bparser' but accepts a 'ByteString'.

bparserBs :: ByteString -> Weigh ()
bparserBs bs = func name (parse p "") bs
  where
    name = "bs   parser: " ++ B8.unpack bs
    p    = URI.parserBs <* eof :: Parsec Void ByteString URI

-- | Benchmark memory usage of the 'URI' render with given input.

brender :: URI -> Weigh ()
brender uri = func name URI.render uri
  where
    name = "text render: " ++ T.unpack (URI.render uri)

-- | The same as 'brender' but for the 'URI.renderBs' render.

brenderBs :: URI -> Weigh ()
brenderBs uri = func name URI.renderBs uri
  where
    name = "bs   render: " ++ T.unpack (URI.render uri)

-- | The same as 'brender' but for the 'URI.renderString' render.

brenderStr :: URI -> Weigh ()
brenderStr uri = func name URI.renderStr uri
  where
    name = "str  render: " ++ T.unpack (URI.render uri)
