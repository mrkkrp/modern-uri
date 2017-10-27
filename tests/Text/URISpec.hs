module Text.URISpec (spec) where

import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.URI (URI)
import qualified Text.URI as URI

spec :: Spec
spec = do
  describe "parse and render" $
    it "parser and render are consistent" $
      property $ \uri ->
        parse urip "" (URI.render uri)
          `shouldParse` uri

----------------------------------------------------------------------------
-- Helpers

urip :: Parsec Void Text URI
urip = URI.parse
