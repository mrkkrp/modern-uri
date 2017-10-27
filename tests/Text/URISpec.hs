module Text.URISpec (spec) where

import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.URI (URI)
import qualified Text.URI as URI

spec :: Spec
spec =
  describe "parse and render" $
    it "parser and render are consistent" $
      property $ \uri ->
        shouldParse urip (URI.render uri) uri

----------------------------------------------------------------------------
-- Helpers

urip :: Parsec Void Text URI
urip = URI.parse

shouldParse :: (Eq a, Show a)
  => Parsec Void Text a
  -> Text
  -> a
  -> Expectation
shouldParse p s a =
  case runParser p "" s of
    Left e -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      parseErrorPretty' s e
    Right a' -> a' `shouldBe` a
