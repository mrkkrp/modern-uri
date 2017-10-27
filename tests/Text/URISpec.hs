{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.URISpec (spec) where

import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec
import Text.URI (URI (..), RTextException (..), RTextLabel (..))
import qualified Data.Text as T
import qualified Text.URI  as URI

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

spec :: Spec
spec = do
  describe "mkURI" $
    it "" pending -- TODO
  describe "makeAbsolute" $ do
    context "when given URI already has scheme" $
      it "returns that URI unchanged" $
        property $ \scheme uri -> isJust (uriScheme uri) ==>
          uriScheme (URI.makeAbsolute scheme uri) `shouldBe` uriScheme uri
    context "when given URI has no scheme" $
      it "sets the specified scheme" $
        property $ \scheme uri -> isNothing (uriScheme uri) ==>
          uriScheme (URI.makeAbsolute scheme uri) `shouldBe` Just scheme
  describe "mkScheme" $ do
    it "accepts valid schemes" $ do
      URI.mkScheme "http"   `shouldRText` "http"
      URI.mkScheme "HTTPS"  `shouldRText` "https"
      URI.mkScheme "mailto" `shouldRText` "mailto"
      URI.mkScheme "a+-."   `shouldRText` "a+-."
    it "rejects invalid schemes" $ do
      URI.mkScheme "123"   `shouldThrow` (== RTextException Scheme "123")
      URI.mkScheme "схема" `shouldThrow` (== RTextException Scheme "схема")
      URI.mkScheme "+-."   `shouldThrow` (== RTextException Scheme "+-.")
  describe "mkHost" $ do
    it "accepts valid IPv4 literals" $ do
      URI.mkHost "127.0.0.1"    `shouldRText` "127.0.0.1"
      URI.mkHost "198.98.43.23" `shouldRText` "198.98.43.23"
    it "accepts valid IPv6 literals" $ do
      URI.mkHost "[0123:4567:89ab:cdef:0:0:0:0]" `shouldRText`
        "[0123:4567:89ab:cdef:0:0:0:0]"
      URI.mkHost "[0123::4567:89ab]" `shouldRText`
        "[0123::4567:89ab]"
      URI.mkHost "[::0123:4567:89ab]" `shouldRText`
        "[::0123:4567:89ab]"
      URI.mkHost "[0123:4567:89ab::]" `shouldRText`
        "[0123:4567:89ab::]"
    it "rejects invalid IPv6 literals" $ do
      URI.mkHost "[0123:4567:89ab]" `shouldThrow`
        (== RTextException Host "[0123:4567:89ab]")
      URI.mkHost "[0123::4567:89ab::]" `shouldThrow`
        (== RTextException Host "[0123::4567:89ab::]")
    it "accepts valid IP future literals" $ do
      URI.mkHost "[va.something]" `shouldRText` "[va.something]"
      URI.mkHost "[v1.123-456]"   `shouldRText` "[v1.123-456]"
    it "rejects invalid IP future literals" $
      URI.mkHost "[vv.something]" `shouldThrow`
        (== RTextException Host "[vv.something]")
    it "accepts valid domain names" $ do
      URI.mkHost "LOCALHOST"       `shouldRText` "localhost"
      URI.mkHost "github.com"      `shouldRText` "github.com"
      URI.mkHost "foo.example.com" `shouldRText` "foo.example.com"
      URI.mkHost "юникод.рф"       `shouldRText` "юникод.рф"
    it "rejects invalid hosts" $ do
      URI.mkHost "_something" `shouldThrow`
        (== RTextException Host "_something")
      URI.mkHost "some@thing" `shouldThrow`
        (== RTextException Host "some@thing")
  describe "mkUsername" $ do
    it "accepts valid usernames" $
      property $ \txt -> not (T.null txt) ==> do
        username <- URI.mkUsername txt
        URI.unRText username `shouldBe` txt
    it "rejects invalid usernames" $
      URI.mkUsername "" `shouldThrow` (== RTextException Username "")
  describe "mkPassword" $
    it "lifts any text into password" $
      property $ \txt -> do
        pass <- URI.mkPassword txt
        URI.unRText pass `shouldBe` txt
  describe "mkPathPiece" $ do
    it "accepts valid path pieces" $
      property $ \txt -> not (T.null txt) ==> do
        pp <- URI.mkPathPiece txt
        URI.unRText pp `shouldBe` txt
    it "rejects invalid path pieces" $
      URI.mkPathPiece "" `shouldThrow` (== RTextException PathPiece "")
  describe "mkQueryKey" $ do
    it "accepts valid query keys" $
      property $ \txt -> not (T.null txt) ==> do
        k <- URI.mkQueryKey txt
        URI.unRText k `shouldBe` txt
    it "rejects invalid query keys" $
      URI.mkQueryKey "" `shouldThrow` (== RTextException QueryKey "")
  describe "mkQueryValue" $
    it "lifts any text into query value" $
      property $ \txt -> do
        v <- URI.mkQueryValue txt
        URI.unRText v `shouldBe` txt
  describe "mkFragment" $
    it "lifts any text into fragment" $
      property $ \txt -> do
        fragment <- URI.mkFragment txt
        URI.unRText fragment `shouldBe` txt
  describe "parse and render" $
    it "parser and render are consistent" $
      property $ \uri ->
        shouldParse (URI.render uri) uri
  -- TODO something nominal for rendering in text
  -- TODO rendering of byte strings
  -- TODO parser: various stuff, check corner cases
  -- TODO parser: check parse errors

----------------------------------------------------------------------------
-- Helpers

-- | Expect that the given action constructs 'URI.RText' with certain text
-- inside.

shouldRText
  :: IO (URI.RText l)  -- ^ Action that produces refined text
  -> Text              -- ^ Inner text to compare with
  -> Expectation
shouldRText rtext txt = do
  txt' <- rtext
  URI.unRText txt' `shouldBe` txt

-- | Expect that the specified input for parser will produce 'URI' equal to
-- a given one.

shouldParse
  :: Text              -- ^ Parser input
  -> URI               -- ^ 'URI' to compare with
  -> Expectation
shouldParse s a =
  case runParser (URI.parse :: Parsec Void Text URI) "" s of
    Left e -> expectationFailure $
      "the parser is expected to succeed, but it failed with:\n" ++
      parseErrorPretty' s e
    Right a' -> a' `shouldBe` a
