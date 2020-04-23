{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Text.QQSpec
  ( spec,
  )
where

import Test.Hspec
import Text.URI
import qualified Text.URI.QQ as QQ

spec :: Spec
spec = do
  describe "uri" $ do
    it "works as an expression" $ do
      let uriQQ = [QQ.uri|https://markkarpov.com|]
      uri <- mkURI "https://markkarpov.com"
      uriQQ `shouldBe` uri
    it "works as a pattern" $
      mkURI "https://markkarpov.com" >>= \case
        [QQ.uri|https://haskell.org|] -> shouldNotMatch
        [QQ.uri|https://markkarpov.com|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "scheme" $ do
    it "works as an expression" $ do
      let schemeQQ = [QQ.scheme|https|]
      scheme <- mkScheme "https"
      schemeQQ `shouldBe` scheme
    it "works as a pattern" $
      mkScheme "https" >>= \case
        [QQ.scheme|ftp|] -> shouldNotMatch
        [QQ.scheme|https|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "host" $ do
    it "works as an expression" $ do
      let hostQQ = [QQ.host|markkarpov.com|]
      host <- mkHost "markkarpov.com"
      hostQQ `shouldBe` host
    it "works as a pattern" $ do
      mkHost "markkarpov.com" >>= \case
        [QQ.host|haskell.org|] -> shouldNotMatch
        [QQ.host|markkarpov.com|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "username" $ do
    it "works as an expression" $ do
      let usernameQQ = [QQ.username|mark|]
      username <- mkUsername "mark"
      usernameQQ `shouldBe` username
    it "works as a pattern" $ do
      mkUsername "mark" >>= \case
        [QQ.username|chris|] -> shouldNotMatch
        [QQ.username|mark|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "password" $ do
    it "works as an expression" $ do
      let passwordQQ = [QQ.password|secret123|]
      password <- mkPassword "secret123"
      passwordQQ `shouldBe` password
    it "works as a pattern" $ do
      mkPassword "secret123" >>= \case
        [QQ.password|secretXYZ|] -> shouldNotMatch
        [QQ.password|secret123|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "pathPiece" $ do
    it "works as an expression" $ do
      let pathPieceQQ = [QQ.pathPiece|foo|]
      pathPiece <- mkPathPiece "foo"
      pathPieceQQ `shouldBe` pathPiece
    it "works as a pattern" $ do
      mkPathPiece "foo" >>= \case
        [QQ.pathPiece|bar|] -> shouldNotMatch
        [QQ.pathPiece|foo|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "queryKey" $ do
    it "works as an expression" $ do
      let queryKeyQQ = [QQ.queryKey|foo|]
      queryKey <- mkQueryKey "foo"
      queryKeyQQ `shouldBe` queryKey
    it "works as a pattern" $ do
      mkQueryKey "foo" >>= \case
        [QQ.queryKey|xyz|] -> shouldNotMatch
        [QQ.queryKey|foo|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "queryValue" $ do
    it "works as an expression" $ do
      let queryValueQQ = [QQ.queryValue|bar|]
      queryValue <- mkQueryValue "bar"
      queryValueQQ `shouldBe` queryValue
    it "works as a pattern" $ do
      mkQueryValue "bar" >>= \case
        [QQ.queryValue|xyz|] -> shouldNotMatch
        [QQ.queryValue|bar|] -> return ()
        _ -> shouldHaveMatchedAlready

  describe "fragment" $ do
    it "works as an expression" $ do
      let fragmentQQ = [QQ.fragment|frag|]
      fragment <- mkFragment "frag"
      fragmentQQ `shouldBe` fragment
    it "works as a pattern" $ do
      mkFragment "frag" >>= \case
        [QQ.fragment|xyz|] -> shouldNotMatch
        [QQ.fragment|frag|] -> return ()
        _ -> shouldHaveMatchedAlready

shouldNotMatch :: Expectation
shouldNotMatch =
  expectationFailure
    "First case should not have matched, but did"

shouldHaveMatchedAlready :: Expectation
shouldHaveMatchedAlready =
  expectationFailure
    "Second case should have matched, but didn't"
