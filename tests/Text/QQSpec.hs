{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.QQSpec (spec) where

import Test.Hspec
import Text.URI
import qualified Text.URI.QQ as QQ

spec :: Spec
spec = do
  describe "uri" . it "works" $ do
    let uriQQ = [QQ.uri|https://markkarpov.com|]
    uri <- mkURI "https://markkarpov.com"
    uriQQ `shouldBe` uri
  describe "scheme" . it "works" $ do
    let schemeQQ = [QQ.scheme|https|]
    scheme <- mkScheme "https"
    schemeQQ `shouldBe` scheme
  describe "host" . it "works" $ do
    let hostQQ = [QQ.host|markkarpov.com|]
    host <- mkHost "markkarpov.com"
    hostQQ `shouldBe` host
  describe "username" . it "works" $ do
    let usernameQQ = [QQ.username|mark|]
    username <- mkUsername "mark"
    usernameQQ `shouldBe` username
  describe "password" . it "works" $ do
    let passwordQQ = [QQ.password|secret123|]
    password <- mkPassword "secret123"
    passwordQQ `shouldBe` password
  describe "pathPiece" . it "works" $ do
    let pathPieceQQ = [QQ.pathPiece|foo|]
    pathPiece <- mkPathPiece "foo"
    pathPieceQQ `shouldBe` pathPiece
  describe "queryKey" . it "works" $ do
    let queryKeyQQ = [QQ.queryKey|foo|]
    queryKey <- mkQueryKey "foo"
    queryKeyQQ `shouldBe` queryKey
  describe "queryValue" . it "works" $ do
    let queryValueQQ = [QQ.queryValue|bar|]
    queryValue <- mkQueryKey "bar"
    queryValueQQ `shouldBe` queryValue
  describe "fragment" . it "works" $ do
    let fragmentQQ = [QQ.fragment|frag|]
    fragment <- mkQueryKey "frag"
    fragmentQQ `shouldBe` fragment
