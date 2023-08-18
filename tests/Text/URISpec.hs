{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.URISpec (spec) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, isNothing)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.URI (RTextException (..), RTextLabel (..), URI (..))
import Text.URI qualified as URI

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

spec :: Spec
spec = do
  describe "mkURI" $ do
    it "accepts valid URIs" $ do
      uri <- mkTestURI
      URI.mkURI testURI `shouldReturn` uri
    it "accepts a URI with an underscore in host name" $ do
      scheme <- URI.mkScheme "http"
      host <- URI.mkHost "auth_service"
      path <- mapM URI.mkPathPiece ["api", "v1", "users", "validate"]
      URI.mkURI "http://auth_service:3000/api/v1/users/validate"
        `shouldReturn` URI
          { uriScheme = Just scheme,
            uriAuthority =
              Right
                URI.Authority
                  { URI.authUserInfo = Nothing,
                    URI.authHost = host,
                    URI.authPort = Just 3000
                  },
            uriPath = Just (False, NE.fromList path),
            uriQuery = [],
            uriFragment = Nothing
          }
    it "rejects invalid URIs" $ do
      let e =
            err 0 . mconcat $
              [ utok 'ч',
                etok '#',
                etok '/',
                etoks "//",
                etok '?',
                elabel "ASCII alpha character",
                elabel "path piece",
                eeof
              ]
          b =
            ParseErrorBundle
              { bundleErrors = e :| [],
                bundlePosState = initialPosState s
              }
          s = "что-то"
      URI.mkURI s `shouldThrow` (== URI.ParseException b)
  describe "mkURIBs" $ do
    it "accepts valid URIs" $ do
      uri <- mkTestURI
      URI.mkURIBs testURI `shouldReturn` uri
  describe "emptyURI" $ do
    it "parsing of empty input produces emptyURI" $
      URI.mkURI "" `shouldReturn` URI.emptyURI
    context "rendering of emptyURI produces empty output" $ do
      it "with render" $
        URI.render URI.emptyURI `shouldBe` ""
      it "with renderBs" $
        URI.renderBs URI.emptyURI `shouldBe` ""
      it "with renderStr" $
        URI.renderStr URI.emptyURI `shouldBe` ""
  describe "makeAbsolute" $ do
    context "when given URI already has scheme" $
      it "returns that URI unchanged" $
        property $ \scheme uri ->
          isJust (uriScheme uri) ==>
            uriScheme (URI.makeAbsolute scheme uri) `shouldBe` uriScheme uri
    context "when given URI has no scheme" $
      it "sets the specified scheme" $
        property $ \scheme uri ->
          isNothing (uriScheme uri) ==>
            uriScheme (URI.makeAbsolute scheme uri) `shouldBe` Just scheme
  describe "isPathAbsolute" $ do
    context "when URI has authority component" $
      it "returns True" $
        property $ \uri auth ->
          URI.isPathAbsolute (uri {uriAuthority = Right auth}) `shouldBe` True
    context "when URI has no authority component" $
      it "return what is inside of Left in uriAuthority" $
        property $ \uri b ->
          URI.isPathAbsolute (uri {uriAuthority = Left b}) `shouldBe` b
  describe "mkScheme" $ do
    it "accepts valid schemes" $ do
      URI.mkScheme "http" `shouldRText` "http"
      URI.mkScheme "HTTPS" `shouldRText` "https"
      URI.mkScheme "mailto" `shouldRText` "mailto"
      URI.mkScheme "a+-." `shouldRText` "a+-."
    it "rejects invalid schemes" $ do
      URI.mkScheme "123" `shouldThrow` (== RTextException Scheme "123")
      URI.mkScheme "схема" `shouldThrow` (== RTextException Scheme "схема")
      URI.mkScheme "+-." `shouldThrow` (== RTextException Scheme "+-.")
  describe "mkHost" $ do
    it "accepts valid IPv4 literals" $ do
      URI.mkHost "127.0.0.1" `shouldRText` "127.0.0.1"
      URI.mkHost "198.98.43.23" `shouldRText` "198.98.43.23"
    it "accepts valid IPv6 literals" $ do
      URI.mkHost "[0123:4567:89ab:cdef:0:0:0:0]"
        `shouldRText` "[0123:4567:89ab:cdef:0:0:0:0]"
      URI.mkHost "[0123::4567:89ab]"
        `shouldRText` "[0123::4567:89ab]"
      URI.mkHost "[::0123:4567:89ab]"
        `shouldRText` "[::0123:4567:89ab]"
      URI.mkHost "[0123:4567:89ab::]"
        `shouldRText` "[0123:4567:89ab::]"
    it "rejects invalid IPv6 literals" $ do
      URI.mkHost "[0123:4567:89ab]"
        `shouldThrow` (== RTextException Host "[0123:4567:89ab]")
      URI.mkHost "[0123::4567:89ab::]"
        `shouldThrow` (== RTextException Host "[0123::4567:89ab::]")
    it "accepts valid IP future literals" $ do
      URI.mkHost "[va.something]" `shouldRText` "[va.something]"
      URI.mkHost "[v1.123-456]" `shouldRText` "[v1.123-456]"
    it "rejects invalid IP future literals" $
      URI.mkHost "[vv.something]"
        `shouldThrow` (== RTextException Host "[vv.something]")
    it "accepts valid domain names" $ do
      URI.mkHost "LOCALHOST" `shouldRText` "localhost"
      URI.mkHost "github.com" `shouldRText` "github.com"
      URI.mkHost "foo.example.com" `shouldRText` "foo.example.com"
      URI.mkHost "104.155.144.4.sslip.io" `shouldRText` "104.155.144.4.sslip.io"
      URI.mkHost "юникод.рф" `shouldRText` "юникод.рф"
      URI.mkHost "" `shouldRText` ""
      URI.mkHost "." `shouldRText` "."
      URI.mkHost "my-host." `shouldRText` "my-host."
      URI.mkHost "auth_service" `shouldRText` "auth_service"
    it "rejects invalid hosts" $ do
      URI.mkHost ")something"
        `shouldThrow` (== RTextException Host ")something")
      URI.mkHost "some@thing"
        `shouldThrow` (== RTextException Host "some@thing")
  describe "mkUsername" $ do
    it "accepts valid usernames" $
      property $ \txt ->
        not (T.null txt) ==> do
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
      property $ \txt ->
        not (T.null txt) ==> do
          pp <- URI.mkPathPiece txt
          URI.unRText pp `shouldBe` txt
    it "rejects invalid path pieces" $
      URI.mkPathPiece "" `shouldThrow` (== RTextException PathPiece "")
  describe "mkQueryKey" $ do
    it "accepts valid query keys" $
      property $ \txt ->
        not (T.null txt) ==> do
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
        shouldParse' (URI.render uri) uri
  describe "parseBs and renderBs" $
    it "parser and render are consistent" $
      property $ \uri ->
        shouldParseBs (URI.renderBs uri) uri
  describe "parse" $ do
    it "rejects Unicode in scheme" $
      parse urip "" "что:something"
        `shouldFailWith` err
          0
          ( mconcat
              [ utok 'ч',
                etok '#',
                etok '/',
                etoks "//",
                etok '?',
                elabel "ASCII alpha character",
                elabel "path piece",
                eeof
              ]
          )
    it "rejects Unicode in host" $ do
      let s = "https://юникод.рф"
      parse urip "" s
        `shouldFailWith` err
          8
          ( mconcat
              [ utok 'ю',
                etok '#',
                etok '%',
                etok '.',
                etok '/',
                etok ':',
                etok '?',
                etok '[',
                elabel "path piece",
                elabel "unreserved character",
                elabel "username",
                eeof
              ]
          )
    it "rejects Unicode in path" $ do
      let s = "https://github.com/марк"
      parse urip "" s
        `shouldFailWith` err
          19
          ( mconcat
              [ utok 'м',
                etok '#',
                etok '/',
                etok '?',
                elabel "path piece",
                eeof
              ]
          )
    it "parses URIs with sub-domains that look like IPv4" $ do
      scheme <- URI.mkScheme "https"
      host <- URI.mkHost "104.155.144.4.sslip.io"
      let s = "https://104.155.144.4.sslip.io:443/"
      parse urip "" s
        `shouldParse` URI
          { uriScheme = Just scheme,
            uriAuthority =
              Right
                URI.Authority
                  { URI.authUserInfo = Nothing,
                    URI.authHost = host,
                    URI.authPort = Just 443
                  },
            uriPath = Nothing,
            uriQuery = [],
            uriFragment = Nothing
          }
    it "parses URIs with empty authority" $ do
      scheme <- URI.mkScheme "file"
      ppetc <- URI.mkPathPiece "etc"
      pphosts <- URI.mkPathPiece "hosts"
      host <- URI.mkHost ""
      let s = "file:///etc/hosts"
      parse urip "" s
        `shouldParse` URI
          { uriScheme = Just scheme,
            uriAuthority =
              Right
                URI.Authority
                  { URI.authUserInfo = Nothing,
                    URI.authHost = host,
                    URI.authPort = Nothing
                  },
            uriPath = Just (False, ppetc :| [pphosts]),
            uriQuery = [],
            uriFragment = Nothing
          }
    it "parses URIs with superfluous '&' before query parameters" $ do
      uri <- mkTestURI
      let s = "https://mark%3a%40:secret:%40@github.com:443/mrkkrp/modern-uri+%3a@?&foo:@=bar+:@#fragment:@"
      parse urip "" s `shouldParse` uri
    it "treats gracefully percent-encoded sequences that are not UTF-8 encoded Text" $ do
      let s = "https://foo%f0bar"
      parse urip "" s
        `shouldFailWith` err
          8
          ( mconcat
              [ utoks "foo%f0bar",
                etok '%',
                etok '.',
                elabel "unreserved character",
                elabel "host that can be decoded as UTF-8"
              ]
          )
  describe "render" $ do
    it "sort of works" $
      fmap URI.render mkTestURI `shouldReturn` testURI
    context "when URI has scheme" $
      it "does not escape colon in path components" $
        (URI.render <$> URI.mkURI "https:/fir:st/se:cond")
          `shouldReturn` "https:/fir:st/se:cond"
    context "when URI is a network-path reference" $
      it "does not escape colon in path components" $
        (URI.render <$> URI.mkURI "//host/fir:st/se:cond")
          `shouldReturn` "//host/fir:st/se:cond"
    context "when URI is a relative-path reference" $
      it "escapes colon but only in the first path segment" $ do
        firstSeg <- URI.mkPathPiece "fir:st"
        secondSeg <- URI.mkPathPiece "se:cond"
        let uri =
              URI.emptyURI
                { uriPath = Just (False, firstSeg :| [secondSeg])
                }
        URI.render uri `shouldBe` "fir%3ast/se:cond"
    context "when URI has no path" $
      it "is rendered without trailing slash" $ do
        let uriWithoutSlash = "https://example.com"
        uri <- URI.mkURI uriWithoutSlash
        URI.render uri `shouldBe` uriWithoutSlash
    context "when the scheme is mailto" $
      it "does not escape @ in the path" $ do
        let mailtoUri = "mailto:john.smith@example.org"
        uri <- URI.mkURI mailtoUri
        URI.render uri `shouldBe` mailtoUri
  describe "renderBs" $
    it "sort of works" $
      fmap URI.renderBs mkTestURI `shouldReturn` testURI
  describe "renderStr" $
    it "sort of works" $
      fmap URI.renderStr mkTestURI `shouldReturn` testURI
  describe "relativeTo" $ do
    let testResolution r e = do
          base <- URI.mkURI "http://a/b/c/d;p?q"
          reference <- URI.mkURI r
          expected <- URI.mkURI e
          URI.relativeTo reference base `shouldBe` Just expected
    context "when reference URI has no scheme" $
      forM_ resolutionTests $
        \(r, e) ->
          it ("resolves reference path \"" <> T.unpack r <> "\"") $
            testResolution r e
    context "when reference URI has scheme" $ do
      context "when the scheme is the same as the scheme of base URI" $
        it "reference URI is preserved intact" $
          testResolution "http:g" "http:g"
      context "when the scheme is different from the scheme of base URI" $
        it "reference URI is preserved intact" $
          testResolution "ftp:g" "ftp:g"
    context "when base URI has no scheme" $
      it "returns Nothing" $
        property $ \reference base ->
          isNothing (uriScheme base) ==>
            URI.relativeTo reference base `shouldBe` Nothing
    context "when base URI has scheme" $
      it "the resulting URI always has scheme" $
        property $ \reference base ->
          isJust (uriScheme base) ==> do
            let scheme = URI.relativeTo reference base >>= uriScheme
            scheme `shouldSatisfy` isJust

----------------------------------------------------------------------------
-- Helpers

-- | Construct a test URI.
mkTestURI :: IO URI
mkTestURI = do
  scheme <- URI.mkScheme "https"
  username <- URI.mkUsername "mark:@"
  password <- URI.mkPassword "secret:@"
  host <- URI.mkHost "github.com"
  path <- mapM URI.mkPathPiece ["mrkkrp", "modern-uri+:@"]
  k <- URI.mkQueryKey "foo:@"
  v <- URI.mkQueryValue "bar :@"
  fragment <- URI.mkFragment "fragment:@"
  return
    URI
      { uriScheme = Just scheme,
        uriAuthority =
          Right
            URI.Authority
              { URI.authUserInfo =
                  Just
                    URI.UserInfo
                      { URI.uiUsername = username,
                        URI.uiPassword = Just password
                      },
                URI.authHost = host,
                URI.authPort = Just 443
              },
        uriPath = Just (False, NE.fromList path),
        uriQuery = [URI.QueryParam k v],
        uriFragment = Just fragment
      }

-- | Polymorphic textual rendering of the 'URI' generated by 'mkTestURI'.
testURI :: (IsString a) => a
testURI = "https://mark%3a%40:secret:%40@github.com:443/mrkkrp/modern-uri%2b:%40?foo:@=bar+:@#fragment:@"

-- | A utility wrapper around 'URI.parser'.
urip :: Parsec Void Text URI
urip = URI.parser <* eof

-- | Expect that the given action constructs 'URI.RText' with certain text
-- inside.
shouldRText ::
  -- | Action that produces refined text
  IO (URI.RText l) ->
  -- | Inner text to compare with
  Text ->
  Expectation
shouldRText rtext txt = do
  txt' <- rtext
  URI.unRText txt' `shouldBe` txt

-- | Expect that the specified input for parser will produce 'URI' equal to
-- a given one.
shouldParse' ::
  -- | Parser input
  Text ->
  -- | 'URI' to compare with
  URI ->
  Expectation
shouldParse' s a =
  case runParser urip "" s of
    Left e ->
      expectationFailure $
        "the parser is expected to succeed, but it failed with:\n"
          ++ errorBundlePretty e
    Right a' -> a' `shouldBe` a

-- | Similar to 'shouldParse'' but uses 'URI.parserBs' under the hood.
shouldParseBs ::
  -- | Parser input
  ByteString ->
  -- | 'URI' to compare with
  URI ->
  Expectation
shouldParseBs s a =
  case runParser (URI.parserBs <* eof :: Parsec Void ByteString URI) "" s of
    Left e ->
      expectationFailure $
        "the parser is expected to succeed, but it failed with:\n"
          ++ errorBundlePretty e
    Right a' -> a' `shouldBe` a

-- | Test cases from section 5.4.1 from RFC 3986.
--
-- First item in the tuple is the relative path, the second is the expected
-- result. The base path is always @http://a/b/c/d;p?q@.
resolutionTests :: [(Text, Text)]
resolutionTests =
  [ -- Normal examples
    ("g:h", "g:h"),
    ("g", "http://a/b/c/g"),
    ("./g", "http://a/b/c/g"),
    ("g/", "http://a/b/c/g/"),
    ("/g", "http://a/g"),
    ("//g", "http://g"),
    ("?y", "http://a/b/c/d;p?y"),
    ("g?y", "http://a/b/c/g?y"),
    ("#s", "http://a/b/c/d;p?q#s"),
    ("g#s", "http://a/b/c/g#s"),
    ("g?y#s", "http://a/b/c/g?y#s"),
    (";x", "http://a/b/c/;x"),
    ("g;x", "http://a/b/c/g;x"),
    ("g;x?y#s", "http://a/b/c/g;x?y#s"),
    ("", "http://a/b/c/d;p?q"),
    (".", "http://a/b/c/"),
    ("./", "http://a/b/c/"),
    ("..", "http://a/b/"),
    ("../", "http://a/b/"),
    ("../g", "http://a/b/g"),
    ("../..", "http://a/"),
    ("../../", "http://a/"),
    ("../../g", "http://a/g"),
    -- Abnormal cases
    ("../../../g", "http://a/g"),
    ("../../../../g", "http://a/g"),
    -- Dot segments
    ("/./g", "http://a/g"),
    ("/../g", "http://a/g"),
    ("g.", "http://a/b/c/g."),
    (".g", "http://a/b/c/.g"),
    ("g..", "http://a/b/c/g.."),
    ("..g", "http://a/b/c/..g"),
    -- Nonsensical forms of the "." and ".."
    ("./../g", "http://a/b/g"),
    ("./g/.", "http://a/b/c/g/"),
    ("g/./h", "http://a/b/c/g/h"),
    ("g/../h", "http://a/b/c/h"),
    ("g;x=1/./y", "http://a/b/c/g;x=1/y"),
    ("g;x=1/../y", "http://a/b/c/y"),
    -- Query and/or fragment components
    ("g?y/./x", "http://a/b/c/g?y/./x"),
    ("g?y/../x", "http://a/b/c/g?y/../x"),
    ("g#s/./x", "http://a/b/c/g#s/./x"),
    ("g#s/../x", "http://a/b/c/g#s/../x")
  ]
