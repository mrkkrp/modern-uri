{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Text.URI.Types
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- 'URI' types, an internal module.
module Text.URI.Types
  ( -- * Data types
    URI (..),
    makeAbsolute,
    isPathAbsolute,
    Authority (..),
    UserInfo (..),
    QueryParam (..),
    ParseException (..),
    ParseExceptionBs (..),

    -- * Refined text
    RText,
    RTextLabel (..),
    mkScheme,
    mkHost,
    mkUsername,
    mkPassword,
    mkPathPiece,
    mkQueryKey,
    mkQueryValue,
    mkFragment,
    unRText,
    RTextException (..),

    -- * Utils
    pHost,
  )
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch (Exception (..), MonadThrow (..))
import Data.ByteString (ByteString)
import Data.Char
import Data.Data (Data)
import Data.Either (fromLeft)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Data.Void
import Data.Word (Word16, Word8)
import GHC.Generics
import qualified Language.Haskell.TH.Syntax as TH
import Numeric (showHex, showInt)
import Test.QuickCheck
import Text.Megaparsec
import Text.URI.Parser.Text.Utils (pHost)

----------------------------------------------------------------------------
-- Data types

-- | Uniform resource identifier (URI) reference. We use refined 'Text'
-- (@'RText' l@) here because information is presented in human-readable
-- form, i.e. percent-decoded, and thus it may contain Unicode characters.
data URI = URI
  { -- | URI scheme, if 'Nothing', then the URI reference is relative
    uriScheme :: Maybe (RText 'Scheme),
    -- | 'Authority' component in 'Right' or a 'Bool' value in 'Left'
    -- indicating if 'uriPath' path is absolute ('True') or relative
    -- ('False'); if we have an 'Authority' component, then the path is
    -- necessarily absolute, see 'isPathAbsolute'
    --
    -- __Note__: before version /0.1.0.0/ type of 'uriAuthority' was
    -- @'Maybe' 'Authority'@
    uriAuthority :: Either Bool Authority,
    -- | 'Nothing' represents the empty path, while 'Just' contains an
    -- indication 'Bool' whether the path component has a trailing slash,
    -- and the collection of path pieces @'NonEmpty' ('RText' 'PathPiece')@.
    --
    -- __Note__: before version /0.2.0.0/ type of 'uriPath' was @['RText'
    -- 'PathPiece']@.
    uriPath :: Maybe (Bool, NonEmpty (RText 'PathPiece)),
    -- | Query parameters, RFC 3986 does not define the inner organization
    -- of query string, so we deconstruct it following RFC 1866 here
    uriQuery :: [QueryParam],
    -- | Fragment, without @#@
    uriFragment :: Maybe (RText 'Fragment)
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | @since 0.3.5.0
instance Hashable URI

instance Arbitrary URI where
  arbitrary =
    URI
      <$> arbitrary
      <*> arbitrary
      <*> ( do
              mpieces <- NE.nonEmpty <$> arbitrary
              trailingSlash <- arbitrary
              return ((trailingSlash,) <$> mpieces)
          )
      <*> arbitrary
      <*> arbitrary

instance NFData URI

-- | @since 0.3.1.0
instance TH.Lift URI where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Make a given 'URI' reference absolute using the supplied @'RText'
-- 'Scheme'@ if necessary.
makeAbsolute :: RText 'Scheme -> URI -> URI
makeAbsolute scheme URI {..} =
  URI
    { uriScheme = pure (fromMaybe scheme uriScheme),
      ..
    }

-- | Return 'True' if path in a given 'URI' is absolute.
--
-- @since 0.1.0.0
isPathAbsolute :: URI -> Bool
isPathAbsolute = fromLeft True . uriAuthority

-- | Authority component of 'URI'.
data Authority = Authority
  { -- | User information
    authUserInfo :: Maybe UserInfo,
    -- | Host
    authHost :: RText 'Host,
    -- | Port number
    authPort :: Maybe Word
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | @since 0.3.5.0
instance Hashable Authority

instance Arbitrary Authority where
  arbitrary =
    Authority
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance NFData Authority

-- | @since 0.3.1.0
instance TH.Lift Authority where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | User info as a combination of username and password.
data UserInfo = UserInfo
  { -- | Username
    uiUsername :: RText 'Username,
    -- | Password, 'Nothing' means that there was no @:@ character in the
    -- user info string
    uiPassword :: Maybe (RText 'Password)
  }
  deriving (Show, Eq, Ord, Data, Generic)

-- | @since 0.3.5.0
instance Hashable UserInfo

instance Arbitrary UserInfo where
  arbitrary =
    UserInfo
      <$> arbitrary
      <*> arbitrary

instance NFData UserInfo

-- | @since 0.3.1.0
instance TH.Lift UserInfo where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Query parameter either in the form of flag or as a pair of key and
-- value. A key cannot be empty, while a value can.
data QueryParam
  = -- | Flag parameter
    QueryFlag (RText 'QueryKey)
  | -- | Key–value pair
    QueryParam (RText 'QueryKey) (RText 'QueryValue)
  deriving (Show, Eq, Ord, Data, Generic)

-- | @since 0.3.5.0
instance Hashable QueryParam

instance Arbitrary QueryParam where
  arbitrary =
    oneof
      [ QueryFlag <$> arbitrary,
        QueryParam <$> arbitrary <*> arbitrary
      ]

instance NFData QueryParam

-- | @since 0.3.1.0
instance TH.Lift QueryParam where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Parse exception thrown by 'mkURI' when a given 'Text' value cannot be
-- parsed as a 'URI'.
newtype ParseException
  = -- | Arguments are: original input and parse error
    ParseException (ParseErrorBundle Text Void)
  deriving (Show, Eq, Data, Generic)

instance Exception ParseException where
  displayException (ParseException b) = errorBundlePretty b

instance NFData ParseException

-- | Parse exception thrown by 'mkURIBs' when a given 'ByteString' value cannot be
-- parsed as a 'URI'.
--
-- @since 0.3.3.0
newtype ParseExceptionBs
  = -- | Arguments are: original input and parse error
    ParseExceptionBs (ParseErrorBundle ByteString Void)
  deriving (Show, Eq, Data, Generic)

instance Exception ParseExceptionBs where
  displayException (ParseExceptionBs b) = errorBundlePretty b

instance NFData ParseExceptionBs

----------------------------------------------------------------------------
-- Refined text

-- | Refined text labelled at the type level.
newtype RText (l :: RTextLabel) = RText Text
  deriving (Eq, Ord, Data, Generic)

-- | @since 0.3.5.0
instance Hashable (RText l)

instance Show (RText l) where
  show (RText txt) = show txt

instance NFData (RText l)

-- | @since 0.3.1.0
instance (Typeable l) => TH.Lift (RText l) where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Refined text labels.
data RTextLabel
  = -- | See 'mkScheme'
    Scheme
  | -- | See 'mkHost'
    Host
  | -- | See 'mkUsername'
    Username
  | -- | See 'mkPassword'
    Password
  | -- | See 'mkPathPiece'
    PathPiece
  | -- | See 'mkQueryKey'
    QueryKey
  | -- | See 'mkQueryValue'
    QueryValue
  | -- | See 'mkFragment'
    Fragment
  deriving (Show, Eq, Ord, Data, Generic)

-- | This type class associates checking, normalization, and a term level
-- label with a label on the type level.
--
-- We would like to have a closed type class here, and so we achieve almost
-- that by not exporting 'RLabel' and 'mkRText' (only specialized helpers
-- like 'mkScheme').
class RLabel (l :: RTextLabel) where
  rcheck :: Proxy l -> Text -> Bool
  rnormalize :: Proxy l -> Text -> Text
  rlabel :: Proxy l -> RTextLabel

-- | Construct a refined text value.
mkRText :: forall m l. (MonadThrow m, RLabel l) => Text -> m (RText l)
mkRText txt =
  if rcheck lproxy txt
    then return . RText $ rnormalize lproxy txt
    else throwM (RTextException (rlabel lproxy) txt)
  where
    lproxy = Proxy :: Proxy l

-- | Lift a 'Text' value into @'RText' 'Scheme'@.
--
-- Scheme names consist of a sequence of characters beginning with a letter
-- and followed by any combination of letters, digits, plus @\"+\"@, period
-- @\".\"@, or hyphen @\"-\"@.
--
-- This smart constructor performs normalization of valid schemes by
-- converting them to lower case.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.1>
mkScheme :: (MonadThrow m) => Text -> m (RText 'Scheme)
mkScheme = mkRText

instance RLabel 'Scheme where
  rcheck Proxy = ifMatches $ do
    void . satisfy $ \x ->
      isAscii x && isAlpha x
    skipMany . satisfy $ \x ->
      isAscii x && isAlphaNum x || x == '+' || x == '-' || x == '.'
  rnormalize Proxy = T.toLower
  rlabel Proxy = Scheme

instance Arbitrary (RText 'Scheme) where
  arbitrary = arbScheme

-- | Lift a 'Text' value into @'RText' 'Host'@.
--
-- The host sub-component of authority is identified by an IP literal
-- encapsulated within square brackets, an IPv4 address in dotted-decimal
-- form, or a registered name.
--
-- This smart constructor performs normalization of valid hosts by
-- converting them to lower case.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.2.2>
mkHost :: (MonadThrow m) => Text -> m (RText 'Host)
mkHost = mkRText

instance RLabel 'Host where
  rcheck Proxy = (ifMatches . void . pHost) False
  rnormalize Proxy = T.toLower
  rlabel Proxy = Host

instance Arbitrary (RText 'Host) where
  arbitrary = arbHost

-- | Lift a 'Text' value into @'RText' 'Username'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.2.1>
mkUsername :: (MonadThrow m) => Text -> m (RText 'Username)
mkUsername = mkRText

instance RLabel 'Username where
  rcheck Proxy = not . T.null
  rnormalize Proxy = id
  rlabel Proxy = Username

instance Arbitrary (RText 'Username) where
  arbitrary = arbText' mkUsername

-- | Lift a 'Text' value into @'RText' 'Password'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.2.1>
mkPassword :: (MonadThrow m) => Text -> m (RText 'Password)
mkPassword = mkRText

instance RLabel 'Password where
  rcheck Proxy = const True
  rnormalize Proxy = id
  rlabel Proxy = Password

instance Arbitrary (RText 'Password) where
  arbitrary = arbText mkPassword

-- | Lift a 'Text' value into @'RText' 'PathPiece'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.3>
mkPathPiece :: (MonadThrow m) => Text -> m (RText 'PathPiece)
mkPathPiece = mkRText

instance RLabel 'PathPiece where
  rcheck Proxy = not . T.null
  rnormalize Proxy = id
  rlabel Proxy = PathPiece

instance Arbitrary (RText 'PathPiece) where
  arbitrary = arbText' mkPathPiece

-- | Lift a 'Text' value into @'RText 'QueryKey'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.4>
mkQueryKey :: (MonadThrow m) => Text -> m (RText 'QueryKey)
mkQueryKey = mkRText

instance RLabel 'QueryKey where
  rcheck Proxy = not . T.null
  rnormalize Proxy = id
  rlabel Proxy = QueryKey

instance Arbitrary (RText 'QueryKey) where
  arbitrary = arbText' mkQueryKey

-- | Lift a 'Text' value into @'RText' 'QueryValue'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.4>
mkQueryValue :: (MonadThrow m) => Text -> m (RText 'QueryValue)
mkQueryValue = mkRText

instance RLabel 'QueryValue where
  rcheck Proxy = const True
  rnormalize Proxy = id
  rlabel Proxy = QueryValue

instance Arbitrary (RText 'QueryValue) where
  arbitrary = arbText mkQueryValue

-- | Lift a 'Text' value into @'RText' 'Fragment'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.5>
mkFragment :: (MonadThrow m) => Text -> m (RText 'Fragment)
mkFragment = mkRText

instance RLabel 'Fragment where
  rcheck Proxy = const True
  rnormalize Proxy = id
  rlabel Proxy = Fragment

instance Arbitrary (RText 'Fragment) where
  arbitrary = arbText mkFragment

-- | Project a plain strict 'Text' value from a refined @'RText' l@ value.
unRText :: RText l -> Text
unRText (RText txt) = txt

-- | The exception is thrown when a refined @'RText' l@ value cannot be
-- constructed due to the fact that given 'Text' value is not correct.
data RTextException
  = -- | 'RTextLabel' identifying what sort of refined text value could not be
    -- constructed and the input that was supplied, as a 'Text' value
    RTextException RTextLabel Text
  deriving (Show, Eq, Ord, Data, Generic)

instance Exception RTextException where
  displayException (RTextException lbl txt) =
    "The value \""
      ++ T.unpack txt
      ++ "\" could not be lifted into a "
      ++ show lbl

----------------------------------------------------------------------------
-- Parser helpers

-- | Return 'True' if given parser can consume 'Text' in its entirety.
ifMatches :: Parsec Void Text () -> Text -> Bool
ifMatches p = isJust . parseMaybe p

----------------------------------------------------------------------------
-- Arbitrary helpers

-- | Generator of 'Arbitrary' schemes.
arbScheme :: Gen (RText 'Scheme)
arbScheme = do
  let g = oneof [choose ('a', 'z'), choose ('A', 'Z')]
  x <- g
  xs <-
    listOf $
      frequency [(3, g), (1, choose ('0', '9'))]
  return . fromJust . mkScheme . T.pack $ x : xs

-- | Generator of 'Arbitrary' hosts.
arbHost :: Gen (RText 'Host)
arbHost =
  fromJust . mkHost . T.pack
    <$> frequency
      [ (1, ipLiteral),
        (2, ipv4Address),
        (4, regName),
        (1, return "")
      ]
  where
    ipLiteral = do
      xs <- oneof [ipv6Address, ipvFuture]
      return ("[" ++ xs ++ "]")
    ipv6Address =
      -- NOTE We do not mess with zeroes here, because it's a hairy stuff.
      -- We test how we handle :: thing manually in the test suite.
      intercalate ":" . fmap (`showHex` "")
        <$> vectorOf 8 (arbitrary :: Gen Word16)
    ipv4Address =
      intercalate "." . fmap (`showInt` "")
        <$> vectorOf 4 (arbitrary :: Gen Word8)
    ipvFuture = do
      v <- oneof [choose ('0', '9'), choose ('a', 'f')]
      xs <-
        listOf1 $
          frequency
            [ (3, choose ('a', 'z')),
              (3, choose ('A', 'Z')),
              (2, choose ('0', '9')),
              (2, elements "-._~!$&'()*+,;=:")
            ]
      return ("v" ++ [v] ++ "." ++ xs)
    domainLabel = do
      let g = arbitrary `suchThat` isUnreservedChar
      x <- g
      xs <-
        listOf $
          frequency [(3, g), (1, return '-')]
      x' <- g
      return ([x] ++ xs ++ [x'])
    regName = intercalate "." <$> resize 5 (listOf1 domainLabel)

-- | Return 'True' if the given character is unreserved.
isUnreservedChar :: Char -> Bool
isUnreservedChar x =
  isAlphaNum x || x == '-' || x == '.' || x == '_' || x == '~'

-- | Make generator for refined text given how to lift a possibly empty
-- arbitrary 'Text' value into a refined type.
arbText :: (Text -> Maybe (RText l)) -> Gen (RText l)
arbText f = fromJust . f . T.pack <$> listOf arbitrary

-- | Like 'arbText'', but the lifting function will be given non-empty
-- arbitrary 'Text' value.
arbText' :: (Text -> Maybe (RText l)) -> Gen (RText l)
arbText' f = fromJust . f . T.pack <$> listOf1 arbitrary

----------------------------------------------------------------------------
-- TH lifting helpers

liftData :: (Data a, TH.Quote m) => a -> m TH.Exp
liftData = TH.dataToExpQ (fmap liftText . cast)

liftText :: (TH.Quote m) => Text -> m TH.Exp
liftText t = TH.AppE (TH.VarE 'T.pack) <$> TH.lift (T.unpack t)
