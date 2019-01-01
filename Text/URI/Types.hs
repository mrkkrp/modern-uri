-- |
-- Module      :  Text.URI.Types
-- Copyright   :  © 2017–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- 'URI' types, an internal module.

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Text.URI.Types
  ( -- * Data types
    URI (..)
  , makeAbsolute
  , isPathAbsolute
  , Authority (..)
  , UserInfo (..)
  , QueryParam (..)
  , ParseException (..)
    -- * Refined text
  , RText
  , RTextLabel (..)
  , mkScheme
  , mkHost
  , mkUsername
  , mkPassword
  , mkPathPiece
  , mkQueryKey
  , mkQueryValue
  , mkFragment
  , unRText
  , RTextException (..)
    -- * Utils
  , pHost )
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch (Exception (..), MonadThrow (..))
import Data.Char
import Data.Data (Data)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import Data.Word (Word8, Word16)
import GHC.Generics
import Numeric (showInt, showHex)
import Test.QuickCheck
import Text.Megaparsec
import Text.URI.Parser.Text.Utils (pHost)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

----------------------------------------------------------------------------
-- Data types

-- | Uniform resource identifier (URI) reference. We use refined 'Text'
-- (@'RText' l@) here because information is presented in human-readable
-- form, i.e. percent-decoded, and thus it may contain Unicode characters.

data URI = URI
  { uriScheme :: Maybe (RText 'Scheme)
    -- ^ URI scheme, if 'Nothing', then the URI reference is relative
  , uriAuthority :: Either Bool Authority
    -- ^ 'Authority' component in 'Right' or a 'Bool' value in 'Left'
    -- indicating if 'uriPath' path is absolute ('True') or relative
    -- ('False'); if we have an 'Authority' component, then the path is
    -- necessarily absolute, see 'isPathAbsolute'
    --
    -- __Note__: before version /0.1.0.0/ type of 'uriAuthority' was
    -- @'Maybe' 'Authority'@
  , uriPath :: Maybe (Bool, NonEmpty (RText 'PathPiece))
    -- ^ 'Nothing' represents the empty path, while 'Just' contains an
    -- indication 'Bool' whether the path component has a trailing slash,
    -- and the collection of path pieces @'NonEmpty' ('RText' 'PathPiece')@.
    --
    -- __Note__: before version /0.2.0.0/ type of 'uriPath' was @['RText'
    -- 'PathPiece']@.
  , uriQuery :: [QueryParam]
    -- ^ Query parameters, RFC 3986 does not define the inner organization
    -- of query string, so we deconstruct it following RFC 1866 here
  , uriFragment :: Maybe (RText 'Fragment)
    -- ^ Fragment, without @#@
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary URI where
  arbitrary = URI
    <$> arbitrary
    <*> arbitrary
    <*> (do mpieces <- NE.nonEmpty <$> arbitrary
            trailingSlash <- arbitrary
            return ((trailingSlash,) <$> mpieces))
    <*> arbitrary
    <*> arbitrary

instance NFData URI

-- | Make a given 'URI' reference absolute using the supplied @'RText'
-- 'Scheme'@ if necessary.

makeAbsolute :: RText 'Scheme -> URI -> URI
makeAbsolute scheme URI {..} = URI
  { uriScheme = pure (fromMaybe scheme uriScheme)
  , .. }

-- | Return 'True' if path in a given 'URI' is absolute.
--
-- @since 0.1.0.0

isPathAbsolute :: URI -> Bool
isPathAbsolute = either id (const True) . uriAuthority

-- | Authority component of 'URI'.

data Authority = Authority
  { authUserInfo :: Maybe UserInfo
    -- ^ User information
  , authHost :: RText 'Host
    -- ^ Host
  , authPort :: Maybe Word
    -- ^ Port number
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary Authority where
  arbitrary = Authority
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance NFData Authority

-- | User info as a combination of username and password.

data UserInfo = UserInfo
  { uiUsername :: RText 'Username
    -- ^ Username
  , uiPassword :: Maybe (RText 'Password)
    -- ^ Password, 'Nothing' means that there was no @:@ character in the
    -- user info string
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary UserInfo where
  arbitrary = UserInfo
    <$> arbitrary
    <*> arbitrary

instance NFData UserInfo

-- | Query parameter either in the form of flag or as a pair of key and
-- value. A key cannot be empty, while a value can.

data QueryParam
  = QueryFlag (RText 'QueryKey)
    -- ^ Flag parameter
  | QueryParam (RText 'QueryKey) (RText 'QueryValue)
    -- ^ Key–value pair
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary QueryParam where
  arbitrary = oneof
    [ QueryFlag  <$> arbitrary
    , QueryParam <$> arbitrary <*> arbitrary ]

instance NFData QueryParam

-- | Parse exception thrown by 'mkURI' when a given 'Text' value cannot be
-- parsed as a 'URI'.

newtype ParseException = ParseException (ParseErrorBundle Text Void)
  -- ^ Arguments are: original input and parse error
  deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ParseException where
  displayException (ParseException b) = errorBundlePretty b

instance NFData ParseException

----------------------------------------------------------------------------
-- Refined text

-- | Refined text labelled at the type level.

newtype RText (l :: RTextLabel) = RText Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show (RText l) where
  show (RText txt) = show txt

instance NFData (RText l) where

-- | Refined text labels.

data RTextLabel
  = Scheme             -- ^ See 'mkScheme'
  | Host               -- ^ See 'mkHost'
  | Username           -- ^ See 'mkUsername'
  | Password           -- ^ See 'mkPassword'
  | PathPiece          -- ^ See 'mkPathPiece'
  | QueryKey           -- ^ See 'mkQueryKey'
  | QueryValue         -- ^ See 'mkQueryValue'
  | Fragment           -- ^ See 'mkFragment'
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | This type class associates checking, normalization, and a term level
-- label with a label on the type level.
--
-- We would like to have a closed type class here, and so we achieve almost
-- that by not exporting 'RLabel' and 'mkRText' (only specialized helpers
-- like 'mkScheme').

class RLabel (l :: RTextLabel) where
  rcheck     :: Proxy l -> Text -> Bool
  rnormalize :: Proxy l -> Text -> Text
  rlabel     :: Proxy l -> RTextLabel

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

mkScheme :: MonadThrow m => Text -> m (RText 'Scheme)
mkScheme = mkRText

instance RLabel 'Scheme where
  rcheck     Proxy = ifMatches $ do
    void . satisfy $ \x ->
      isAscii x && isAlpha x
    skipMany . satisfy $ \x ->
      isAscii x && isAlphaNum x || x == '+' || x == '-' || x == '.'
  rnormalize Proxy = T.toLower
  rlabel     Proxy = Scheme

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

mkHost :: MonadThrow m => Text -> m (RText 'Host)
mkHost = mkRText

instance RLabel 'Host where
  rcheck     Proxy = (ifMatches . void . pHost) False
  rnormalize Proxy = T.toLower
  rlabel     Proxy = Host

instance Arbitrary (RText 'Host) where
  arbitrary = arbHost

-- | Lift a 'Text' value into @'RText' 'Username'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.2.1>

mkUsername :: MonadThrow m => Text -> m (RText 'Username)
mkUsername = mkRText

instance RLabel 'Username where
  rcheck     Proxy = not . T.null
  rnormalize Proxy = id
  rlabel     Proxy = Username

instance Arbitrary (RText 'Username) where
  arbitrary = arbText' mkUsername

-- | Lift a 'Text' value into @'RText' 'Password'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.2.1>

mkPassword :: MonadThrow m => Text -> m (RText 'Password)
mkPassword = mkRText

instance RLabel 'Password where
  rcheck     Proxy = const True
  rnormalize Proxy = id
  rlabel     Proxy = Password

instance Arbitrary (RText 'Password) where
  arbitrary = arbText mkPassword

-- | Lift a 'Text' value into @'RText' 'PathPiece'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.3>

mkPathPiece :: MonadThrow m => Text -> m (RText 'PathPiece)
mkPathPiece = mkRText

instance RLabel 'PathPiece where
  rcheck     Proxy = not . T.null
  rnormalize Proxy = id
  rlabel     Proxy = PathPiece

instance Arbitrary (RText 'PathPiece) where
  arbitrary = arbText' mkPathPiece

-- | Lift a 'Text' value into @'RText 'QueryKey'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.4>

mkQueryKey :: MonadThrow m => Text -> m (RText 'QueryKey)
mkQueryKey = mkRText

instance RLabel 'QueryKey where
  rcheck     Proxy = not . T.null
  rnormalize Proxy = id
  rlabel     Proxy = QueryKey

instance Arbitrary (RText 'QueryKey) where
  arbitrary = arbText' mkQueryKey

-- | Lift a 'Text' value into @'RText' 'QueryValue'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.4>

mkQueryValue :: MonadThrow m => Text -> m (RText 'QueryValue)
mkQueryValue = mkRText

instance RLabel 'QueryValue where
  rcheck     Proxy = const True
  rnormalize Proxy = id
  rlabel     Proxy = QueryValue

instance Arbitrary (RText 'QueryValue) where
  arbitrary = arbText mkQueryValue

-- | Lift a 'Text' value into @'RText' 'Fragment'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.5>

mkFragment :: MonadThrow m => Text -> m (RText 'Fragment)
mkFragment = mkRText

instance RLabel 'Fragment where
  rcheck     Proxy = const True
  rnormalize Proxy = id
  rlabel     Proxy = Fragment

instance Arbitrary (RText 'Fragment) where
  arbitrary = arbText mkFragment

-- | Project a plain strict 'Text' value from a refined @'RText' l@ value.

unRText :: RText l -> Text
unRText (RText txt) = txt

-- | The exception is thrown when a refined @'RText' l@ value cannot be
-- constructed due to the fact that given 'Text' value is not correct.

data RTextException = RTextException RTextLabel Text
  -- ^ 'RTextLabel' identifying what sort of refined text value could not be
  -- constructed and the input that was supplied, as a 'Text' value
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Exception RTextException where
  displayException (RTextException lbl txt) = "The value \"" ++
    T.unpack txt ++ "\" could not be lifted into a " ++ show lbl

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
  let g = oneof [choose ('a','z'), choose ('A','Z')]
  x  <- g
  xs <- listOf $
    frequency [(3, g), (1, choose ('0','9'))]
  return . fromJust . mkScheme . T.pack $ x:xs

-- | Generator of 'Arbitrary' hosts.

arbHost :: Gen (RText 'Host)
arbHost = fromJust . mkHost . T.pack <$> frequency
  [ (1, ipLiteral)
  , (2, ipv4Address)
  , (4, regName)
  , (1, return "")
  ]
  where
    ipLiteral = do
      xs <- oneof [ipv6Address, ipvFuture]
      return ("[" ++ xs ++ "]")
    ipv6Address =
      -- NOTE We do not mess with zeroes here, because it's a hairy stuff.
      -- We test how we handle :: thing manually in the test suite.
      intercalate ":" . fmap (`showHex` "") <$>
        vectorOf 8 (arbitrary :: Gen Word16)
    ipv4Address =
      intercalate "." . fmap (`showInt` "") <$>
        vectorOf 4 (arbitrary :: Gen Word8)
    ipvFuture = do
      v  <- oneof [choose ('0', '9'), choose ('a', 'f')]
      xs <- listOf1 $ frequency
        [ (3, choose ('a', 'z'))
        , (3, choose ('A', 'Z'))
        , (2, choose ('0', '9'))
        , (2, elements "-._~!$&'()*+,;=:") ]
      return ("v" ++ [v] ++ "." ++ xs)
    domainLabel = do
      let g = arbitrary `suchThat` isAlphaNum
      x  <- g
      xs <- listOf $
        frequency [(3, g), (1, return '-')]
      x' <- g
      return ([x] ++ xs ++ [x'])
    regName = intercalate "." <$> resize 5 (listOf1 domainLabel)

-- | Make generator for refined text given how to lift a possibly empty
-- arbitrary 'Text' value into a refined type.

arbText :: (Text -> Maybe (RText l)) -> Gen (RText l)
arbText f = fromJust . f . T.pack <$> listOf arbitrary

-- | Like 'arbText'', but the lifting function will be given non-empty
-- arbitrary 'Text' value.

arbText' :: (Text -> Maybe (RText l)) -> Gen (RText l)
arbText' f = fromJust . f . T.pack <$> listOf1 arbitrary
