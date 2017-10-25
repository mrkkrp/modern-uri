-- |
-- Module      :  Text.URI.Types
-- Copyright   :  © 2017 Mark Karpov
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
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.URI.Types
  ( -- * Data types
    URI (..)
  , makeAbsolute
  , Authority (..)
  , UserInfo (..)
  , QueryParam (..)
    -- * Refined text
  , RText
  , RTextLabel (..)
  , mkScheme
  , mkHost
  , mkUsername
  , mkPassword
  , mkPathPiece
  , mkQueryPiece
  , mkFragment
  , unRText
  , RTextException (..) )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (Exception (..), MonadThrow (..))
import Data.Char
import Data.Data (Data)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text                  as T
import qualified Text.Megaparsec.Char.Lexer as L

----------------------------------------------------------------------------
-- Data types

-- | Uniform resource identifier (URI) reference. We use refined 'Text'
-- (@'RText' @l) here because information is presented in human-readable
-- form, i.e. percent-decoded, and thus it may contain Unicode characters.

data URI = URI
  { uriScheme :: Maybe (RText 'Scheme)
    -- ^ URI scheme, if 'Nothing', then the URI reference is relative
  , uriAuthority :: Maybe Authority
    -- ^ 'Authority' component
  , uriPath :: [RText 'PathPiece]
    -- ^ Path
  , uriQuery :: [QueryParam]
    -- ^ Query parameters
  , uriFragment :: Maybe (RText 'Fragment)
    -- ^ Fragment, without @#@
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Make a given 'URI' reference absolute using the supplied @'RText'
-- 'Scheme'@ if necessary.

makeAbsolute :: RText 'Scheme -> URI -> URI
makeAbsolute scheme URI {..} = URI
  { uriScheme = pure (fromMaybe scheme uriScheme)
  , .. }

-- | Authority component of 'URI'.

data Authority = Authority
  { authUserInfo :: Maybe UserInfo
    -- ^ User information
  , authHost :: RText 'Host
    -- ^ Host
  , authPort :: Maybe Word
    -- ^ Port number
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | User info as a combination of username and password.

data UserInfo = UserInfo
  { uiUsername :: RText 'Username
    -- ^ Username
  , uiPassword :: Maybe (RText 'Password)
    -- ^ Password
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Query parameter either in the form of flag or as a pair of key and
-- value.

data QueryParam
  = QueryFlag (RText 'QueryPiece)
    -- ^ Flag parameter
  | QueryParam (RText 'QueryPiece) (RText 'QueryPiece)
    -- ^ Key–value pair
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

----------------------------------------------------------------------------
-- Refined text

-- | Refined text labelled at the type level.

newtype RText (l :: RTextLabel) = RText Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Refined text labels.

data RTextLabel
  = Scheme             -- ^ See 'mkScheme'
  | Host               -- ^ See 'mkHost'
  | Username           -- ^ See 'mkUsername'
  | Password           -- ^ See 'mkPassword'
  | PathPiece          -- ^ See 'mkPathPiece'
  | QueryPiece         -- ^ See 'mkQueryPiece'
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
      isAscii x && isLetter x
    skipMany . satisfy $ \x ->
      isAscii x && isAlphaNum x || x == '+' || x == '-' || x == '.'
  rnormalize Proxy = T.toLower
  rlabel     Proxy = Scheme

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
  rcheck     Proxy = ifMatches $
    try ipLiteral <|> try ipv4Address <|> regName
    where
      ipLiteral = between (char '[') (char ']') $
        try ipv6Address <|> ipvFuture
      ipv4Address = do
        n <- fmap length . flip sepBy1 (char '.') $ do
          x <- L.decimal
          guard (x < (256 :: Integer))
        guard (n == 4)
      ipv6Address = do
        xs <- flip sepEndBy1 (char ':') $
          count' 0 4 hexDigitChar <*  lookAhead (char ':')
        let nskips  = length (filter null xs)
            npieces = length xs
        guard (nskips < 2)
        guard (npieces == 8 || (nskips == 1 && npieces < 8))
      ipvFuture = do
        void (char 'v')
        void hexDigitChar
        void (char '.')
        skipSome (unreserved <|> subDelim <|> char ':')
      regName = void . flip sepBy1 (char '.') $ do
        void letterChar
        skipMany $ letterChar <|>
          (char '-' <* notFollowedBy eof)
  rnormalize Proxy = T.toLower
  rlabel     Proxy = Host

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

-- | Lift a 'Text' value into @'RText 'QueryPiece'@.
--
-- This smart constructor does not perform any sort of normalization.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-3.4>

mkQueryPiece :: MonadThrow m => Text -> m (RText 'QueryPiece)
mkQueryPiece = mkRText

instance RLabel 'QueryPiece where
  rcheck     Proxy = not . T.null
  rnormalize Proxy = id
  rlabel     Proxy = QueryPiece

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

-- | Project a plain strict 'Text' value from refined @'RText' l@ value.

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

-- | Parser type we use in the helper parsers.

type Parser = Parsec Void Text

-- | Return 'True' if given parser can consume 'Text' in its entirety.

ifMatches :: Parser () -> Text -> Bool
ifMatches p = isJust . parseMaybe p

-- | Parser for unreserved characters as per RFC3986.

unreserved :: Parser Char
unreserved = label "unreserved character" . satisfy $ \x ->
  isAlphaNum x || x == '-' || x == '.' || x == '_' || x == '~'

-- | Match a sub-delimiter.

subDelim :: Parser Char
subDelim = label "sub-delimiter" $ oneOf ("!$&'()*+,;=" :: String)
