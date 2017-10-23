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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.URI.Types
  ( URI (..)
  , makeAbsolute
  , Scheme
  , mkScheme
  , unScheme
  , SchemeException (..)
  , Authority (..)
  , UserInfo (..)
  , Host
  , mkHost
  , unHost
  , HostException (..)
  , PathPiece
  , mkPathPiece
  , unPathPiece
  , PathPieceException (..)
  , QueryParam (..) )
where

import Control.Monad.Catch
import Data.Char
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import qualified Data.Text as T

-- | Uniform resource identifier (URI) reference. We use 'Text' here because
-- information is presented in human-readable form, i.e. percent-decoded,
-- and thus it may contain Unicode characters.

data URI = URI
  { uriScheme :: Maybe Scheme
    -- ^ URI scheme, if 'Nothing', then the URI reference is relative
  , uriAuthority :: Maybe Authority
    -- ^ 'Authority' component
  , uriPath :: [PathPiece]
    -- ^ Path
  , uriQuery :: [QueryParam]
    -- ^ Query parameters
  , uriFragment :: Text
    -- ^ Fragment, without @#@
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Make a given 'Plain' URI reference absolute using the supplied 'Scheme'
-- if necessary.

makeAbsolute :: Scheme -> URI -> URI
makeAbsolute scheme URI {..} = URI
  { uriScheme = pure (fromMaybe scheme uriScheme)
  , .. }

-- | URI scheme.

newtype Scheme = Scheme Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Lift a 'Text' value into 'Scheme'.

mkScheme :: MonadThrow m => Text -> m Scheme
mkScheme scheme =
  case T.uncons scheme of
    Nothing -> giveup
    Just (h, r) ->
      if isLetter h && isAscii h && T.all validSchemeChar r
        then (return . Scheme . T.toLower) scheme
        else giveup
  where
    giveup = throwM (SchemeException scheme)
    validSchemeChar = undefined

-- | Extract a 'Text' value representing normalized URI scheme.

unScheme :: Scheme -> Text
unScheme (Scheme scheme) = scheme

-- | Exception that is thrown by 'mkScheme' when its argument is not a valid
-- scheme.

data SchemeException = SchemeException Text
  deriving (Eq, Show, Typeable, Data, Generic)

instance Exception SchemeException

-- | Authority component of 'URI'.

data Authority = Authority
  { authUser :: Maybe UserInfo -- ^ User name and password
  , authHost :: Host           -- ^ Host
  , authPort :: Maybe Word     -- ^ Port number
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | User info as a combination of username and password.

data UserInfo = UserInfo
  { uiUsername :: Text -- ^ Username FIXME
  , uiPassword :: Text -- ^ Password FIXME
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | 'URI' host.

newtype Host = Host Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Lift a 'Text' value into 'Host'.

mkHost :: MonadThrow m => Text -> m Host
mkHost = undefined -- TODO

-- | Extract a 'Text' value representing normalized host name.

unHost :: Host -> Text
unHost (Host host) = host

-- | Exception that is thrown by 'mkHost' when its argument is not a valid
-- host.

data HostException = HostException Text
  deriving (Eq, Show, Typeable, Data, Generic)

instance Exception HostException

-- | Path piece.

newtype PathPiece = PathPiece Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Lift a 'Text' value into 'PathPiece'.

mkPathPiece :: MonadThrow m => Text -> m PathPiece
mkPathPiece = undefined -- TODO

-- | Extract a 'Text' value representing path piece.

unPathPiece :: PathPiece -> Text
unPathPiece (PathPiece piece) = piece

data PathPieceException = PathPieceException Text
  deriving (Eq, Show, Typeable, Data, Generic)

instance Exception PathPieceException

-- | Query parameter either in the form of flag or as a pair key–value.

data QueryParam
  = QueryFlag Text       -- ^ Flag parameter
  | QueryParam Text Text -- ^ Key–value pair
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
