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

module Text.URI.Types
  ( Plain (..)
  , Normalized
  , normalize
  , Scheme (..)
  , Authority (..)
  , UserInfo (..)
  , Host
  , mkHost
  , unHost
  , PathPiece
  , mkPathPiece
  , unPathPiece
  , QueryParam (..) )
where

import Control.Monad.Catch
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

-- TODO Don't forget data and typeable here.

-- | Universal resource identifier (URI). We use 'Text' here because
-- information is presented in human-readable form, i.e. percent-decoded,
-- and it may contain Unicode characters. When you render a 'URI' it's
-- percent-encoded and normalized as necessary for you.

data Plain = Plain
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath      :: Maybe (NonEmpty PathPiece)
  , uriQuery     :: [QueryParam]
  , uriFragment  :: Text
  } deriving (Show, Eq, Ord, Generic) -- TODO Read

-- |

newtype Normalized = Normalized Plain
  deriving (Show, Eq, Ord, Generic)

-- |

normalize :: Plain -> Normalized
normalize = undefined -- TODO

-- | Registered URI schemes.

data Scheme -- TODO extend this enumeration to support all registered
     -- schemes as per https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
  = SchemeData         -- ^ @data@
  | SchemeFile         -- ^ @file@
  | SchemeFtp          -- ^ @ftp@
  | SchemeHttp         -- ^ @http@
  | SchemeHttps        -- ^ @https@
  | SchemeIrc          -- ^ @irc@
  | SchemeMailto       -- ^ @mailto@
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

-- | Authority component of 'URI'.

data Authority = Authority
  { authUser :: Maybe UserInfo     -- ^ User name and password
  , authHost :: Host               -- ^ Host
  , authPort :: Word               -- ^ Port number
  } deriving (Show, Eq, Ord, Generic) -- TODO Read

data UserInfo = UserInfo
  { uiUsername :: Text
  , uiPassword :: Text
  } deriving (Show, Eq, Ord, Generic)

-- | 'URI' host.

newtype Host = Host Text
  deriving (Show, Eq, Ord, Generic) -- TODO Read

mkHost :: MonadThrow m => Text -> m Host
mkHost = undefined -- TODO

unHost :: Host -> Text
unHost = undefined -- TODO

-- |

newtype PathPiece = PathPiece Text
  deriving (Show, Eq, Ord, Generic) -- TODO Read

mkPathPiece :: MonadThrow m => Text -> m PathPiece
mkPathPiece = undefined -- TODO

unPathPiece :: PathPiece -> Text
unPathPiece = undefined -- TODO

-- |

data QueryParam
  = QueryFlag Text
  | QueryParam Text Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
