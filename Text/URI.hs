-- |
-- Module      :  Text.URI
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO Write a proper header here.

{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.URI
  ( -- * Data types
    URI (..)
  , Scheme (..)
  , Authority (..)
  , Host
  , mkHost
  , unHost
  , PathPiece
  , mkPathPiece
  , unPathPiece
  , QueryParam (..)
    -- * Parsing/construction
  , parse
    -- * Rendering
  , render
    -- * Utils
  , normalize
  , percentEscape
  , isReference
  , isRelative )
where

import Control.Monad.Catch
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import GHC.Generics
import Text.Megaparsec hiding (parse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

----------------------------------------------------------------------------
-- Data types

-- | Universal resource identifier (URI).

data URI = URI
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath      :: Maybe (NonEmpty PathPiece)
  , uriQuery     :: [QueryParam]
  , uriFragment  :: Text
  } deriving (Show, Eq, Ord, Generic)

-- | Registered URI schemes.

data Scheme
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
  { authUser :: Maybe (Text, Text) -- ^ User name and password
  , authHost :: Host               -- ^ Host
  , authPort :: Word               -- ^ Port number
  } deriving (Show, Eq, Ord, Generic)

-- | 'URI' host.

newtype Host = Host Text
  deriving (Show, Eq, Ord, Generic) -- TODO Read

mkHost :: MonadThrow m => Text -> m Host
mkHost = undefined

unHost :: Host -> Text
unHost = undefined

-- |

newtype PathPiece = PathPiece Text
  deriving (Show, Eq, Ord, Generic) -- TODO Read

mkPathPiece :: MonadThrow m => Text -> m PathPiece
mkPathPiece = undefined

unPathPiece :: PathPiece -> Text
unPathPiece = undefined

-- |

data QueryParam
  = QueryFlag Text
  | QueryParam Text Text
  deriving (Show, Eq, Ord, Generic)

----------------------------------------------------------------------------
-- Parsing/construction

parse :: MonadParsec e Text m => m URI
parse = undefined -- TODO
{-# INLINEABLE parse #-}

----------------------------------------------------------------------------
-- Rendering

render :: URI -> Text
render = undefined

----------------------------------------------------------------------------
-- Utils

normalize :: URI -> URI
normalize = undefined

percentEscape :: Text -> Text
percentEscape = undefined

isReference :: URI -> Bool
isReference = undefined

isRelative :: URI -> Bool
isRelative = undefined
