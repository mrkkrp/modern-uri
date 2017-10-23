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

module Text.URI
  ( -- * Data types
    Plain (..)
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
  , QueryParam (..)
    -- * Parsing
  , parse
  , parseBs
    -- * Rendering
  , render
  , renderBs )
where

import Text.URI.Types
import Text.URI.Parser
import Text.URI.Render
