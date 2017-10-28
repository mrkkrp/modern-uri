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
    URI (..)
  , mkURI
  , makeAbsolute
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
    -- * Parsing
  , parser
    -- * Rendering
  , render
  , render'
  , renderBs
  , renderBs' )
where

import Text.URI.Parser
import Text.URI.Render
import Text.URI.Types
