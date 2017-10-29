-- |
-- Module      :  Text.URI
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is modern library for working with URIs as per RFC 3986:
--
-- <https://tools.ietf.org/html/rfc3986>
--
-- This module is intended to be imported qualified, e.g.:
--
-- > import Text.URI (URI)
-- > import qualified Text.URI as URI
--
-- See also "Text.URI.Lens" for lens, prisms, and traversals; see
-- "Text.URI.QQ" for quasi-quoters for compile-time validation of URIs and
-- refined text components.

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
    -- $rtext
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
  , parserBs
    -- * Rendering
  , render
  , render'
  , renderBs
  , renderBs'
  , renderStr
  , renderStr' )
where

import Text.URI.Parser.ByteString
import Text.URI.Parser.Text
import Text.URI.Render
import Text.URI.Types

-- $rtext
--
-- Refined text values can only be created by using the smart constructors
-- listed below, such as 'mkScheme'. This eliminates the possibility of
-- having an invalid component in 'URI' which could invalidate the whole
-- 'URI'.
--
-- Note that the refined text 'RText' type is labelled at the type level
-- with 'RTextLabel's, which see.
--
-- When an invalid 'Data.Text.Text' value is passed to a smart constructor,
-- it rejects it by throwing the 'RTextException'. Remember that the 'Maybe'
-- datatype is also an instance of 'Control.Monad.Catch.MonadThrow', and so
-- one could as well use the smart constructors in the 'Maybe' monad.
