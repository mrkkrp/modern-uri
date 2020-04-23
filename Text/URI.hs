{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Text.URI
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a modern library for working with URIs as per RFC 3986:
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
    URI (..),
    mkURI,
    emptyURI,
    makeAbsolute,
    isPathAbsolute,
    relativeTo,
    Authority (..),
    UserInfo (..),
    QueryParam (..),
    ParseException (..),

    -- * Refined text
    -- $rtext
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

    -- * Parsing
    -- $parsing
    parser,
    parserBs,

    -- * Rendering
    -- $rendering
    render,
    render',
    renderBs,
    renderBs',
    renderStr,
    renderStr',
  )
where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Text.URI.Parser.ByteString
import Text.URI.Parser.Text
import Text.URI.Render
import Text.URI.Types

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

-- | The empty 'URI'.
--
-- @since 0.2.1.0
emptyURI :: URI
emptyURI =
  URI
    { uriScheme = Nothing,
      uriAuthority = Left False,
      uriPath = Nothing,
      uriQuery = [],
      uriFragment = Nothing
    }

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

-- $parsing
--
-- The input you feed into the parsers must be a valid URI as per RFC 3986,
-- that is, its components should be percent-encoded where necessary.

-- $rendering
--
-- Rendering functions take care of constructing correct 'URI'
-- representation as per RFC 3986, that is, percent-encoding will be applied
-- when necessary automatically.

-- | @'relativeTo' reference base@ makes the @reference@ 'URI' absolute
-- resolving it against the @base@ 'URI'.
--
-- If the base 'URI' is not absolute itself (that is, it has no scheme),
-- this function returns 'Nothing'.
--
-- See also: <https://tools.ietf.org/html/rfc3986#section-5.2>.
--
-- @since 0.2.0.0
relativeTo ::
  -- | Reference 'URI' to make absolute
  URI ->
  -- | Base 'URI'
  URI ->
  -- | The target 'URI'
  Maybe URI
relativeTo r base =
  case uriScheme base of
    Nothing -> Nothing
    Just bscheme ->
      Just $
        if isJust (uriScheme r)
          then r {uriPath = uriPath r >>= removeDotSegments}
          else
            r
              { uriScheme = Just bscheme,
                uriAuthority = case uriAuthority r of
                  Right auth -> Right auth
                  Left rabs ->
                    case uriAuthority base of
                      Right auth -> Right auth
                      Left babs -> Left (babs || rabs),
                uriPath =
                  (>>= removeDotSegments) $
                    if isPathAbsolute r
                      then uriPath r
                      else case (uriPath base, uriPath r) of
                        (Nothing, Nothing) -> Nothing
                        (Just b', Nothing) -> Just b'
                        (Nothing, Just r') -> Just r'
                        (Just (bt, bps), Just (rt, rps)) ->
                          fmap (rt,) . NE.nonEmpty $
                            (if bt then NE.toList bps else NE.init bps)
                              <> NE.toList rps,
                uriQuery =
                  if isLeft (uriAuthority r)
                    && isNothing (uriPath r)
                    && null (uriQuery r)
                    then uriQuery base
                    else uriQuery r
              }

----------------------------------------------------------------------------
-- Helpers

-- | Remove dot segments from a path.
removeDotSegments ::
  (Bool, NonEmpty (RText 'PathPiece)) ->
  Maybe (Bool, NonEmpty (RText 'PathPiece))
removeDotSegments (trailSlash, path) = go [] (NE.toList path) trailSlash
  where
    go out [] ts = (fmap (ts,) . NE.nonEmpty . reverse) out
    go out (x : xs) ts
      | unRText x == "." = go out xs (null xs || ts)
      | unRText x == ".." = go (drop 1 out) xs (null xs || ts)
      | otherwise = go (x : out) xs ts
