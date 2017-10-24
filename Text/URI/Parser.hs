-- |
-- Module      :  Text.URI.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parsers, an internal module.

{-# LANGUAGE FlexibleContexts #-}

module Text.URI.Parser
  ( parse
  , parseBs )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Megaparsec hiding (parse)
import qualified Text.URI.Types as URI

-- | This parser can be used to parse URIs from strict 'Text'.

parse :: MonadParsec e Text m => m URI.URI
parse = undefined -- TODO
{-# INLINEABLE parse #-}

-- | Similarly to 'parse', this parser can be used to URIs from strict
-- 'ByteString's.

parseBs :: MonadParsec e ByteString m => m URI.URI
parseBs = undefined -- TODO
{-# INLINEABLE parseBs #-}
