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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}

module Text.URI.Parser
  ( -- * Data types
    Err (..)
  , SumWithErr (..)
  , ParseException (..)
    -- * Functions
  , mkURI
  , parse )
where

import Control.Monad.Catch
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.Megaparsec hiding (parse)
import Text.URI.Types (URI, RTextException (..))
import qualified Data.Text as T

-- | Custom component of error messages returned by 'parse'.

newtype Err = Err RTextException
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | Type class for sum types containing 'Err'.

class SumWithErr e where
  injectErr :: Err -> e

instance SumWithErr Err where
  injectErr = id

instance ShowErrorComponent Err where
  showErrorComponent (Err (RTextException rlabel x)) =
    "\"" ++ T.unpack x ++ "\" cannot be parsed as a " ++ show rlabel

-- | Parse exception thrown by 'mkURI' when a given 'Text' value cannot be
-- parsed as a 'URI'.

data ParseException e = ParseException Text (ParseError Char e)
  deriving (Show, Eq, Data, Typeable, Generic)

instance (Show e, Typeable e, ShowErrorComponent e)
    => Exception (ParseException e) where
  displayException (ParseException s e) = parseErrorPretty' s e

-- | Construct a 'URI' from 'Text'. In case of failure @'ParseException'
-- 'Err'@ is thrown.

mkURI :: MonadThrow m => Text -> m URI
mkURI input =
  case runParser (parse :: Parsec Err Text URI) "" input of
    Left err -> throwM (ParseException input err)
    Right x  -> return x

-- | This parser can be used to parse 'URI' from strict 'Text'.

parse :: (MonadParsec e Text m, SumWithErr e) => m URI
parse = undefined -- TODO
{-# INLINEABLE parse #-}
