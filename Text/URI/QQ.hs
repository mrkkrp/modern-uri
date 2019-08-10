-- |
-- Module      :  Text.URI.QQ
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Quasi-quoters for compile-time construction of URIs and refined text
-- values.

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.URI.QQ
  ( uri
  , scheme
  , host
  , username
  , password
  , pathPiece
  , queryKey
  , queryValue
  , fragment )
where

import Control.Exception (SomeException, Exception (..))
import Data.Text (Text)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Text.URI.Parser.Text
import Text.URI.Types
import qualified Data.Text as T

-- | Construct a 'URI' value at compile time.

uri :: QuasiQuoter
uri = liftToQQ mkURI

-- | Construct a @'RText' 'Scheme'@ value at compile time.

scheme :: QuasiQuoter
scheme = liftToQQ mkScheme

-- | Construct a @'RText' 'Host'@ value at compile time.

host :: QuasiQuoter
host = liftToQQ mkHost

-- | Construct a @'RText' 'Username'@ value at compile time.

username :: QuasiQuoter
username = liftToQQ mkUsername

-- | Construct a @'RText' 'Password'@ value at compile time.

password :: QuasiQuoter
password = liftToQQ mkPassword

-- | Construct a @'RText' 'PathPiece'@ value at compile time.

pathPiece :: QuasiQuoter
pathPiece = liftToQQ mkPathPiece

-- | Construct a @'RText' 'QueryKey'@ value at compile time.

queryKey :: QuasiQuoter
queryKey = liftToQQ mkQueryKey

-- | Construct a @'RText 'QueryValue'@ value at compile time.

queryValue :: QuasiQuoter
queryValue = liftToQQ mkQueryValue

-- | Construct a @'RText' 'Fragment'@ value at compile time.

fragment :: QuasiQuoter
fragment = liftToQQ mkFragment

----------------------------------------------------------------------------
-- Helpers

-- | Lift a smart constructor for refined text into a 'QuasiQuoter'.

liftToQQ :: Lift a => (Text -> Either SomeException a) -> QuasiQuoter
liftToQQ f = QuasiQuoter
  { quoteExp  = \str ->
      case f (T.pack str) of
        Left err -> fail (displayException err)
        Right x  -> lift x
  , quotePat  = error "This usage is not supported"
  , quoteType = error "This usage is not supported"
  , quoteDec  = error "This usage is not supported" }
