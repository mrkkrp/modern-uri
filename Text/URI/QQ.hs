-- |
-- Module      :  Text.URI.QQ
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Quasi-quoters for compile-time construction of URIs.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.URI.QQ
  ( scheme
  , host
  , username
  , password
  , pathPiece
  , queryPiece
  , fragment )
where

import Control.Exception (SomeException, Exception (..))
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (cast)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import qualified Data.Text      as T
import qualified Text.URI.Types as URI

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Syntax (dataToExpQ)
#else
dataToExpQ :: Data a => (forall b. Data b => b -> Maybe (Q Exp)) -> a -> Q Exp
dataToExpQ _ _ = fail "The feature requires at least GHC 8 to work"
#endif

-- | Construct a @'RText' 'Scheme'@ value at compile time.

scheme :: QuasiQuoter
scheme = liftToQQ URI.mkScheme

-- | Construct a @'RText' 'Host'@ value at compile time.

host :: QuasiQuoter
host = liftToQQ URI.mkHost

-- | Construct a @'RText' 'Username'@ value at compile time.

username :: QuasiQuoter
username = liftToQQ URI.mkUsername

-- | Construct a @'RText' 'Password'@ value at compile time.

password :: QuasiQuoter
password = liftToQQ URI.mkPassword

-- | Construct a @'RText' 'PathPiece'@ value at compile time.

pathPiece :: QuasiQuoter
pathPiece = liftToQQ URI.mkPathPiece

-- | Construct a @'RText' 'QueryPiece'@ value at compile time.

queryPiece :: QuasiQuoter
queryPiece = liftToQQ URI.mkQueryPiece

-- | Construct a @'RText' 'Fragment'@ value at compile time.

fragment :: QuasiQuoter
fragment = liftToQQ URI.mkFragment

----------------------------------------------------------------------------
-- Helpers

-- | Lift a smart constructor for refined text into a 'QuasiQuoter'.

liftToQQ :: Data a => (Text -> Either SomeException a) -> QuasiQuoter
liftToQQ f = QuasiQuoter
  { quoteExp  = \str ->
      case f (T.pack str) of
        Left err -> fail (displayException err)
        Right x  -> dataToExpQ (fmap liftText . cast) x
  , quotePat  = error "This usage is not supported"
  , quoteType = error "This usage is not supported"
  , quoteDec  = error "This usage is not supported" }

-- | Lift strict 'T.Text' to @'Q' 'Exp'@.

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
