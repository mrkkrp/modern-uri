-- |
-- Module      :  Text.URI.Lens
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for working with the 'URI.Plain' data type and its internals.

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Text.URI.Lens
  ( uriScheme
  , uriAuthority
  , uriPath
  , uriQuery
  , uriFragment
  , authUser
  , authHost
  , authPort
  , uiUsername
  , uiPassword
  -- TODO the rest of the stuff
  )
where

import Text.URI.Types (URI, Authority, UserInfo, RText, RTextLabel (..))
import qualified Text.URI.Types as URI

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | 'URI' scheme lens.

uriScheme :: Lens' URI (Maybe (RText 'Scheme))
uriScheme f s = (\x -> s { URI.uriScheme = x }) <$> f (URI.uriScheme s)

-- | 'URI' authority lens.

uriAuthority :: Lens' URI (Maybe URI.Authority)
uriAuthority f s = (\x -> s { URI.uriAuthority = x }) <$> f (URI.uriAuthority s)

-- | 'URI' path lens.

uriPath :: Lens' URI [RText 'PathPiece]
uriPath f s = (\x -> s { URI.uriPath = x }) <$> f (URI.uriPath s)

-- | 'URI' query params lens.

uriQuery :: Lens' URI [URI.QueryParam]
uriQuery f s = (\x -> s { URI.uriQuery = x }) <$> f (URI.uriQuery s)

-- | 'URI' fragment lens.

uriFragment :: Lens' URI (Maybe (RText 'Fragment))
uriFragment f s = (\x -> s { URI.uriFragment = x }) <$> f (URI.uriFragment s)

-- | 'Authority' user lens.

authUser :: Lens' Authority (Maybe URI.UserInfo)
authUser f s = (\x -> s { URI.authUser = x }) <$> f (URI.authUser s)

-- | 'Authority' host lens.

authHost :: Lens' Authority (RText 'Host)
authHost f s = (\x -> s { URI.authHost = x }) <$> f (URI.authHost s)

-- | 'Authority' port lens.

authPort :: Lens' Authority (Maybe Word)
authPort f s = (\x -> s { URI.authPort = x }) <$> f (URI.authPort s)

-- | 'UserInfo' username lens.

uiUsername :: Lens' UserInfo (RText 'Username)
uiUsername f s = (\x -> s { URI.uiUsername = x }) <$> f (URI.uiUsername s)

-- | 'UserInfo' password lens.

uiPassword :: Lens' UserInfo (RText 'Password)
uiPassword f s = (\x -> s { URI.uiPassword = x }) <$> f (URI.uiPassword s)
