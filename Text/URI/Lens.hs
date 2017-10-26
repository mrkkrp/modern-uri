-- |
-- Module      :  Text.URI.Lens
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for working with the 'URI' data type and its internals.

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Text.URI.Lens
  ( uriScheme
  , uriAuthority
  , uriPath
  , uriQuery
  , uriFragment
  , authUserInfo
  , authHost
  , authPort
  , uiUsername
  , uiPassword
  , queryFlag
  , queryParam
  , unRText )
where

import Data.Functor.Contravariant
import Data.Profunctor
import Data.Text (Text)
import Text.URI.Types (URI, Authority, UserInfo, QueryParam (..), RText, RTextLabel (..))
import qualified Text.URI.Types as URI

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

-- | 'Authority' user info lens.

authUserInfo :: Lens' Authority (Maybe URI.UserInfo)
authUserInfo f s = (\x -> s { URI.authUserInfo = x }) <$> f (URI.authUserInfo s)

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

uiPassword :: Lens' UserInfo (Maybe (RText 'Password))
uiPassword f s = (\x -> s { URI.uiPassword = x }) <$> f (URI.uiPassword s)

-- | 'QueryParam' prism for query flags.

queryFlag :: Prism' URI.QueryParam (RText 'QueryKey)
queryFlag = prism' QueryFlag $ \case
  QueryFlag x -> Just x
  _           -> Nothing

-- | 'QueryParam' prism for query parameters.

queryParam :: Prism' QueryParam (RText 'QueryKey, RText 'QueryValue)
queryParam = prism' construct pick
  where
    construct (x, y) = QueryParam x y
    pick = \case
      QueryParam x y -> Just (x, y)
      _              -> Nothing

-- | A getter that can project 'Text' from refined text values.

unRText :: Getter (RText l) Text
unRText = to URI.unRText

----------------------------------------------------------------------------
-- Helpers

type Lens' s a =
  forall f. Functor f => (a -> f a) -> s -> f s
type Getter s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
type Prism s t a b =
  forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

-- | Build a 'Prism'.

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- | Another way to build a 'Prism'.

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))

-- | Lift a function into optic.

to :: (Profunctor p, Contravariant f) => (s -> a) -> (p a (f a) -> p s (f s))
to f = dimap f (contramap f)
