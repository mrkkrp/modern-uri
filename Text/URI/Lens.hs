{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Text.URI.Lens
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Lenses for working with the 'URI' data type and its internals.
module Text.URI.Lens
  ( uriScheme,
    uriAuthority,
    uriPath,
    isPathAbsolute,
    uriTrailingSlash,
    uriQuery,
    uriFragment,
    authUserInfo,
    authHost,
    authPort,
    uiUsername,
    uiPassword,
    _QueryFlag,
    _QueryParam,
    queryFlag,
    queryParam,
    unRText,
  )
where

import Control.Applicative (liftA2)
import Data.Foldable (find)
import Data.Functor.Contravariant
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Profunctor
import Data.Text (Text)
import Text.URI.Types
  ( Authority,
    QueryParam (..),
    RText,
    RTextLabel (..),
    URI,
    UserInfo,
  )
import qualified Text.URI.Types as URI

-- | 'URI' scheme lens.
uriScheme :: Lens' URI (Maybe (RText 'Scheme))
uriScheme f s = (\x -> s {URI.uriScheme = x}) <$> f (URI.uriScheme s)

-- | 'URI' authority lens.
--
-- __Note__: before version /0.1.0.0/ this lens allowed to focus on @'Maybe'
-- 'URI.Authority'@.
uriAuthority :: Lens' URI (Either Bool URI.Authority)
uriAuthority f s = (\x -> s {URI.uriAuthority = x}) <$> f (URI.uriAuthority s)

-- | 'URI' path lens.
uriPath :: Lens' URI [RText 'PathPiece]
uriPath f s = (\x -> s {URI.uriPath = (ts,) <$> NE.nonEmpty x}) <$> f ps
  where
    ts = maybe False fst path
    ps = maybe [] (NE.toList . snd) path
    path = URI.uriPath s

-- | A getter that can tell if path component of a 'URI' is absolute.
--
-- @since 0.1.0.0
isPathAbsolute :: Getter URI Bool
isPathAbsolute = to URI.isPathAbsolute

-- | A 0-1 traversal allowing to view and manipulate trailing slash.
--
-- @since 0.2.0.0
uriTrailingSlash :: Traversal' URI Bool
uriTrailingSlash f s =
  (\x -> s {URI.uriPath = liftA2 (,) x ps}) <$> traverse f ts
  where
    ts = fst <$> path
    ps = snd <$> path
    path = URI.uriPath s

-- | 'URI' query params lens.
uriQuery :: Lens' URI [URI.QueryParam]
uriQuery f s = (\x -> s {URI.uriQuery = x}) <$> f (URI.uriQuery s)

-- | 'URI' fragment lens.
uriFragment :: Lens' URI (Maybe (RText 'Fragment))
uriFragment f s = (\x -> s {URI.uriFragment = x}) <$> f (URI.uriFragment s)

-- | 'Authority' user info lens.
authUserInfo :: Lens' Authority (Maybe URI.UserInfo)
authUserInfo f s = (\x -> s {URI.authUserInfo = x}) <$> f (URI.authUserInfo s)

-- | 'Authority' host lens.
authHost :: Lens' Authority (RText 'Host)
authHost f s = (\x -> s {URI.authHost = x}) <$> f (URI.authHost s)

-- | 'Authority' port lens.
authPort :: Lens' Authority (Maybe Word)
authPort f s = (\x -> s {URI.authPort = x}) <$> f (URI.authPort s)

-- | 'UserInfo' username lens.
uiUsername :: Lens' UserInfo (RText 'Username)
uiUsername f s = (\x -> s {URI.uiUsername = x}) <$> f (URI.uiUsername s)

-- | 'UserInfo' password lens.
uiPassword :: Lens' UserInfo (Maybe (RText 'Password))
uiPassword f s = (\x -> s {URI.uiPassword = x}) <$> f (URI.uiPassword s)

-- | 'QueryParam' prism for query flags.
_QueryFlag :: Prism' URI.QueryParam (RText 'QueryKey)
_QueryFlag = prism' QueryFlag $ \case
  QueryFlag x -> Just x
  _ -> Nothing

-- | 'QueryParam' prism for query parameters.
_QueryParam :: Prism' QueryParam (RText 'QueryKey, RText 'QueryValue)
_QueryParam = prism' construct pick
  where
    construct (x, y) = QueryParam x y
    pick = \case
      QueryParam x y -> Just (x, y)
      _ -> Nothing

-- | Check if the given query key is present in the collection of query
-- parameters.
queryFlag :: RText 'QueryKey -> Getter [URI.QueryParam] Bool
queryFlag k = to (isJust . find g)
  where
    g (QueryFlag k') = k' == k
    g _ = False

-- | Manipulate a query parameter by its key. Note that since there may be
-- several query parameters with the same key this is a traversal that can
-- return\/modify several items at once.
queryParam :: RText 'QueryKey -> Traversal' [URI.QueryParam] (RText 'QueryValue)
queryParam k f = traverse g
  where
    g p@(QueryParam k' v) =
      if k == k'
        then QueryParam k' <$> f v
        else pure p
    g p = pure p

-- | A getter that can project 'Text' from refined text values.
unRText :: Getter (RText l) Text
unRText = to URI.unRText

----------------------------------------------------------------------------
-- Helpers

type Lens' s a =
  forall f. Functor f => (a -> f a) -> s -> f s

type Traversal' s a =
  forall f. Applicative f => (a -> f a) -> s -> f s

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
