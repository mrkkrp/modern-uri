{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Text.URI.Render
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI renders, an internal module.
module Text.URI.Render
  ( render,
    render',
    renderBs,
    renderBs',
    renderStr,
    renderStr',
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, intToDigit)
import Data.Kind (Type)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Reflection
import qualified Data.Semigroup as S
import Data.String (IsString (..))
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import Data.Word (Word8)
import Numeric (showInt)
import Text.URI.Types

----------------------------------------------------------------------------
-- High-level wrappers

-- | Render a given 'URI' value as strict 'Text'.
render :: URI -> Text
render = TL.toStrict . TLB.toLazyText . render'

-- | Render a given 'URI' value as a 'TLB.Builder'.
render' :: URI -> TLB.Builder
render' x =
  equip
    TLB.decimal
    (TLB.fromText . percentEncode)
    (genericRender x)

-- | Render a given 'URI' value as a strict 'ByteString'.
renderBs :: URI -> ByteString
renderBs = BL.toStrict . BB.toLazyByteString . renderBs'

-- | Render a given 'URI' value as a 'BB.Builder'.
renderBs' :: URI -> BB.Builder
renderBs' x =
  equip
    BB.wordDec
    (BB.byteString . TE.encodeUtf8 . percentEncode)
    (genericRender x)

-- | Render a given 'URI' value as a 'String'.
--
-- @since 0.0.2.0
renderStr :: URI -> String
renderStr = ($ []) . renderStr'

-- | Render a given 'URI' value as 'ShowS'.
--
-- @since 0.0.2.0
renderStr' :: URI -> ShowS
renderStr' x =
  toShowS $
    equip
      (DString . showInt)
      (fromString . T.unpack . percentEncode)
      (genericRender x)

----------------------------------------------------------------------------
-- Reflection stuff

data Renders b = Renders
  { rWord :: Word -> b,
    rText :: forall l. RLabel l => RText l -> b
  }

equip ::
  forall b.
  (Word -> b) ->
  (forall l. RLabel l => RText l -> b) ->
  (forall (s :: Type). Reifies s (Renders b) => Tagged s b) ->
  b
equip rWord rText f = reify Renders {..} $ \(Proxy :: Proxy s') ->
  unTagged (f :: Tagged s' b)

renderWord ::
  forall s b.
  Reifies s (Renders b) =>
  Word ->
  Tagged s b
renderWord = Tagged . rWord (reflect (Proxy :: Proxy s))

renderText ::
  forall s b l.
  (Reifies s (Renders b), RLabel l) =>
  RText l ->
  Tagged s b
renderText = Tagged . rText (reflect (Proxy :: Proxy s))

----------------------------------------------------------------------------
-- Generic render

type Render a b =
  forall (s :: Type).
  (Semigroup b, Monoid b, IsString b, Reifies s (Renders b)) =>
  a ->
  Tagged s b

genericRender :: Render URI b
genericRender URI {..} =
  mconcat
    [ rJust rScheme uriScheme,
      rJust rAuthority (either (const Nothing) Just uriAuthority),
      rAbsPathSlash uriAuthority uriPath,
      rPath uriPath,
      rQuery uriQuery,
      rJust rFragment uriFragment
    ]
{-# INLINE genericRender #-}

rJust :: Monoid m => (a -> m) -> Maybe a -> m
rJust = maybe mempty

rScheme :: Render (RText 'Scheme) b
rScheme = (<> ":") . renderText
{-# INLINE rScheme #-}

rAuthority :: Render Authority b
rAuthority Authority {..} =
  mconcat
    [ "//",
      rJust rUserInfo authUserInfo,
      renderText authHost,
      rJust ((":" <>) . renderWord) authPort
    ]
{-# INLINE rAuthority #-}

rUserInfo :: Render UserInfo b
rUserInfo UserInfo {..} =
  mconcat
    [ renderText uiUsername,
      rJust ((":" <>) . renderText) uiPassword,
      "@"
    ]
{-# INLINE rUserInfo #-}

rAbsPathSlash ::
  Either Bool a ->
  Render (Maybe (Bool, NonEmpty (RText 'PathPiece))) b
rAbsPathSlash (Left isAbsolute) _ = if isAbsolute then "/" else mempty
rAbsPathSlash (Right _) Nothing = mempty
rAbsPathSlash (Right _) (Just _) = "/"
{-# INLINE rAbsPathSlash #-}

rPath :: Render (Maybe (Bool, NonEmpty (RText 'PathPiece))) b
rPath path =
  case path of
    Nothing -> mempty
    Just (trailingSlash, ps) ->
      (mconcat . intersperse "/" . fmap renderText . NE.toList) ps
        <> if trailingSlash then "/" else mempty
{-# INLINE rPath #-}

rQuery :: Render [QueryParam] b
rQuery = \case
  [] -> mempty
  qs -> "?" <> mconcat (intersperse "&" (rQueryParam <$> qs))
{-# INLINE rQuery #-}

rQueryParam :: Render QueryParam b
rQueryParam = \case
  QueryFlag flag -> renderText flag
  QueryParam k v -> renderText k <> "=" <> renderText v
{-# INLINE rQueryParam #-}

rFragment :: Render (RText 'Fragment) b
rFragment = ("#" <>) . renderText
{-# INLINE rFragment #-}

----------------------------------------------------------------------------
-- DString

newtype DString = DString {toShowS :: ShowS}

instance S.Semigroup DString where
  DString a <> DString b = DString (a . b)

instance Monoid DString where
  mempty = DString id
  mappend = (S.<>)

instance IsString DString where
  fromString str = DString (str ++)

----------------------------------------------------------------------------
-- Percent-encoding

-- | Percent-encode a 'Text' value.
percentEncode ::
  forall l.
  RLabel l =>
  -- | Input text to encode
  RText l ->
  -- | Percent-encoded text
  Text
percentEncode rtxt =
  if skipEscaping (Proxy :: Proxy l) txt
    then txt
    else T.unfoldr f (TE.encodeUtf8 txt, [])
  where
    f (bs', []) =
      case B.uncons bs' of
        Nothing -> Nothing
        Just (w, bs'') ->
          Just $
            if
                | sap && w == 32 -> ('+', (bs'', []))
                | nne w -> (chr (fromIntegral w), (bs'', []))
                | otherwise ->
                  let c :| cs = encodeByte w
                   in (c, (bs'', cs))
    f (bs', x : xs) = Just (x, (bs', xs))
    encodeByte x = '%' :| [intToDigit h, intToDigit l]
      where
        (h, l) = fromIntegral x `quotRem` 16
    nne = needsNoEscaping (Proxy :: Proxy l)
    sap = spaceAsPlus (Proxy :: Proxy l)
    txt = unRText rtxt
{-# INLINE percentEncode #-}

-- | This type class attaches some predicates that control serialization to
-- the type level label of kind 'RTextLabel'.
class RLabel (l :: RTextLabel) where
  -- | The predicate selects bytes that are not to be percent-escaped in
  -- rendered URI.
  needsNoEscaping :: Proxy l -> Word8 -> Bool

  -- | Whether to serialize space as the plus sign.
  spaceAsPlus :: Proxy l -> Bool
  spaceAsPlus Proxy = False

  -- | Whether to skip percent-escaping altogether for this value.
  skipEscaping :: Proxy l -> Text -> Bool
  skipEscaping Proxy _ = False

instance RLabel 'Scheme where
  needsNoEscaping Proxy x = isAlphaNum x || x == 43 || x == 45 || x == 46

instance RLabel 'Host where
  needsNoEscaping Proxy x = isUnreserved x || isDelim x
  skipEscaping Proxy x = T.take 1 x == "["

instance RLabel 'Username where
  needsNoEscaping Proxy x = isUnreserved x || isDelim x

instance RLabel 'Password where
  needsNoEscaping Proxy x = isUnreserved x || isDelim x || x == 58

instance RLabel 'PathPiece where
  needsNoEscaping Proxy x =
    isUnreserved x || isDelim x || x == 64

instance RLabel 'QueryKey where
  needsNoEscaping Proxy x =
    isPChar isDelim' x || x == 47 || x == 63
  spaceAsPlus Proxy = True

instance RLabel 'QueryValue where
  needsNoEscaping Proxy x =
    isPChar isDelim' x || x == 47 || x == 63
  spaceAsPlus Proxy = True

instance RLabel 'Fragment where
  needsNoEscaping Proxy x =
    isPChar isDelim x || x == 47 || x == 63

isPChar :: (Word8 -> Bool) -> Word8 -> Bool
isPChar f x = isUnreserved x || f x || x == 58 || x == 64

isUnreserved :: Word8 -> Bool
isUnreserved x = isAlphaNum x || other
  where
    other = case x of
      45 -> True
      46 -> True
      95 -> True
      126 -> True
      _ -> False

isAlphaNum :: Word8 -> Bool
isAlphaNum x
  | x >= 65 && x <= 90 = True -- 'A'..'Z'
  | x >= 97 && x <= 122 = True -- 'a'..'z'
  | x >= 48 && x <= 57 = True -- '0'..'9'
  | otherwise = False

isDelim :: Word8 -> Bool
isDelim x
  | x == 33 = True
  | x == 36 = True
  | x >= 38 && x <= 44 = True
  | x == 59 = True
  | x == 61 = True
  | otherwise = False

isDelim' :: Word8 -> Bool
isDelim' x
  | x == 33 = True
  | x == 36 = True
  | x >= 39 && x <= 42 = True
  | x == 44 = True
  | x == 59 = True
  | otherwise = False
