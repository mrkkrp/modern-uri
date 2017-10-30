-- |
-- Module      :  Text.URI.Render
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI renders, an internal module.

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.URI.Render
  ( render
  , render'
  , renderBs
  , renderBs'
  , renderStr
  , renderStr' )
where

import Data.ByteString (ByteString)
import Data.Char (chr, intToDigit)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Word (Word8)
import Numeric (showInt)
import Text.URI.Types
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import qualified Data.Semigroup               as S
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.Builder.Int   as TLB

----------------------------------------------------------------------------
-- High-level wrappers

-- | Render a given 'URI' value as strict 'Text'.

render :: URI -> Text
render = TL.toStrict . TLB.toLazyText . render'

-- | Render a given 'URI' value as a 'TLB.Builder'.

render' :: URI -> TLB.Builder
render' = genericRender TLB.decimal $ \e ->
  TLB.fromText . percentEncode e . unRText

-- | Render a given 'URI' value as a strict 'ByteString'.

renderBs :: URI -> ByteString
renderBs = BL.toStrict . BLB.toLazyByteString . renderBs'

-- | Render a given 'URI' value as a 'BLB.Builder'.

renderBs' :: URI -> BLB.Builder
renderBs' = genericRender BLB.wordDec $ \e ->
  BLB.byteString . TE.encodeUtf8 . percentEncode e . unRText

-- | Render a given 'URI' value as a 'String'.
--
-- @since 0.0.2.0

renderStr :: URI -> String
renderStr = ($ []) . renderStr'

-- | Render a given 'URI' value as 'ShowS'.
--
-- @since 0.0.2.0

renderStr' :: URI -> ShowS
renderStr' = toShowS . genericRender (DString . showInt) (\e ->
  fromString . T.unpack . percentEncode e . unRText)

----------------------------------------------------------------------------
-- Generic render

-- | Percent encoding options.

data Escaping
  = N                  -- ^ No escaping
  | P                  -- ^ Path-piece style, do not encode @:@ and @\@@
  | PQ                 -- ^ Mixed style, encode @:@ but not @\@@
  | Q                  -- ^ Query string style, encode @:@ and @\@@
  deriving (Eq)

type Render a b = (forall l. Escaping -> RText l -> b) -> a -> b
type R        b = (Monoid b, IsString b)

genericRender :: R b => (Word -> b) -> Render URI b
genericRender d r uri@URI {..} = mconcat
  [ rJust (rScheme r) uriScheme
  , rJust (rAuthority d r) (either (const Nothing) Just uriAuthority)
  , rPath (isPathAbsolute uri) r uriPath
  , rQuery r uriQuery
  , rJust (rFragment r) uriFragment ]
{-# INLINE genericRender #-}

rJust :: Monoid m => (a -> m) -> Maybe a -> m
rJust = maybe mempty

rScheme :: R b => Render (RText 'Scheme) b
rScheme r = (<> ":") . r Q
{-# INLINE rScheme #-}

rAuthority :: R b => (Word -> b) -> Render Authority b
rAuthority d r Authority {..} = mconcat
  [ "//"
  , rJust (rUserInfo r) authUserInfo
  , if T.head (unRText authHost) == '['
      then r N authHost
      else r Q authHost
  , rJust ((":" <>) . d) authPort ]
{-# INLINE rAuthority #-}

rUserInfo :: R b => Render UserInfo b
rUserInfo r UserInfo {..} = mconcat
  [ r Q uiUsername
  , rJust ((":" <>) . r Q) uiPassword
  , "@" ]
{-# INLINE rUserInfo #-}

rPath :: R b => Bool -> Render [RText 'PathPiece] b
rPath isAbsolute r ps' = leading <> other
  where
    leading = if isAbsolute then "/" else mempty
    other   = mconcat . intersperse "/" $
      case (isAbsolute, ps') of
        (False, p:ps) -> r PQ p : fmap (r P) ps
        _             -> r P <$> ps'
{-# INLINE rPath #-}

rQuery :: R b => Render [QueryParam] b
rQuery r = \case
  [] -> mempty
  qs -> "?" <> mconcat (intersperse "&" (rQueryParam r <$> qs))
{-# INLINE rQuery #-}

rQueryParam :: R b => Render QueryParam b
rQueryParam r = \case
  QueryFlag flag -> r P flag
  QueryParam k v -> r P k <> "=" <> r P v
{-# INLINE rQueryParam #-}

rFragment :: R b => Render (RText 'Fragment) b
rFragment r = ("#" <>) . r P
{-# INLINE rFragment #-}

----------------------------------------------------------------------------
-- DString

newtype DString = DString { toShowS :: ShowS }

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

percentEncode
  :: Escaping          -- ^ Whether to leave @':'@ and @'@'@ unescaped
  -> Text              -- ^ Input text to encode
  -> Text              -- ^ Percent-encoded text
percentEncode e txt = T.unfoldrN n f (bs, [])
  where
    f (bs', []) =
      case B.uncons bs' of
        Nothing -> Nothing
        Just (w, bs'') -> Just $
          if isUnreserved e w
            then (chr (fromIntegral w), (bs'', []))
            else let c:|cs = encodeByte w
                 in (c, (bs'', cs))
    f (bs', x:xs) = Just (x, (bs', xs))
    bs = TE.encodeUtf8 txt
    n  = B.foldl' (\n' w -> g w + n') 0 bs
    g x = if isUnreserved e x then 1 else 3
    encodeByte x = '%' :| [intToDigit h, intToDigit l]
      where
        (h, l) = fromIntegral x `quotRem` 16

-- | The predicate selects unreserved bytes.

isUnreserved :: Escaping -> Word8 -> Bool
isUnreserved N _ = True
isUnreserved t x
  | x >= 65 && x <= 90  = True -- 'A'..'Z'
  | x >= 97 && x <= 122 = True -- 'a'..'z'
  | x >= 48 && x <= 57  = True -- '0'..'9'
  | x == 45             = True -- '-'
  | x == 95             = True -- '_'
  | x == 46             = True -- '.'
  | x == 126            = True -- '~'
  | semi && x == 58     = True -- ':'
  | at   && x == 64     = True -- '@'
  | otherwise           = False
  where
    semi = t == P
    at   = t == P || t == PQ
