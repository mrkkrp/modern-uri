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

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Proxy
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
render' = genericRender TLB.decimal $
  TLB.fromText . percentEncode

-- | Render a given 'URI' value as a strict 'ByteString'.

renderBs :: URI -> ByteString
renderBs = BL.toStrict . BLB.toLazyByteString . renderBs'

-- | Render a given 'URI' value as a 'BLB.Builder'.

renderBs' :: URI -> BLB.Builder
renderBs' = genericRender BLB.wordDec $
  BLB.byteString . TE.encodeUtf8 . percentEncode

-- | Render a given 'URI' value as a 'String'.
--
-- @since 0.0.2.0

renderStr :: URI -> String
renderStr = ($ []) . renderStr'

-- | Render a given 'URI' value as 'ShowS'.
--
-- @since 0.0.2.0

renderStr' :: URI -> ShowS
renderStr' = toShowS . genericRender (DString . showInt)
  (fromString . T.unpack . percentEncode)

----------------------------------------------------------------------------
-- Generic render

type Render a b = (forall l. RLabel l => RText l -> b) -> a -> b
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
rScheme r = (<> ":") . r
{-# INLINE rScheme #-}

rAuthority :: R b => (Word -> b) -> Render Authority b
rAuthority d r Authority {..} = mconcat
  [ "//"
  , rJust (rUserInfo r) authUserInfo
  , r authHost
  , rJust ((":" <>) . d) authPort ]
{-# INLINE rAuthority #-}

rUserInfo :: R b => Render UserInfo b
rUserInfo r UserInfo {..} = mconcat
  [ r uiUsername
  , rJust ((":" <>) . r) uiPassword
  , "@" ]
{-# INLINE rUserInfo #-}

rPath :: R b => Bool -> Render [RText 'PathPiece] b
rPath isAbsolute r ps = leading <> other
  where
    leading = if isAbsolute then "/" else mempty
    other   = mconcat . intersperse "/" $ r <$> ps
{-# INLINE rPath #-}

rQuery :: R b => Render [QueryParam] b
rQuery r = \case
  [] -> mempty
  qs -> "?" <> mconcat (intersperse "&" (rQueryParam r <$> qs))
{-# INLINE rQuery #-}

rQueryParam :: R b => Render QueryParam b
rQueryParam r = \case
  QueryFlag flag -> r flag
  QueryParam k v -> r k <> "=" <> r v
{-# INLINE rQueryParam #-}

rFragment :: R b => Render (RText 'Fragment) b
rFragment r = ("#" <>) . r
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

percentEncode :: forall l. RLabel l
  => RText l           -- ^ Input text to encode
  -> Text              -- ^ Percent-encoded text
percentEncode rtxt =
  if skipEscaping (Proxy :: Proxy l) txt
    then txt
    else T.unfoldr f (TE.encodeUtf8 txt, [])
  where
    f (bs', []) =
      case B.uncons bs' of
        Nothing -> Nothing
        Just (w, bs'') -> Just $
          if | sap && w == 32 -> ('+', (bs'', []))
             | nne w          -> (chr (fromIntegral w), (bs'', []))
             | otherwise      ->
               let c:|cs = encodeByte w
               in (c, (bs'', cs))
    f (bs', x:xs) = Just (x, (bs', xs))
    encodeByte x = '%' :| [intToDigit h, intToDigit l]
      where
        (h, l) = fromIntegral x `quotRem` 16
    nne = needsNoEscaping (Proxy :: Proxy l)
    sap = spaceAsPlus     (Proxy :: Proxy l)
    txt = unRText rtxt

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
  skipEscaping Proxy x = T.head x == '['

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
      45  -> True
      46  -> True
      95  -> True
      126 -> True
      _   -> False

isAlphaNum :: Word8 -> Bool
isAlphaNum x
  | x >= 65 && x <= 90  = True -- 'A'..'Z'
  | x >= 97 && x <= 122 = True -- 'a'..'z'
  | x >= 48 && x <= 57  = True -- '0'..'9'
  | otherwise           = False

isDelim :: Word8 -> Bool
isDelim x
  | x == 33            = True
  | x == 36            = True
  | x >= 38 && x <= 44 = True
  | x == 59            = True
  | x == 61            = True
  | otherwise          = False

isDelim' :: Word8 -> Bool
isDelim' x
  | x == 33            = True
  | x == 36            = True
  | x >= 39 && x <= 42 = True
  | x == 44            = True
  | x == 59            = True
  | otherwise          = False
