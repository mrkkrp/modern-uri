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

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.URI.Render
  ( render
  , render'
  , renderBs
  , renderBs' )
where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word8)
import Text.URI.Types
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB
import qualified Data.Text.Lazy.Builder.Int   as TLB

-- | Render a given 'URI' value as strict 'Text'.

render :: URI -> Text
render = TL.toStrict . TLB.toLazyText . render'

-- | Render a given 'URI' value as a 'TLB.Builder'.

render' :: URI -> TLB.Builder
render' URI {..} = mconcat
  [ rJust rScheme uriScheme
  , rJust rAuthority uriAuthority
  , rPath uriPath
  , rQuery uriQuery
  , rJust rFragment uriFragment ]

rJust :: (a -> TLB.Builder) -> Maybe a -> TLB.Builder
rJust = maybe mempty

rScheme :: RText 'Scheme -> TLB.Builder
rScheme = (<> ":") . TLB.fromText . unRText

rAuthority :: Authority -> TLB.Builder
rAuthority Authority {..} = mconcat
  [ "//"
  , rJust rUserInfo authUserInfo
  , unRText' authHost
  , rJust ((":" <>) . TLB.decimal) authPort ]

rUserInfo :: UserInfo -> TLB.Builder
rUserInfo UserInfo {..} = mconcat
  [ unRText' uiUsername
  , rJust ((":" <>) . unRText') uiPassword
  , "@" ]

rPath :: [RText 'PathPiece] -> TLB.Builder
rPath ps = "/" <> mconcat (intersperse "/" (unRText' <$> ps))

rQuery :: [QueryParam] -> TLB.Builder
rQuery = \case
  [] -> mempty
  qs -> "?" <> mconcat (intersperse "&" (rQueryParam <$> qs))

rQueryParam :: QueryParam -> TLB.Builder
rQueryParam = \case
  QueryFlag flag -> unRText' flag
  QueryParam k v -> unRText' k <> "=" <> unRText' v

rFragment :: RText 'Fragment -> TLB.Builder
rFragment = (<> "#") . unRText'

-- | Render a given 'URI.Normalized' value as a strict 'ByteString'.

renderBs :: URI -> ByteString
renderBs = BL.toStrict . BLB.toLazyByteString . renderBs'

-- | Render a given 'URI' value as a 'BLB.Builder'.

renderBs' :: URI -> BLB.Builder
renderBs' = undefined -- TODO

----------------------------------------------------------------------------
-- Helpers

unRText' :: RText l -> TLB.Builder
unRText' = TLB.fromText . percentEncode . unRText

-- | Percent-encode a 'Text' value.

percentEncode :: Text -> Text
percentEncode = id -- FIXME

-- | The predicate selects unreserved bytes.

isUnreserved :: Word8 -> Bool
isUnreserved = \case
  45  -> True
  95  -> True
  46  -> True
  126 -> True
  _   -> False
