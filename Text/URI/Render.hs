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

module Text.URI.Render
  ( render
  , render'
  , renderBs
  , renderBs' )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.URI.Types (URI)
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BLB
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Builder       as TLB

-- | Render a given 'URI' value as strict 'Text'.

render :: URI -> Text
render = TL.toStrict . TLB.toLazyText . render'

-- | Render a given 'URI' value as a 'TLB.Builder'.

render' :: URI -> TLB.Builder
render' = undefined -- TODO

-- | Render a given 'URI.Normalized' value as a strict 'ByteString'.

renderBs :: URI -> ByteString
renderBs = BL.toStrict . BLB.toLazyByteString . renderBs'

-- | Render a given 'URI' value as a 'BLB.Builder'.

renderBs' :: URI -> BLB.Builder
renderBs' = undefined -- TODO
