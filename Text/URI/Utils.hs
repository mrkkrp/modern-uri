-- |
-- Module      :  Text.URI.Utils
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Random utilities, an internal module.

module Text.URI.Utils
  ( percentDecode
  , percentEncode )
where

import Data.Text (Text)

-- | Percent-decode given 'Text' value.

percentDecode :: Text -> Text
percentDecode = undefined -- TODO

-- | Percent-encode given 'Text' value.

percentEncode :: Text -> Text
percentEncode = undefined -- TODO
