{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.URI.Parser.Text.Utils
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Random utilities for our 'Text' parsers.
module Text.URI.Parser.Text.Utils
  ( pHost,
    asciiAlphaChar,
    asciiAlphaNumChar,
    unreservedChar,
    percentEncChar,
    subDelimChar,
    pchar,
    pchar',
  )
where

import Control.Monad
import Data.Char
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import qualified Data.Set as E
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Parser that can parse host names.
pHost ::
  (MonadParsec e Text m) =>
  -- | Demand percent-encoding in reg names
  Bool ->
  m String
pHost pe =
  choice
    [ try (asConsumed ipLiteral),
      regName
    ]
  where
    asConsumed :: (MonadParsec e Text m) => m a -> m String
    asConsumed p = T.unpack . fst <$> match p
    ipLiteral =
      between (char '[') (char ']') $
        try ipv6Address <|> ipvFuture
    ipv6Address = do
      pos <- getOffset
      (toks, xs) <- match $ do
        xs' <- maybeToList <$> optional ([] <$ string "::")
        xs <- flip sepBy1 (char ':') $ do
          (skip, hasMore) <- lookAhead . hidden $ do
            skip <- option False (True <$ char ':')
            hasMore <- option False (True <$ hexDigitChar)
            return (skip, hasMore)
          case (skip, hasMore) of
            (True, True) -> return []
            (True, False) -> [] <$ char ':'
            (False, _) -> count' 1 4 hexDigitChar
        return (xs' ++ xs)
      let nskips = length (filter null xs)
          npieces = length xs
      unless (nskips < 2 && (npieces == 8 || (nskips == 1 && npieces < 8))) $ do
        setOffset pos
        failure
          (fmap Tokens . NE.nonEmpty . T.unpack $ toks)
          (E.singleton . Label . NE.fromList $ "valid IPv6 address")
    ipvFuture = do
      void (char 'v')
      void hexDigitChar
      void (char '.')
      skipSome (unreservedChar <|> subDelimChar <|> char ':')
    regName = fmap (intercalate ".") . flip sepBy1 (char '.') $ do
      many $
        if pe
          then percentEncChar <|> unreservedChar
          else unreservedCharUnicode
{-# INLINEABLE pHost #-}

-- | Parse an ASCII alpha character.
asciiAlphaChar :: (MonadParsec e Text m) => m Char
asciiAlphaChar = satisfy isAsciiAlpha <?> "ASCII alpha character"
{-# INLINE asciiAlphaChar #-}

-- | Parse an ASCII alpha-numeric character.
asciiAlphaNumChar :: (MonadParsec e Text m) => m Char
asciiAlphaNumChar = satisfy isAsciiAlphaNum <?> "ASCII alpha-numeric character"
{-# INLINE asciiAlphaNumChar #-}

-- | Parse an unreserved character.
unreservedChar :: (MonadParsec e Text m) => m Char
unreservedChar = label "unreserved character" . satisfy $ \x ->
  isAsciiAlphaNum x || x == '-' || x == '.' || x == '_' || x == '~'
{-# INLINE unreservedChar #-}

-- | Parse an unreserved character allowing Unicode.
unreservedCharUnicode :: (MonadParsec e Text m) => m Char
unreservedCharUnicode = label "unreserved character" . satisfy $ \x ->
  isAlphaNum x || x == '-' || x == '.' || x == '_' || x == '~'
{-# INLINE unreservedCharUnicode #-}

-- | Parse a percent-encoded character.
percentEncChar :: (MonadParsec e Text m) => m Char
percentEncChar = do
  void (char '%')
  h <- digitToInt <$> hexDigitChar
  l <- digitToInt <$> hexDigitChar
  return . chr $ h * 16 + l
{-# INLINE percentEncChar #-}

-- | Parse a sub-delimiter character.
subDelimChar :: (MonadParsec e Text m) => m Char
subDelimChar = oneOf s <?> "sub-delimiter"
  where
    s = E.fromList "!$&'()*+,;="
{-# INLINE subDelimChar #-}

-- | PCHAR thing from the spec.
pchar :: (MonadParsec e Text m) => m Char
pchar =
  choice
    [ unreservedChar,
      percentEncChar,
      subDelimChar,
      char ':',
      char '@'
    ]
{-# INLINE pchar #-}

-- | 'pchar' adjusted for query parsing.
pchar' :: (MonadParsec e Text m) => m Char
pchar' =
  choice
    [ unreservedChar,
      percentEncChar,
      char '+' >> pure ' ',
      oneOf s <?> "sub-delimiter",
      char ':',
      char '@'
    ]
  where
    s = E.fromList "!$'()*,;"
{-# INLINE pchar' #-}

isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAscii x && isAlpha x

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum x = isAscii x && isAlphaNum x
