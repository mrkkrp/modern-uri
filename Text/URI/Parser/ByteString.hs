-- |
-- Module      :  Text.URI.Parser.ByteString
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parser for string 'ByteString', an internal module.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.URI.Parser.ByteString
  ( parserBs )
where

import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString (ByteString)
import Data.Char
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (isJust, catMaybes, maybeToList)
import Data.Text (Text)
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.URI.Types hiding (pHost)
import qualified Data.ByteString            as B
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as E
import qualified Data.Text.Encoding         as TE
import qualified Text.Megaparsec.Byte.Lexer as L

#if !MIN_VERSION_megaparsec(6,4,0)
import Control.Applicative (empty)
#endif

-- | This parser can be used to parse 'URI' from strict 'ByteString'.
-- Remember to use a concrete non-polymorphic parser type for efficiency.
--
-- @since 0.0.2.0

parserBs :: MonadParsec e ByteString m => m URI
parserBs = do
  uriScheme          <- optional (try pScheme)
  mauth              <- optional pAuthority
  (absPath, uriPath) <- pPath (isJust mauth)
  uriQuery           <- option [] pQuery
  uriFragment        <- optional pFragment
  let uriAuthority = maybe (Left absPath) Right mauth
  return URI {..}
{-# INLINEABLE parserBs #-}
{-# SPECIALIZE parserBs :: Parsec Void ByteString URI #-}

pScheme :: MonadParsec e ByteString m => m (RText 'Scheme)
pScheme = do
  x  <- asciiAlphaChar
  xs <- many (asciiAlphaNumChar <|> char 43 <|> char 45 <|> char 46)
  void (char 58)
  liftR mkScheme (x:xs)
{-# INLINE pScheme #-}

pAuthority :: MonadParsec e ByteString m => m Authority
pAuthority = do
  void (string "//")
  authUserInfo <- optional pUserInfo
  authHost <- pHost >>= liftR mkHost
  authPort <- optional (char 58 *> L.decimal)
  return Authority {..}
{-# INLINE pAuthority #-}

-- | Parser that can parse host names.

pHost :: MonadParsec e ByteString m => m [Word8]
pHost = choice
  [ try (asConsumed ipLiteral)
  , try (asConsumed ipv4Address)
  , regName ]
  where
    asConsumed :: MonadParsec e ByteString m => m a -> m [Word8]
    asConsumed p = B.unpack . fst <$> match p
    ipLiteral = between (char 91) (char 93) $
      try ipv6Address <|> ipvFuture
    octet = do
      pos       <- getNextTokenPosition
      (toks, x) <- match L.decimal
      when (x >= (256 :: Integer)) $ do
        mapM_ setPosition pos
        failure
          (fmap Tokens . NE.nonEmpty . B.unpack $ toks)
          (E.singleton . Label . NE.fromList $ "decimal number from 0 to 255")
    ipv4Address =
      count 3 (octet <* char 46) *> octet
    ipv6Address = do
      pos        <- getNextTokenPosition
      (toks, xs) <- match $ do
        xs' <- maybeToList <$> optional ([] <$ string "::")
        xs  <- flip sepBy1 (char 58) $ do
          (skip, hasMore) <- lookAhead . hidden $ do
            skip    <- option False (True <$ char 58)
            hasMore <- option False (True <$ hexDigitChar)
            return (skip, hasMore)
          case (skip, hasMore) of
            (True,  True)  -> return []
            (True,  False) -> [] <$ char 58
            (False, _)     -> count' 1 4 hexDigitChar
        return (xs' ++ xs)
      let nskips  = length (filter null xs)
          npieces = length xs
      unless (nskips < 2 && (npieces == 8 || (nskips == 1 && npieces < 8))) $ do
        mapM_ setPosition pos
        failure
          (fmap Tokens . NE.nonEmpty . B.unpack $ toks)
          (E.singleton . Label . NE.fromList $ "valid IPv6 address")
    ipvFuture = do
      void (char 118)
      void hexDigitChar
      void (char 46)
      skipSome (unreservedChar <|> subDelimChar <|> char 58)
    regName = fmap (intercalate [46]) . flip sepBy1 (char 46) $ do
      let ch = percentEncChar <|> asciiAlphaNumChar
      x <- ch
      let r = ch <|> try
            (char 45 <* (lookAhead . try) (ch <|> char 45))
      xs <- many r
      return (x:xs)

pUserInfo :: MonadParsec e ByteString m => m UserInfo
pUserInfo = try $ do
  uiUsername <- label "username" $
    many (unreservedChar <|> percentEncChar <|> subDelimChar)
      >>= liftR mkUsername
  uiPassword <- optional $ do
    void (char 58)
    many (unreservedChar <|> percentEncChar <|> subDelimChar <|> char 58)
      >>= liftR mkPassword
  void (char 64)
  return UserInfo {..}
{-# INLINE pUserInfo #-}

pPath :: MonadParsec e ByteString m => Bool -> m (Bool, Maybe (Bool, NonEmpty (RText 'PathPiece)))
pPath hasAuth = do
  doubleSlash <- lookAhead (option False (True <$ string "//"))
  when (doubleSlash && not hasAuth) $
    (unexpected . Tokens . NE.fromList) [47,47]
  absPath <- option False (True <$ char 47)
  path <- flip sepBy (char 47) . label "path piece" $
    many pchar
  let trailingSlash = if null path then False else null (last path)
  pieces <- mapM (liftR mkPathPiece) (filter (not . null) path)
  if null pieces
    then return (absPath, Nothing)
    else return (absPath, Just (trailingSlash, fromList pieces))
{-# INLINE pPath #-}

pQuery :: MonadParsec e ByteString m => m [QueryParam]
pQuery = do
  void (char 63)
  fmap catMaybes . flip sepBy (char 38) . label "query parameter" $ do
    let p = many (pchar' <|> char 47 <|> char 63)
    k' <- p
    mv <- optional (char 61 *> p)
    k  <- liftR mkQueryKey k'
    if null k'
      then return Nothing
      else Just <$> case mv of
             Nothing -> return (QueryFlag k)
             Just v  -> QueryParam k <$> liftR mkQueryValue v
{-# INLINE pQuery #-}

pFragment :: MonadParsec e ByteString m => m (RText 'Fragment)
pFragment = do
  void (char 35)
  xs <- many . label "fragment character" $
    pchar <|> char 47 <|> char 63
  liftR mkFragment xs
{-# INLINE pFragment #-}

----------------------------------------------------------------------------
-- Helpers

liftR :: MonadParsec e s m
  => (forall n. MonadThrow n => Text -> n r)
  -> [Word8]
  -> m r
liftR f = maybe empty return . f . TE.decodeUtf8 . B.pack
{-# INLINE liftR #-}

asciiAlphaChar :: MonadParsec e ByteString m => m Word8
asciiAlphaChar = satisfy isAsciiAlpha <?> "ASCII alpha character"
{-# INLINE asciiAlphaChar #-}

asciiAlphaNumChar :: MonadParsec e ByteString m => m Word8
asciiAlphaNumChar = satisfy isAsciiAlphaNum <?> "ASCII alpha-numeric character"
{-# INLINE asciiAlphaNumChar #-}

unreservedChar :: MonadParsec e ByteString m => m Word8
unreservedChar = label "unreserved character" . satisfy $ \x ->
  isAsciiAlphaNum x || x == 45 || x == 46 || x == 95 || x == 126
{-# INLINE unreservedChar #-}

percentEncChar :: MonadParsec e ByteString m => m Word8
percentEncChar = do
  void (char 37)
  h <- restoreDigit <$> hexDigitChar
  l <- restoreDigit <$> hexDigitChar
  return (h * 16 + l)
{-# INLINE percentEncChar #-}

subDelimChar :: MonadParsec e ByteString m => m Word8
subDelimChar = oneOf s <?> "sub-delimiter"
  where
    s = E.fromList (fromIntegral . ord <$> "!$&'()*+,;=")
{-# INLINE subDelimChar #-}

pchar :: MonadParsec e ByteString m => m Word8
pchar = choice
  [ unreservedChar
  , percentEncChar
  , subDelimChar
  , char 58
  , char 64 ]
{-# INLINE pchar #-}

pchar' :: MonadParsec e ByteString m => m Word8
pchar' = choice
  [ unreservedChar
  , percentEncChar
  , char 43 >> pure 32
  , oneOf s <?> "sub-delimiter"
  , char 58
  , char 64 ]
  where
    s = E.fromList (fromIntegral . ord <$> "!$'()*,;")
{-# INLINE pchar' #-}

isAsciiAlpha :: Word8 -> Bool
isAsciiAlpha x
  | 65 <= x && x <= 90  = True
  | 97 <= x && x <= 122 = True
  | otherwise           = False

isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum x
  | isAsciiAlpha x     = True
  | 48 <= x && x <= 57 = True
  | otherwise          = False

restoreDigit :: Word8 -> Word8
restoreDigit x
  | 48 <= x && x <= 57  = x - 48
  | 65 <= x && x <= 70  = x - 55
  | 97 <= x && x <= 102 = x - 87
  | otherwise           = error "Text.URI.Parser.restoreDigit: bad input"
