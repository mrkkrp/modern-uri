{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.URI.Parser.ByteString
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parser for string 'ByteString', an internal module.
module Text.URI.Parser.ByteString
  ( mkURIBs,
    parserBs,
  )
where

import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, maybeToList)
import qualified Data.Set as E
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L
import Text.URI.Types hiding (pHost)

-- | Construct a 'URI' from 'ByteString'. The input you pass to 'mkURIBs'
-- must be a valid URI as per RFC 3986, that is, its components should be
-- percent-encoded where necessary. In case of parse failure
-- 'ParseExceptionBs' is thrown.
--
-- This function uses the 'parserBs' parser under the hood, which you can also
-- use directly in a Megaparsec parser.
--
-- @since 0.3.3.0
mkURIBs :: (MonadThrow m) => ByteString -> m URI
mkURIBs input =
  case runParser (parserBs <* eof :: Parsec Void ByteString URI) "" input of
    Left b -> throwM (ParseExceptionBs b)
    Right x -> return x

-- | This parser can be used to parse 'URI' from strict 'ByteString'.
-- Remember to use a concrete non-polymorphic parser type for efficiency.
--
-- @since 0.0.2.0
parserBs :: (MonadParsec e ByteString m) => m URI
parserBs = do
  uriScheme <- optional (try pScheme)
  mauth <- optional pAuthority
  (absPath, uriPath) <- pPath (isJust mauth)
  uriQuery <- option [] pQuery
  uriFragment <- optional pFragment
  let uriAuthority = maybe (Left absPath) Right mauth
  return URI {..}
{-# INLINEABLE parserBs #-}
{-# SPECIALIZE parserBs :: Parsec Void ByteString URI #-}

pScheme :: (MonadParsec e ByteString m) => m (RText 'Scheme)
pScheme = do
  r <- liftR "scheme" mkScheme $ do
    x <- asciiAlphaChar
    xs <- many (asciiAlphaNumChar <|> char 43 <|> char 45 <|> char 46)
    return (x : xs)
  void (char 58)
  return r
{-# INLINE pScheme #-}

pAuthority :: (MonadParsec e ByteString m) => m Authority
pAuthority = do
  void (string "//")
  authUserInfo <- optional pUserInfo
  authHost <- liftR "host" mkHost pHost
  authPort <- optional (char 58 *> L.decimal)
  return Authority {..}
{-# INLINE pAuthority #-}

-- | Parser that can parse host names.
pHost :: (MonadParsec e ByteString m) => m [Word8]
pHost =
  choice
    [ try (asConsumed ipLiteral),
      try (asConsumed ipv4Address),
      regName
    ]
  where
    asConsumed :: (MonadParsec e ByteString m) => m a -> m [Word8]
    asConsumed p = B.unpack . fst <$> match p
    ipLiteral =
      between (char 91) (char 93) $
        try ipv6Address <|> ipvFuture
    octet = do
      o <- getOffset
      (toks, x) <- match L.decimal
      when (x >= (256 :: Integer)) $ do
        setOffset o
        failure
          (fmap Tokens . NE.nonEmpty . B.unpack $ toks)
          (E.singleton . Label . NE.fromList $ "decimal number from 0 to 255")
    ipv4Address =
      count 3 (octet <* char 46) *> octet
    ipv6Address = do
      o <- getOffset
      (toks, xs) <- match $ do
        xs' <- maybeToList <$> optional ([] <$ string "::")
        xs <- flip sepBy1 (char 58) $ do
          (skip, hasMore) <- lookAhead . hidden $ do
            skip <- option False (True <$ char 58)
            hasMore <- option False (True <$ hexDigitChar)
            return (skip, hasMore)
          case (skip, hasMore) of
            (True, True) -> return []
            (True, False) -> [] <$ char 58
            (False, _) -> count' 1 4 hexDigitChar
        return (xs' ++ xs)
      let nskips = length (filter null xs)
          npieces = length xs
      unless (nskips < 2 && (npieces == 8 || (nskips == 1 && npieces < 8))) $ do
        setOffset o
        failure
          (fmap Tokens . NE.nonEmpty . B.unpack $ toks)
          (E.singleton . Label . NE.fromList $ "valid IPv6 address")
    ipvFuture = do
      void (char 118)
      void hexDigitChar
      void (char 46)
      skipSome (unreservedChar <|> subDelimChar <|> char 58)
    regName = fmap (intercalate [46]) . flip sepBy1 (char 46) $ do
      let ch = percentEncChar <|> unreservedChar
      mx <- optional ch
      case mx of
        Nothing -> return []
        Just x -> do
          let r =
                ch
                  <|> try
                    (char 45 <* (lookAhead . try) (ch <|> char 45))
          xs <- many r
          return (x : xs)

pUserInfo :: (MonadParsec e ByteString m) => m UserInfo
pUserInfo = try $ do
  uiUsername <-
    liftR
      "username"
      mkUsername
      ( label "username" $
          many (unreservedChar <|> percentEncChar <|> subDelimChar)
      )
  uiPassword <- optional $ do
    void (char 58)
    liftR
      "password"
      mkPassword
      (many (unreservedChar <|> percentEncChar <|> subDelimChar <|> char 58))
  void (char 64)
  return UserInfo {..}
{-# INLINE pUserInfo #-}

pPath ::
  (MonadParsec e ByteString m) =>
  Bool ->
  m (Bool, Maybe (Bool, NonEmpty (RText 'PathPiece)))
pPath hasAuth = do
  doubleSlash <- lookAhead (option False (True <$ string "//"))
  when (doubleSlash && not hasAuth) $
    (unexpected . Tokens . NE.fromList) [47, 47]
  absPath <- option False (True <$ char 47)
  let mkPathPiece' x =
        if T.null x
          then Just Nothing
          else Just <$> mkPathPiece x
  (maybePieces, trailingSlash) <- flip runStateT False $
    flip sepBy (char 47) $
      liftR "path piece" mkPathPiece' $
        label "path piece" $ do
          x <- many pchar
          put (null x)
          return x
  let pieces = catMaybes maybePieces
  return
    ( absPath,
      case NE.nonEmpty pieces of
        Nothing -> Nothing
        Just ps -> Just (trailingSlash, ps)
    )
{-# INLINE pPath #-}

pQuery :: (MonadParsec e ByteString m) => m [QueryParam]
pQuery = do
  void (char 63)
  void (optional (char 38))
  fmap catMaybes . flip sepBy (char 38) . label "query parameter" $ do
    let p = many (pchar' <|> char 47 <|> char 63)
    k <- liftR "query key" mkQueryKey p
    mv <- optional (char 61 *> liftR "query value" mkQueryValue p)
    return $
      if T.null (unRText k)
        then Nothing
        else
          Just
            ( case mv of
                Nothing -> QueryFlag k
                Just v -> QueryParam k v
            )
{-# INLINE pQuery #-}

pFragment :: (MonadParsec e ByteString m) => m (RText 'Fragment)
pFragment = do
  void (char 35)
  liftR
    "fragment"
    mkFragment
    ( many . label "fragment character" $
        pchar <|> char 47 <|> char 63
    )
{-# INLINE pFragment #-}

----------------------------------------------------------------------------
-- Helpers

-- | Lift a smart constructor that consumes 'Text' into a parser.
liftR ::
  (MonadParsec e ByteString m) =>
  -- | What is being parsed
  String ->
  -- | The smart constructor that produces the result
  (Text -> Maybe r) ->
  -- | How to parse @['Word8']@ that will be converted to 'Text' and fed to
  -- the smart constructor
  m [Word8] ->
  m r
liftR lbl f p = do
  o <- getOffset
  (toks, s) <- match p
  case TE.decodeUtf8' (B.pack s) of
    Left _ -> do
      let unexp = NE.fromList (B.unpack toks)
          expecting = NE.fromList (lbl ++ " that can be decoded as UTF-8")
      parseError
        ( TrivialError
            o
            (Just (Tokens unexp))
            (S.singleton (Label expecting))
        )
    Right text -> maybe empty return (f text)
{-# INLINE liftR #-}

asciiAlphaChar :: (MonadParsec e ByteString m) => m Word8
asciiAlphaChar = satisfy isAsciiAlpha <?> "ASCII alpha character"
{-# INLINE asciiAlphaChar #-}

asciiAlphaNumChar :: (MonadParsec e ByteString m) => m Word8
asciiAlphaNumChar = satisfy isAsciiAlphaNum <?> "ASCII alpha-numeric character"
{-# INLINE asciiAlphaNumChar #-}

unreservedChar :: (MonadParsec e ByteString m) => m Word8
unreservedChar = label "unreserved character" . satisfy $ \x ->
  isAsciiAlphaNum x || x == 45 || x == 46 || x == 95 || x == 126
{-# INLINE unreservedChar #-}

percentEncChar :: (MonadParsec e ByteString m) => m Word8
percentEncChar = do
  void (char 37)
  h <- restoreDigit <$> hexDigitChar
  l <- restoreDigit <$> hexDigitChar
  return (h * 16 + l)
{-# INLINE percentEncChar #-}

subDelimChar :: (MonadParsec e ByteString m) => m Word8
subDelimChar = oneOf s <?> "sub-delimiter"
  where
    s = E.fromList (fromIntegral . ord <$> "!$&'()*+,;=")
{-# INLINE subDelimChar #-}

pchar :: (MonadParsec e ByteString m) => m Word8
pchar =
  choice
    [ unreservedChar,
      percentEncChar,
      subDelimChar,
      char 58,
      char 64
    ]
{-# INLINE pchar #-}

pchar' :: (MonadParsec e ByteString m) => m Word8
pchar' =
  choice
    [ unreservedChar,
      percentEncChar,
      char 43 >> pure 32,
      oneOf s <?> "sub-delimiter",
      char 58,
      char 64
    ]
  where
    s = E.fromList (fromIntegral . ord <$> "!$'()*,;")
{-# INLINE pchar' #-}

isAsciiAlpha :: Word8 -> Bool
isAsciiAlpha x
  | 65 <= x && x <= 90 = True
  | 97 <= x && x <= 122 = True
  | otherwise = False

isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum x
  | isAsciiAlpha x = True
  | 48 <= x && x <= 57 = True
  | otherwise = False

restoreDigit :: Word8 -> Word8
restoreDigit x
  | 48 <= x && x <= 57 = x - 48
  | 65 <= x && x <= 70 = x - 55
  | 97 <= x && x <= 102 = x - 87
  | otherwise = error "Text.URI.Parser.restoreDigit: bad input"
