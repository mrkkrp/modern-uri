{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.URI.Parser.Text
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parser for strict 'Text', an internal module.
module Text.URI.Parser.Text
  ( mkURI,
    parser,
  )
where

import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.URI.Parser.Text.Utils
import Text.URI.Types

-- | Construct a 'URI' from 'Text'. The input you pass to 'mkURI' must be a
-- valid URI as per RFC 3986, that is, its components should be
-- percent-encoded where necessary. In case of parse failure
-- 'ParseException' is thrown.
--
-- This function uses the 'parser' parser under the hood, which you can also
-- use directly in a Megaparsec parser.
mkURI :: MonadThrow m => Text -> m URI
mkURI input =
  case runParser (parser <* eof :: Parsec Void Text URI) "" input of
    Left b -> throwM (ParseException b)
    Right x -> return x

-- | This parser can be used to parse 'URI' from strict 'Text'. Remember to
-- use a concrete non-polymorphic parser type for efficiency.
parser :: MonadParsec e Text m => m URI
parser = do
  uriScheme <- optional (try pScheme)
  mauth <- optional pAuthority
  (absPath, uriPath) <- pPath (isJust mauth)
  uriQuery <- option [] pQuery
  uriFragment <- optional pFragment
  let uriAuthority = maybe (Left absPath) Right mauth
  return URI {..}
{-# INLINEABLE parser #-}
{-# SPECIALIZE parser :: Parsec Void Text URI #-}

pScheme :: MonadParsec e Text m => m (RText 'Scheme)
pScheme = do
  r <- liftR "scheme" mkScheme $ do
    x <- asciiAlphaChar
    xs <- many (asciiAlphaNumChar <|> char '+' <|> char '-' <|> char '.')
    return (x : xs)
  void (char ':')
  return r
{-# INLINE pScheme #-}

pAuthority :: MonadParsec e Text m => m Authority
pAuthority = do
  void (string "//")
  authUserInfo <- optional pUserInfo
  authHost <- liftR "host" mkHost (pHost True)
  authPort <- optional (char ':' *> L.decimal)
  return Authority {..}
{-# INLINE pAuthority #-}

pUserInfo :: MonadParsec e Text m => m UserInfo
pUserInfo = try $ do
  uiUsername <-
    liftR
      "username"
      mkUsername
      ( label "username" $
          many (unreservedChar <|> percentEncChar <|> subDelimChar)
      )
  uiPassword <- optional $ do
    void (char ':')
    liftR
      "password"
      mkPassword
      (many (unreservedChar <|> percentEncChar <|> subDelimChar <|> char ':'))
  void (char '@')
  return UserInfo {..}
{-# INLINE pUserInfo #-}

pPath ::
  MonadParsec e Text m =>
  Bool ->
  m (Bool, Maybe (Bool, NonEmpty (RText 'PathPiece)))
pPath hasAuth = do
  doubleSlash <- lookAhead (option False (True <$ string "//"))
  when (doubleSlash && not hasAuth) $
    (unexpected . Tokens . NE.fromList) "//"
  absPath <- option False (True <$ char '/')
  let mkPathPiece' x =
        if T.null x
          then Just Nothing
          else Just <$> mkPathPiece x
  (maybePieces, trailingSlash) <- flip runStateT False $
    flip sepBy (char '/') $
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

pQuery :: MonadParsec e Text m => m [QueryParam]
pQuery = do
  void (char '?')
  void (optional (char '&'))
  fmap catMaybes . flip sepBy (char '&') . label "query parameter" $ do
    let p = many (pchar' <|> char '/' <|> char '?')
    k <- liftR "query key" mkQueryKey p
    mv <- optional (char '=' *> liftR "query value" mkQueryValue p)
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

pFragment :: MonadParsec e Text m => m (RText 'Fragment)
pFragment = do
  void (char '#')
  liftR
    "fragment"
    mkFragment
    ( many . label "fragment character" $
        pchar <|> char '/' <|> char '?'
    )
{-# INLINE pFragment #-}

----------------------------------------------------------------------------
-- Helpers

-- | Lift a smart constructor that consumes 'Text' into a parser.
liftR ::
  MonadParsec e Text m =>
  -- | What is being parsed
  String ->
  -- | The smart constructor that produces the result
  (Text -> Maybe r) ->
  -- | How to parse 'String' that will be converted to 'Text' and fed to
  -- the smart constructor
  m String ->
  m r
liftR lbl f p = do
  o <- getOffset
  (toks, s) <- match p
  case TE.decodeUtf8' (B8.pack s) of
    Left _ -> do
      let unexp = NE.fromList (T.unpack toks)
          expecting = NE.fromList (lbl ++ " that can be decoded as UTF-8")
      parseError
        ( TrivialError
            o
            (Just (Tokens unexp))
            (S.singleton (Label expecting))
        )
    Right text -> maybe empty return (f text)
{-# INLINE liftR #-}
