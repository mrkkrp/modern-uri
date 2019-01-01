-- |
-- Module      :  Text.URI.Parser.Text
-- Copyright   :  © 2017–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parser for strict 'Text', an internal module.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.URI.Parser.Text
  ( mkURI
  , parser )
where

import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, catMaybes)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.URI.Parser.Text.Utils
import Text.URI.Types
import qualified Data.ByteString.Char8      as B8
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text.Encoding         as TE
import qualified Text.Megaparsec.Char.Lexer as L

#if !MIN_VERSION_megaparsec(6,4,0)
import Control.Applicative (empty)
#endif

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
    Left  b -> throwM (ParseException b)
    Right x -> return x

-- | This parser can be used to parse 'URI' from strict 'Text'. Remember to
-- use a concrete non-polymorphic parser type for efficiency.

parser :: MonadParsec e Text m => m URI
parser = do
  uriScheme          <- optional (try pScheme)
  mauth              <- optional pAuthority
  (absPath, uriPath) <- pPath (isJust mauth)
  uriQuery           <- option [] pQuery
  uriFragment        <- optional pFragment
  let uriAuthority = maybe (Left absPath) Right mauth
  return URI {..}
{-# INLINEABLE parser #-}
{-# SPECIALIZE parser :: Parsec Void Text URI #-}

pScheme :: MonadParsec e Text m => m (RText 'Scheme)
pScheme = do
  x  <- asciiAlphaChar
  xs <- many (asciiAlphaNumChar <|> char '+' <|> char '-' <|> char '.')
  void (char ':')
  liftR mkScheme (x:xs)
{-# INLINE pScheme #-}

pAuthority :: MonadParsec e Text m => m Authority
pAuthority = do
  void (string "//")
  authUserInfo <- optional pUserInfo
  authHost <- pHost True >>= liftR mkHost
  authPort <- optional (char ':' *> L.decimal)
  return Authority {..}
{-# INLINE pAuthority #-}

pUserInfo :: MonadParsec e Text m => m UserInfo
pUserInfo = try $ do
  uiUsername <- label "username" $
    many (unreservedChar <|> percentEncChar <|> subDelimChar)
      >>= liftR mkUsername
  uiPassword <- optional $ do
    void (char ':')
    many (unreservedChar <|> percentEncChar <|> subDelimChar <|> char ':')
      >>= liftR mkPassword
  void (char '@')
  return UserInfo {..}
{-# INLINE pUserInfo #-}

pPath :: MonadParsec e Text m
  => Bool
  -> m (Bool, Maybe (Bool, NonEmpty (RText 'PathPiece)))
pPath hasAuth = do
  doubleSlash <- lookAhead (option False (True <$ string "//"))
  when (doubleSlash && not hasAuth) $
    (unexpected . Tokens . NE.fromList) "//"
  absPath <- option False (True <$ char '/')
  (rawPieces, trailingSlash) <- flip runStateT False $
    flip sepBy (char '/') . label "path piece" $ do
      x <- many pchar
      put (null x)
      return x
  pieces <- mapM (liftR mkPathPiece) (filter (not . null) rawPieces)
  return
    ( absPath
    , case NE.nonEmpty pieces of
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
    k' <- p
    mv <- optional (char '=' *> p)
    k  <- liftR mkQueryKey k'
    if null k'
      then return Nothing
      else Just <$> case mv of
             Nothing -> return (QueryFlag k)
             Just v  -> QueryParam k <$> liftR mkQueryValue v
{-# INLINE pQuery #-}

pFragment :: MonadParsec e Text m => m (RText 'Fragment)
pFragment = do
  void (char '#')
  xs <- many . label "fragment character" $
    pchar <|> char '/' <|> char '?'
  liftR mkFragment xs
{-# INLINE pFragment #-}

----------------------------------------------------------------------------
-- Helpers

liftR :: MonadParsec e s m
  => (forall n. MonadThrow n => Text -> n r)
  -> String
  -> m r
liftR f = maybe empty return . f . TE.decodeUtf8 . B8.pack
{-# INLINE liftR #-}
