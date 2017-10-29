-- |
-- Module      :  Text.URI.Parser.Text
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parser for strict 'Text', an internal module.

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.URI.Parser.Text
  ( mkURI
  , parser
  , ParseException (..) )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (Exception (..), MonadThrow (..))
import Data.Data (Data)
import Data.Maybe (isNothing, catMaybes)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.URI.Parser.Text.Utils
import Text.URI.Types
import qualified Data.ByteString.Char8      as B8
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text.Encoding         as TE
import qualified Text.Megaparsec.Char.Lexer as L

-- | Construct a 'URI' from 'Text'. In case of failure 'ParseException' is
-- thrown.
--
-- This function uses the 'parser' parser under the hood, which you can also
-- use directly in a Megaparsec parser.

mkURI :: MonadThrow m => Text -> m URI
mkURI input =
  case runParser (parser <* eof :: Parsec Void Text URI) "" input of
    Left err -> throwM (ParseException input err)
    Right x  -> return x

-- | Parse exception thrown by 'mkURI' when a given 'Text' value cannot be
-- parsed as a 'URI'.

data ParseException = ParseException Text (ParseError Char Void)
  -- ^ Arguments are: original input and parse error
  deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ParseException where
  displayException (ParseException s e) = parseErrorPretty' s e

-- | This parser can be used to parse 'URI' from strict 'Text'. Remember to
-- use a concrete non-polymorphic parser type for efficiency.

parser :: MonadParsec e Text m => m URI
parser = do
  uriScheme    <- optional (try pScheme)
  uriAuthority <- optional pAuthority
  uriPath      <- pPath (isNothing uriAuthority)
  uriQuery     <- option [] pQuery
  uriFragment  <- optional pFragment
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

pPath :: MonadParsec e Text m => Bool -> m [RText 'PathPiece]
pPath hadNoAuth = do
  doubleSlash <- lookAhead (True <$ string "//" <|> pure False)
  when (doubleSlash && hadNoAuth) $
    (unexpected . Tokens . NE.fromList) "//"
  path <- flip sepBy (char '/') . label "path piece" $
    many pchar
  mapM (liftR mkPathPiece) (filter (not . null) path)
{-# INLINE pPath #-}

pQuery :: MonadParsec e Text m => m [QueryParam]
pQuery = do
  void (char '?')
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
