-- |
-- Module      :  Text.URI.Parser
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- URI parsers, an internal module.

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.URI.Parser
  ( mkURI
  , parse
  , ParseException (..) )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (Exception (..), MonadThrow (..))
import Data.Char
import Data.Data (Data)
import Data.Maybe (isNothing, catMaybes)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.URI.Types
import qualified Data.ByteString.Char8      as B8
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as E
import qualified Data.Text.Encoding         as TE
import qualified Text.Megaparsec.Char.Lexer as L

-- | Construct a 'URI' from 'Text'. In case of failure 'ParseException' is
-- thrown.

mkURI :: MonadThrow m => Text -> m URI
mkURI input =
  case runParser (parse :: Parsec Void Text URI) "" input of
    Left err -> throwM (ParseException input err)
    Right x  -> return x

-- | Parse exception thrown by 'mkURI' when a given 'Text' value cannot be
-- parsed as a 'URI'.

data ParseException = ParseException Text (ParseError Char Void)
  deriving (Show, Eq, Data, Typeable, Generic)

instance Exception ParseException where
  displayException (ParseException s e) = parseErrorPretty' s e

-- | This parser can be used to parse 'URI' from strict 'Text'.

parse :: MonadParsec e Text m => m URI
parse = do
  uriScheme    <- optional (try pScheme)
  uriAuthority <- optional pAuthority
  uriPath      <- pPath (isNothing uriAuthority)
  uriQuery     <- option [] pQuery
  uriFragment  <- optional pFragment
  return URI {..}

pScheme :: MonadParsec e Text m => m (RText 'Scheme)
pScheme = do
  x  <- asciiAlphaChar
  xs <- many (asciiAlphaNumChar <|> char '+' <|> char '-' <|> char '.')
  void (char ':')
  liftR mkScheme (x:xs)

pAuthority :: MonadParsec e Text m => m Authority
pAuthority = do
  void (string "//")
  authUserInfo <- optional pUserInfo
  authHost <- pHost True >>= liftR mkHost
  authPort <- optional (char ':' *> L.decimal)
  return Authority {..}

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

pPath :: MonadParsec e Text m => Bool -> m [RText 'PathPiece]
pPath hadNoAuth = do
  doubleSlash <- lookAhead (True <$ string "//" <|> pure False)
  when (doubleSlash && hadNoAuth) $
    (unexpected . Tokens . NE.fromList) "//"
  path <- flip sepBy (char '/') . label "path piece" $
    many pchar
  mapM (liftR mkPathPiece) (filter (not . null) path)

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

pFragment :: MonadParsec e Text m => m (RText 'Fragment)
pFragment = do
  void (char '#')
  xs <- many . label "fragment character" $
    pchar <|> char '/' <|> char '?'
  liftR mkFragment xs

----------------------------------------------------------------------------
-- Helpers

liftR :: MonadParsec e s m
  => (forall n. MonadThrow n => Text -> n r)
  -> String
  -> m r
liftR f = maybe empty return . f . TE.decodeUtf8 . B8.pack

asciiAlphaChar :: MonadParsec e Text m => m Char
asciiAlphaChar = satisfy isAsciiAlpha <?> "ASCII alpha character"

asciiAlphaNumChar :: MonadParsec e Text m => m Char
asciiAlphaNumChar = satisfy isAsciiAlphaNum <?> "ASCII alpha-numeric character"

unreservedChar :: MonadParsec e Text m => m Char
unreservedChar = label "unreserved character" . satisfy $ \x ->
  isAsciiAlphaNum x || x == '-' || x == '.' || x == '_' || x == '~'

percentEncChar :: MonadParsec e Text m => m Char
percentEncChar = do
  void (char '%')
  h <- digitToInt <$> hexDigitChar
  l <- digitToInt <$> hexDigitChar
  return . chr $ h * 16 + l

subDelimChar :: MonadParsec e Text m => m Char
subDelimChar = oneOf s <?> "sub-delimiter"
  where
    s = E.fromList "!$&'()*+,;="

pchar :: MonadParsec e Text m => m Char
pchar = choice
  [ unreservedChar
  , percentEncChar
  , subDelimChar
  , char ':'
  , char '@' ]

pchar' :: MonadParsec e Text m => m Char
pchar' = choice
  [ unreservedChar
  , percentEncChar
  , oneOf s <?> "sub-delimiter"
  , char ':'
  , char '@' ]
  where
    s = E.fromList "!$'()*+,;"

isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAscii x && isAlpha x

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum x = isAscii x && isAlphaNum x
