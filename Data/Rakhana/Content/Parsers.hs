{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Content.Parsers
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Content.Parsers where

--------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as L

--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString.Char8 (skipSpace, takeWhile1)
import Data.Attoparsec.ByteString.Lazy (Parser, Result(..), parse)
import Pipes

--------------------------------------------------------------------------------
import Data.Rakhana.Content.Expr
import Data.Rakhana.Content.Operator
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
parseOp :: Parser Op
parseOp = fmap toOp $ takeWhile1 isRegularChar

--------------------------------------------------------------------------------
parseContentObject :: Parser Object
parseContentObject = skipSpace >> go
  where
    go = parseName        <|>
         parseDict        <|>
         parseBoolean     <|>
         parseArray       <|>
         parseHexBytes    <|>
         parseStringBytes <|>
         parseNumber      <|>
         parseNull

--------------------------------------------------------------------------------
parseExpr :: Parser Expr
parseExpr = skipSpace >> (fmap Obj parseContentObject <|> fmap Op parseOp)

--------------------------------------------------------------------------------
exprs :: Monad m => L.ByteString -> Producer' Expr m ()
exprs bytes = loop bytes
  where
    loop src
        = case parse parseExpr src of
              Fail _ _ _
                  -> return ()
              Done rest expr
                  -> yield expr >> loop rest
