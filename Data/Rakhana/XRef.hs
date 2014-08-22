{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.XRef
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.XRef (getXRefPos) where

--------------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isDigit)

--------------------------------------------------------------------------------
import Data.Rakhana.Tape

--------------------------------------------------------------------------------
getXRefPos :: Monad m => Drive m Integer
getXRefPos
    = do driveBottom
         driveDirection Backward
         skipEOL
         parseEOF
         skipEOL
         p <- parseXRefPosInteger
         skipEOL
         parseStartXRef
         return p

--------------------------------------------------------------------------------
skipEOL :: Monad m => Drive m ()
skipEOL
    = do c <- drivePeek 1
         when (c == "\n") (driveDiscard 1 >> skipEOL)

--------------------------------------------------------------------------------
parseEOF :: Monad m => Drive m ()
parseEOF
    = do "%%EOF" <- driveGet 5
         return ()

--------------------------------------------------------------------------------
parseXRefPosInteger :: Monad m => Drive m Integer
parseXRefPosInteger = go []
  where
    go cs = do bs <- drivePeek 1
               case L8.uncons bs of
                   Just (c,_)
                       | isDigit c -> driveDiscard 1 >> go (c:cs)
                       | otherwise -> return $ read cs
                   _ -> return $ read cs

--------------------------------------------------------------------------------
parseStartXRef :: Monad m => Drive m ()
parseStartXRef
    = do "startxref" <- driveGet 9
         return ()
