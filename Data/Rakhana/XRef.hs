{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
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
import           Data.Char (isDigit, isSpace)
import           Data.Typeable

--------------------------------------------------------------------------------
import Control.Monad.Catch (Exception, MonadThrow(..))
import Pipes.Safe ()

--------------------------------------------------------------------------------
import Data.Rakhana.Tape

--------------------------------------------------------------------------------
data XRefParsingException
    = XRefParsingException String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception XRefParsingException

--------------------------------------------------------------------------------
getXRefPos :: MonadThrow m => Drive m Integer
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
    = do bs <- drivePeek 1
         case L8.uncons bs of
             Just (c, _)
                 | isSpace c -> driveDiscard 1 >> skipEOL
                 | otherwise -> return ()
             _ -> return ()

--------------------------------------------------------------------------------
parseEOF :: MonadThrow m => Drive m ()
parseEOF
    = do bs <- driveGet 5
         case bs of
             "%%EOF" -> return ()
             _       -> throwM $ XRefParsingException "Expected %%EOF"

--------------------------------------------------------------------------------
parseXRefPosInteger :: MonadThrow m => Drive m Integer
parseXRefPosInteger = go []
  where
    go cs = do bs <- drivePeek 1
               case L8.uncons bs of
                   Just (c,_)
                       | isDigit c -> driveDiscard 1 >> go (c:cs)
                       | otherwise -> return $ read cs
                   _ -> return $ read cs

    end [] = throwM $ XRefParsingException "Invalid XRef position integer"
    end cs = return $ read cs

--------------------------------------------------------------------------------
parseStartXRef :: MonadThrow m => Drive m ()
parseStartXRef
    = do bs <- driveGet 9
         case bs of
             "startxref" -> return ()
             _           -> throwM $ XRefParsingException "Expected startxref"
