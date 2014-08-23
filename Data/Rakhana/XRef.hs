{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
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
module Data.Rakhana.XRef
    ( getXRef
    , getXRefPos
    ) where

--------------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char (isDigit, isSpace)
import           Data.Typeable

--------------------------------------------------------------------------------
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Attoparsec.ByteString
import Pipes.Safe ()

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape

--------------------------------------------------------------------------------
data XRefParsingException
    = XRefParsingException String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception XRefParsingException

--------------------------------------------------------------------------------
bufferSize :: Int
bufferSize = 4096

--------------------------------------------------------------------------------
getXRefPos :: MonadThrow m => Drive m Integer
getXRefPos
    = do driveBottom
         driveBackward
         skipEOL
         parseEOF
         skipEOL
         p <- parseXRefPosInteger
         skipEOL
         parseStartXRef
         return p

--------------------------------------------------------------------------------
getXRef :: MonadThrow m => Integer -> Drive m XRef
getXRef pos
    = do driveTop
         driveForward
         driveSeek pos
         bs <- driveGet bufferSize
         case parse parseXRef bs of
             Fail _ _ e -> throwM $ XRefParsingException e
             Partial k  -> loop k
             Done _ i   -> return i
  where
    loop k
        = do bs <- driveGet bufferSize
             case k bs of
                 Fail _ _ e -> throwM $ XRefParsingException e
                 Partial k' -> loop k'
                 Done _ i   -> return i

--------------------------------------------------------------------------------
skipEOL :: Monad m => Drive m ()
skipEOL
    = do bs <- drivePeek 1
         case B8.uncons bs of
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
               case B8.uncons bs of
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
