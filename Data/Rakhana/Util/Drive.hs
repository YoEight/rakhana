{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Util.Drive
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Util.Drive where

--------------------------------------------------------------------------------
import qualified Data.ByteString as B
import           Data.Typeable

--------------------------------------------------------------------------------
import Control.Exception
import Control.Lens
import Control.Monad.Catch (MonadThrow(..))
import Data.Attoparsec.ByteString
import Pipes.Safe ()

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape

--------------------------------------------------------------------------------
data ParsingException = ParsingException String deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception ParsingException

--------------------------------------------------------------------------------
parseRepeatedly :: Monad m => Int -> Parser a -> Drive m (Either String a)
parseRepeatedly bufferSize parser
    = loop Nothing
  where
    loop mK
        = do bs <- driveGet bufferSize
             case maybe (parse parser bs) ($ bs) mK of
                 Fail _ _ e -> return $ Left e
                 Partial k  -> loop $ Just k
                 Done r a   -> do let len = fromIntegral $ B.length r
                                  driveModifySeek (\i -> i - len)
                                  return $ Right a

--------------------------------------------------------------------------------
driveParse :: MonadThrow m => Int -> Parser a -> Drive m a
driveParse bufSize parser
    = do eR <- parseRepeatedly bufSize parser
         case eR of
             Left e  -> throwM $ ParsingException e
             Right a -> return a

--------------------------------------------------------------------------------
driveParseObject :: MonadThrow m => Int -> Drive m (Int, Int, Object)
driveParseObject i
    = do rE <- driveParseObjectE i
         either (throwM . ParsingException) return rE

--------------------------------------------------------------------------------
driveParseObjectE :: Monad m
                  => Int
                  -> Drive m (Either String (Int, Int, Object))
driveParseObjectE bufSize
    = do rE <- parseRepeatedly bufSize parseIndirectObject
         case rE of
             Left e -> return $ Left e
             Right r
                 | Dict d <- r ^. _3
                   -> couldBeStreamObject (r ^. _1, r ^. _2) d
                 | otherwise
                   -> return $ Right r
  where
    couldBeStreamObject (idx, gen) d
        = do eR <- parseRepeatedly 16 parseStreamHeader
             case eR of
                 Left _
                     -> return $ Right $ (idx, gen, Dict d)
                 Right _
                     -> do p <- driveGetSeek
                           return $ Right $ (idx, gen, AStream $ Stream d p)
