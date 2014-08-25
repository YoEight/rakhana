{-# LANGUAGE RankNTypes #-}
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
import Data.Attoparsec.ByteString

--------------------------------------------------------------------------------
import Data.Rakhana.Tape

--------------------------------------------------------------------------------
parseRepeatedly :: Monad m => Int -> Parser a -> Drive m (Either String a)
parseRepeatedly bufferSize parser
    = do bs <- driveGet bufferSize
         case parse parser bs of
             Fail _ _ e -> return $ Left e
             Partial k  -> loop k
             Done _ a   -> return $ Right a
  where
    loop k
        = do bs <- driveGet bufferSize
             case k bs of
                 Fail _ _ e -> return $ Left e
                 Partial k' -> loop k'
                 Done _ i   -> return $ Right i
