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
driveParse :: Monad m => Int -> Parser a -> Drive m (Either String a)
driveParse bufferSize parser
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
driveParseObject :: Monad m
                 => Int
                 -> Drive m (Either String (Int, Int, Object))
driveParseObject bufSize
    = do rE <- driveParse bufSize parseIndirectObject
         case rE of
             Left e -> return $ Left e
             Right r
                 | Dict d <- r ^. _3
                   -> couldBeStreamObject (r ^. _1, r ^. _2) d
                 | otherwise
                   -> return $ Right r
  where
    couldBeStreamObject (idx, gen) d
        = do eR <- driveParse 16 parseStreamHeader
             case eR of
                 Left _
                     -> return $ Right $ (idx, gen, Dict d)
                 Right _
                     -> do p <- driveGetSeek
                           return $ Right $ (idx, gen, AStream $ Stream d p)
