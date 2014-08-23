{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Nursery
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Nursery (nursery) where

--------------------------------------------------------------------------------
import Data.Typeable hiding (Proxy)

--------------------------------------------------------------------------------
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Attoparsec.ByteString
import Pipes

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.XRef

--------------------------------------------------------------------------------
data NurseryParsingException
    = NurseryParsingException String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
type Nursery m a = Proxy Req Resp NurseryReq NurseryResp m a

--------------------------------------------------------------------------------
data NurseryReq
data NurseryResp

--------------------------------------------------------------------------------
instance Exception NurseryParsingException

--------------------------------------------------------------------------------
nursery :: MonadThrow m => Drive m XRef
nursery
    = do h   <- getHeader
         pos <- getXRefPos
         getXRef pos

--------------------------------------------------------------------------------
getHeader :: MonadThrow m => Drive m Header
getHeader
    = do bs <- driveGet 8
         case parseOnly parseHeader bs of
             Left e  -> throwM $ NurseryParsingException e
             Right h -> return h
