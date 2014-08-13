{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana where

--------------------------------------------------------------------------------
import Data.ByteString.Lazy (ByteString)
import Data.Typeable

--------------------------------------------------------------------------------
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString.Lazy
import Pipes

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
data RakhanaParserException
    = RakhanaParserException [String] String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception RakhanaParserException

--------------------------------------------------------------------------------
makeDocument :: MonadThrow m => ByteString -> m Document
makeDocument start
    = case parse parseHeader start of
        Fail _ ctx e
            -> throwM $ RakhanaParserException ctx e
        Done rest ver
            -> let doc = Document ver (objectProducer rest) in
               return doc

--------------------------------------------------------------------------------
objectProducer :: MonadThrow m => ByteString -> Producer' Structure m ()
objectProducer bytes
    = case parse parseIndirectObject bytes of
          Fail _ ctx e
              -> lift $ throwM $ RakhanaParserException ctx e
          Done rest iObj
              -> do yield $ IndObj iObj
                    objectProducer rest
