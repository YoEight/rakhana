{-# LANGUAGE RankNTypes #-}
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

--------------------------------------------------------------------------------
import Control.Monad.Trans (liftIO)
import Data.Attoparsec.ByteString.Lazy
import Pipes

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
produceObjects :: ByteString -> Producer' (Reference, Object) IO ()
produceObjects start
    = case parse parseHeader start of
          Fail _ _ e
              -> liftIO $ fail e
          Done rest _
              -> body rest
  where
    body rest
        = case parse parseIndirectObject rest  of
              Fail _ _ e ->
                  liftIO $ fail e
              Done rest' tup
                  -> do yield tup
                        body rest'
