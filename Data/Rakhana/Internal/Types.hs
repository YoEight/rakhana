{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Internal.Types
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Internal.Types where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
import Control.Monad.Catch (MonadThrow)
import Pipes (Producer')

--------------------------------------------------------------------------------
data Number
    = Natural Int
    | Real Double
    deriving (Eq, Show)

--------------------------------------------------------------------------------
type Dictionary = [(ByteString, Object)]

--------------------------------------------------------------------------------
type Reference = (Int, Int)

--------------------------------------------------------------------------------
data Object
    = Number Number
    | Boolean Bool
    | Name ByteString
    | Dict Dictionary
    | Array [Object]
    | Bytes ByteString
    | Ref Int Int
    | Stream Dictionary ByteString
    | Null
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data IndirectObject
    = IndirectObject
      { indObjectIndex      :: !Int
      , indObjectGeneration :: !Int
      , indObject           :: !Object
      }
      deriving (Eq, Show)

--------------------------------------------------------------------------------
data Structure
    = IndObj IndirectObject
    | XRef
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data Document
    = Document
      { documentVersion :: (Int, Int)
      , documentObjects :: forall m. MonadThrow m => Producer' Structure m ()
      }
