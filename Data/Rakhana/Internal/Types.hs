{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
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
import           Control.Applicative (pure)
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Monoid
import qualified Data.Vector     as V

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Pipes (Producer')

--------------------------------------------------------------------------------
data Number
    = Natural Integer
    | Real Double
    deriving (Eq, Show)

--------------------------------------------------------------------------------
type Dictionary = M.Map ByteString Object

--------------------------------------------------------------------------------
type Reference = (Int, Int)

--------------------------------------------------------------------------------
data Object
    = Number Number
    | Boolean Bool
    | Name ByteString
    | Dict Dictionary
    | Array (V.Vector Object)
    | Bytes ByteString
    | Ref Int Int
    | Stream Dictionary Integer
    | Null
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data TableEntry
    = TableEntry
      { tableEntryOffset     :: !Integer
      , tableEntryGeneration :: !Int
      , tableEntryFree       :: !Bool
      }
      deriving Show

--------------------------------------------------------------------------------
data XRef
    = XRef
      { xrefHeader  :: !(Int, Int)
      , xrefEntries :: !(M.Map Reference TableEntry)
      , xrefTrailer :: !Dictionary
      }
      deriving Show

--------------------------------------------------------------------------------
data Header
    = Header
      { headerMaj :: !Int
      , headerMin :: !Int
      }
      deriving Show

--------------------------------------------------------------------------------
-- Prisms
--------------------------------------------------------------------------------
_Number :: Prism' Object Number
_Number = prism' Number go
  where
    go (Number n) = Just n
    go _          = Nothing

--------------------------------------------------------------------------------
_Natural :: Prism' Number Integer
_Natural = prism' Natural go
  where
    go (Natural n) = Just n
    go _           = Nothing

--------------------------------------------------------------------------------
_Real :: Prism' Number Double
_Real = prism' Real go
  where
    go (Real r) = Just r
    go _        = Nothing

--------------------------------------------------------------------------------
_Boolean :: Prism' Object Bool
_Boolean = prism' Boolean go
  where
    go (Boolean b) = Just b
    go _           = Nothing

--------------------------------------------------------------------------------
_Name :: Prism' Object ByteString
_Name = prism' Name go
  where
    go (Name b) = Just b
    go _        = Nothing

--------------------------------------------------------------------------------
_Dict :: Prism' Object Dictionary
_Dict = prism' Dict go
  where
    go (Dict d) = Just d
    go _        = Nothing

--------------------------------------------------------------------------------
_Array :: Prism' Object (V.Vector Object)
_Array = prism' Array go
  where
    go (Array a) = Just a
    go _         = Nothing

--------------------------------------------------------------------------------
_Bytes :: Prism' Object ByteString
_Bytes = prism' Bytes go
  where
    go (Bytes b) = Just b
    go _         = Nothing

--------------------------------------------------------------------------------
_Ref :: Prism' Object Reference
_Ref = prism' (\(i,g) -> Ref i g) go
  where
    go (Ref i g) = Just (i,g)
    go _         = Nothing

--------------------------------------------------------------------------------
_Stream :: Prism' Object (Dictionary, Integer)
_Stream = prism' (\(d,s) -> Stream d s) go
  where
    go (Stream d b) = Just (d, b)
    go _            = Nothing

--------------------------------------------------------------------------------
dictKey :: ByteString -> Traversal' Dictionary Object
dictKey key k d
    = case M.lookup key d of
          Nothing -> pure d
          Just o  -> fmap go $ k o
  where
    go o' = M.insert key o' d

--------------------------------------------------------------------------------
nth :: Int -> Traversal' (V.Vector a) a
nth i k ar
    = case ar V.!? i of
          Nothing -> pure ar
          Just a  -> fmap go $ k a
  where
    go a' = ar V.// [(i,a')]
