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
    | Stream Dictionary Object
    | Null
    deriving (Eq, Show)
