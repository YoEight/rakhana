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
import qualified Data.ByteString.Lazy as BL
import           Data.String (IsString(..))

--------------------------------------------------------------------------------
data Number
    = Natural Int
    | Real Double
    deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Boolean = Boolean Bool deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Name = Name BL.ByteString deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Dict = Dict [(Name, Object)] deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Array = Array [Object] deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Bytes = Bytes BL.ByteString deriving (Eq, Show)

--------------------------------------------------------------------------------
data Stream = Stream Dict Object deriving (Eq, Show)

--------------------------------------------------------------------------------
data Ref = Ref Int Int deriving (Eq, Show)

--------------------------------------------------------------------------------
data Object
    = ONumber Number
    | OBoolean Boolean
    | OName Name
    | ODict Dict
    | OArray Array
    | OBytes Bytes
    | ORef Ref
    | ONull
    deriving (Eq, Show)

--------------------------------------------------------------------------------
instance IsString Name where
    fromString = Name . fromString

--------------------------------------------------------------------------------
instance IsString Bytes where
    fromString = Bytes . fromString
