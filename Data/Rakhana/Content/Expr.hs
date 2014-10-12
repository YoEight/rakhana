--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Content.Expr
-- Copyright : (C) 2013 Yuras Shumovich, (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Content.Expr where

--------------------------------------------------------------------------------
import Data.Rakhana.Content.Operator
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
-- | Expression is a regular objects or an operators
data Expr
    = Obj Object
    | Op Op
    deriving (Show, Eq)
