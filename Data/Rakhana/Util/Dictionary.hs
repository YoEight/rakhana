--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Util.Dictionary
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Util.Dictionary where

--------------------------------------------------------------------------------
import Prelude hiding (lookup)
import Control.Monad (MonadPlus, mzero)
import Data.ByteString (ByteString)
import Data.Map (lookup)

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Types

--------------------------------------------------------------------------------
getDictValue :: MonadPlus m => ByteString -> Dictionary -> m Object
getDictValue k dict = maybe mzero return $ lookup k dict
