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
import           Prelude hiding (null)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isDigit)
import           Data.Typeable

--------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString.Char8 (skipSpace)
import Data.Attoparsec.ByteString.Lazy
import Pipes
import Pipes.Core

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.Nursery

--------------------------------------------------------------------------------
app :: IO XRef
app = runDrive (fileTape "samples/IdiomLite.pdf") nursery
