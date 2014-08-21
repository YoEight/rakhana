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
import           Prelude hiding (null)
import qualified Data.ByteString.Lazy as L
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

--------------------------------------------------------------------------------
data RakhanaParserException
    = RakhanaParserException [String] String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception RakhanaParserException

--------------------------------------------------------------------------------
makeDocument :: MonadThrow m => L.ByteString -> m Document
makeDocument start
    = case parse parseHeader start of
        Fail _ ctx e
            -> throwM $ RakhanaParserException ctx e
        Done rest header
            -> let doc = Document header (objectProducer rest) in
               return doc

--------------------------------------------------------------------------------
objectProducer :: MonadThrow m => L.ByteString -> Producer' Structure m ()
objectProducer bytes
    = case parse skipSpace bytes of
          Done cl _
              | L.null cl -> return ()
              | otherwise
                -> case parse parser bytes of
                       Fail r ctx e
                           -> lift $ throwM $ RakhanaParserException ctx e
                       Done rest struct
                           -> do yield struct
                                 objectProducer rest
  where
    parser = parseIndirectObject <|> parseXRef

--------------------------------------------------------------------------------
fooBytes :: L.ByteString -> L.ByteString
fooBytes input
    = case parse parseHeader input of
          Done rest _   -> L.take 30 rest
          Fail rest _ e -> L.take 30 rest

--------------------------------------------------------------------------------
driveTest :: Drive ()
driveTest
    = do driveBottom
         driveDirection Backward
         bs <- driveGet 30
         liftIO $ print bs

--------------------------------------------------------------------------------
app :: IO ()
app = runDrive (fileTape "samples/IdiomLite.pdf") driveTest
