{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Nursery
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Nursery
    ( Playground
    , nurseryGetInfo
    , nurseryGetHeader
    , nurseryGetPages
    , nurseryResolve
    , withNursery
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString
import qualified Data.Map.Strict as M
import           Data.Traversable (forM)
import           Data.Typeable hiding (Proxy)

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Attoparsec.ByteString
import Pipes hiding (Effect)
import Pipes.Core

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.Util.Dictionary
import Data.Rakhana.Util.Drive
import Data.Rakhana.XRef

--------------------------------------------------------------------------------
data NurseryException
    = NurseryParsingException String
    | NurseryUnresolvedObject Int Int
    | NurseryRootNotFound
    | NurseryPagesNotFound
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
type Nursery m a = Proxy Req Resp NReq NResp m a
type Playground m a = Client' NReq NResp m a
type Root = Dictionary
type Pages = Dictionary

--------------------------------------------------------------------------------
data NReq
    = RqInfo
    | RqHeader
    | RqPages
    | RqResolve Reference

--------------------------------------------------------------------------------
data NResp
    = Unit
    | RInfo Dictionary
    | RHeader Header
    | RPages Dictionary
    | RResolve Object

--------------------------------------------------------------------------------
instance Exception NurseryException

--------------------------------------------------------------------------------
data NurseryState
    = NurseryState
      { nurseryHeader :: !Header
      , nurseryXRef   :: !XRef
      , nurseryRoot   :: !Dictionary
      , nurseryInfo   :: !Dictionary
      , nurseryPages  :: !Dictionary
      }

--------------------------------------------------------------------------------
bufferSize :: Int
bufferSize = 4096

--------------------------------------------------------------------------------
nursery :: MonadThrow m => Nursery m a
nursery
    = do h     <- getHeader
         pos   <- getXRefPos
         xref  <- getXRef pos
         info  <- getInfo xref
         root  <- getRoot xref
         pages <- getPages xref root
         let initState = NurseryState
                         { nurseryHeader = h
                         , nurseryXRef   = xref
                         , nurseryRoot   = root
                         , nurseryInfo   = info
                         , nurseryPages  = pages
                         }
         rq <- respond Unit
         nurseryLoop dispatch initState rq
  where
    dispatch s RqInfo          = serveInfo s
    dispatch s RqPages         = servePages s
    dispatch s (RqResolve ref) = serveResolve s ref
    dispatch s RqHeader        = serveHeader s

--------------------------------------------------------------------------------
serveInfo :: Monad m => NurseryState -> Nursery m (NResp, NurseryState)
serveInfo s = return (RInfo info, s)
  where
    info = nurseryInfo s

--------------------------------------------------------------------------------
serveHeader :: Monad m => NurseryState -> Nursery m (NResp, NurseryState)
serveHeader s = return (RHeader header, s)
  where
    header = nurseryHeader s

--------------------------------------------------------------------------------
servePages :: Monad m => NurseryState -> Nursery m (NResp, NurseryState)
servePages s = return (RPages pages, s)
  where
    pages = nurseryPages s

--------------------------------------------------------------------------------
serveResolve :: MonadThrow m
             => NurseryState
             -> Reference
             -> Nursery m (NResp, NurseryState)
serveResolve s ref
    = do obj <- resolveObject xref ref
         return (RResolve obj, s)
  where
    xref = nurseryXRef s

--------------------------------------------------------------------------------
getHeader :: MonadThrow m => Nursery m Header
getHeader
    = do bs <- driveGet 8
         case parseOnly parseHeader bs of
             Left e  -> throwM $ NurseryParsingException e
             Right h -> return h

--------------------------------------------------------------------------------
getInfo :: MonadThrow m => XRef -> Nursery m Dictionary
getInfo xref = perform action trailer
  where
    trailer = xrefTrailer xref

    action = dictKey "Info"
             . _Ref
             . act (resolveObject xref)
             . _Dict

--------------------------------------------------------------------------------
getRoot :: MonadThrow m => XRef -> Nursery m Root
getRoot xref
    = do mR <- trailer ^!? action
         case mR of
             Nothing -> throwM NurseryRootNotFound
             Just r  -> return r
  where
    trailer = xrefTrailer xref

    action
        = dictKey "Root"
          . _Ref
          . act (resolveObject xref)
          . _Dict

--------------------------------------------------------------------------------
getPages :: MonadThrow m => XRef -> Root -> Nursery m Pages
getPages xref root
    = do mP <- root ^!? action
         case mP of
             Nothing -> throwM NurseryPagesNotFound
             Just p  -> return p
  where
    action
        = dictKey "Pages"
          . _Ref
          . act (resolveObject xref)
          . _Dict

--------------------------------------------------------------------------------
resolveObject :: MonadThrow m => XRef -> Reference -> Nursery m Object
resolveObject xref ref@(idx,gen)
    = do driveTop
         driveForward
         loop ref
  where
    entries = xrefEntries xref

    loop cRef
        = case M.lookup cRef entries of
              Nothing
                  -> throwM $ NurseryUnresolvedObject idx gen
              Just e
                  -> do let offset = tableEntryOffset e
                        driveSeek offset
                        eR <- parseRepeatedly bufferSize parseIndirectObject
                        case eR of
                            Left e
                                -> throwM $ NurseryParsingException e
                            Right obj
                                -> case obj of
                                       Ref idx gen -> loop (idx,gen)
                                       Dict d      -> couldBeStreamObject d
                                       _           -> return obj
    couldBeStreamObject d
        = do eR <- parseRepeatedly 16 parseStreamHeader
             case eR of
                 Left _
                     -> return $ Dict d
                 Right _
                     -> do p <- driveGetSeek
                           return $ Stream d p

--------------------------------------------------------------------------------
withNursery :: MonadThrow m => Client' NReq NResp m a -> Drive m a
withNursery user = nursery >>~ const user

--------------------------------------------------------------------------------
nurseryLoop :: Monad m
            => (NurseryState -> NReq -> Nursery m (NResp, NurseryState))
            -> NurseryState
            -> NReq
            -> Nursery m r
nurseryLoop k s rq
    = do (r, s') <- k s rq
         rq'     <- respond r
         nurseryLoop k s' rq'

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
nurseryGetInfo :: Monad m => Playground m Dictionary
nurseryGetInfo
    = do RInfo info <- request RqInfo
         return info

--------------------------------------------------------------------------------
nurseryGetHeader :: Monad m => Playground m Header
nurseryGetHeader
    = do RHeader header <- request RqHeader
         return header

--------------------------------------------------------------------------------
nurseryGetPages :: Monad m => Playground m Dictionary
nurseryGetPages
    = do RPages pages <- request RqPages
         return pages

--------------------------------------------------------------------------------
nurseryResolve :: Monad m => Reference -> Playground m Object
nurseryResolve ref
    = do RResolve obj <- request $ RqResolve ref
         return obj
