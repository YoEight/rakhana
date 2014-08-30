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
    , NReq
    , NResp
    , nurseryGetInfo
    , nurseryGetHeader
    , nurseryGetPages
    , nurseryLoadStreamData
    , nurseryGetReferences
    , nurseryResolve
    , withNursery
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as M
import           Data.Typeable hiding (Proxy)

--------------------------------------------------------------------------------
import Codec.Compression.Zlib (decompress)
import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Pipes
import Pipes.Core

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.Util.Drive
import Data.Rakhana.XRef

--------------------------------------------------------------------------------
data NurseryException
    = NurseryParsingException String
    | NurseryUnresolvedObject Int Int
    | NurseryRootNotFound
    | NurseryPagesNotFound
    | NurseryInvalidStreamObject
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
type Nursery m a = Proxy TReq TResp NReq NResp m a
type Playground m a = Client' NReq NResp m a
type Root = Dictionary
type Pages = Dictionary

--------------------------------------------------------------------------------
data NReq
    = RqInfo
    | RqHeader
    | RqPages
    | RqResolve Reference
    | RqLoadStreamData Stream
    | RqReferences

--------------------------------------------------------------------------------
data NResp
    = Unit
    | RBinaryLazy BL.ByteString
    | RInfo Dictionary
    | RHeader Header
    | RPages Dictionary
    | RResolve Object
    | RReferences [Reference]

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
    dispatch s RqInfo               = serveInfo s
    dispatch s RqPages              = servePages s
    dispatch s (RqResolve ref)      = serveResolve s ref
    dispatch s RqHeader             = serveHeader s
    dispatch s (RqLoadStreamData t) = serveLoadStream s t
    dispatch s RqReferences         = serveReferences s

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
serveLoadStream :: MonadThrow m
                => NurseryState
                -> Stream
                -> Nursery m (NResp, NurseryState)
serveLoadStream s st
    = do mLen <- dict ^!? dictKey "Length"
                      . act (resolveIfRef xref)
                      . _Number
                      . _Natural
         case mLen of
             Nothing
                 -> throwM NurseryInvalidStreamObject
             Just len
                 -> do driveSeek pos
                       bs <- driveGetLazy $ fromIntegral len
                       let filt = dict ^? dictKey "Filter" . _Name
                           bs'  = case filt of
                                      Nothing -> bs
                                      Just x | "FlateDecode" <- x
                                               -> decompress bs
                                             | otherwise -> bs
                       return (RBinaryLazy bs', s)
  where
    dict = st ^. streamDict
    pos  = st ^. streamPos
    xref = nurseryXRef s

--------------------------------------------------------------------------------
serveReferences :: Monad m => NurseryState -> Nursery m (NResp, NurseryState)
serveReferences s
    = return (RReferences $ rs, s)
  where
    rs = M.keys $ xrefEntries $ nurseryXRef s

--------------------------------------------------------------------------------
getHeader :: MonadThrow m => Nursery m Header
getHeader = driveParse 8 parseHeader

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
resolveIfRef :: MonadThrow m => XRef -> Object -> Nursery m Object
resolveIfRef xref (Ref i g) = resolveObject xref (i,g)
resolveIfRef _ obj          = return obj

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
                        r <- driveParseObject bufferSize
                        case r ^. _3 of
                            Ref nidx ngen -> loop (nidx,ngen)
                            _             -> return $ r ^. _3

--------------------------------------------------------------------------------
withNursery :: MonadThrow m => Playground m a -> Drive m a
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

--------------------------------------------------------------------------------
nurseryLoadStreamData :: Monad m => Stream -> Playground m BL.ByteString
nurseryLoadStreamData s
    = do RBinaryLazy bs <- request $ RqLoadStreamData s
         return bs

--------------------------------------------------------------------------------
nurseryGetReferences :: Monad m => Playground m [Reference]
nurseryGetReferences
    = do RReferences rs <- request $ RqReferences
         return rs
