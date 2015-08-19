{-# LANGUAGE FlexibleContexts   #-}
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
    , Pages
    , Root
    , NReq
    , NResp
    , NurseryException(..)
    , XRefException(..)
    , nurseryGetInfo
    , nurseryGetHeader
    , nurseryGetPages
    , nurseryLoadStreamData
    , nurseryGetReferences
    , nurseryResolve
    , withNursery
    ) where

--------------------------------------------------------------------------------
import           Prelude hiding (take, takeWhile)
import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S

--------------------------------------------------------------------------------
import           Codec.Compression.Zlib (decompress)
import           Control.Lens
import           Control.Lens.Action
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as PL
import           Pipes
import           Pipes.Core

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.Util.Drive
import Data.Rakhana.XRef

--------------------------------------------------------------------------------
data NurseryException
    = NurseryParsingException (Maybe Reference) String
    | NurseryParsingExceptionInObjStm String
    | NurseryUnresolvedObject Int Int
    | NurseryRootNotFound
    | NurseryPagesNotFound
    | NurseryInvalidStreamObject
    | NurseryInvalidLinearizedObject
    | NurseryXRefException XRefException
    | NurseryExpectedStreamObject
    | NurseryInvalidObjStm
    | NurseryUnresolvedObjectInObjStm Int
    | NurseryWrongObject Reference Reference
    | NurseryCyclicDependency Reference
    deriving Show

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
data ObjStm
    = ObjStm
      { objStmCount   :: !Integer
      , objStmFirst   :: !Integer
      , objStmExtends :: !(Maybe Reference)
      , objStmOffset  :: !Integer
      , objStmStream  :: !BL.ByteString
      , objStmReg     :: !(M.Map Integer Integer)
      }
    deriving Show

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
bufferSize = 64

--------------------------------------------------------------------------------
nursery :: MonadError NurseryException m => Nursery m a
nursery
    = do h         <- getHeader
         initState <- regularPDFAccess h
         rq        <- respond Unit
         nurseryLoop dispatch initState rq
  where
    dispatch s RqInfo               = serveInfo s
    dispatch s RqPages              = servePages s
    dispatch s (RqResolve ref)      = serveResolve s ref
    dispatch s RqHeader             = serveHeader s
    dispatch s (RqLoadStreamData t) = serveLoadStream s t
    dispatch s RqReferences         = serveReferences s

--------------------------------------------------------------------------------
regularPDFAccess :: MonadError NurseryException m
                 => Header
                 -> Nursery m NurseryState
regularPDFAccess h
    = do pos <- hoist liftError getXRefPos
         nurseryState h =<< hoist liftError (getXRef h pos)

--------------------------------------------------------------------------------
nurseryState :: MonadError NurseryException m
             => Header
             -> XRef
             -> Nursery m NurseryState
nurseryState h xref
    = do info  <- getInfo xref
         root  <- getRoot xref
         pages <- getPages xref root
         let initState = NurseryState
                         { nurseryHeader = h
                         , nurseryXRef   = xref
                         , nurseryRoot   = root
                         , nurseryInfo   = info
                         , nurseryPages  = pages
                         }
         return initState

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
serveResolve :: MonadError NurseryException m
             => NurseryState
             -> Reference
             -> Nursery m (NResp, NurseryState)
serveResolve s ref
    = do obj <- resolveObject xref ref
         return (RResolve obj, s)
  where
    xref = nurseryXRef s

--------------------------------------------------------------------------------
serveLoadStream :: MonadError NurseryException m
                => NurseryState
                -> Stream
                -> Nursery m (NResp, NurseryState)
serveLoadStream s stream
    = do bs <- loadStream xref stream
         return (RBinaryLazy bs, s)
  where
    xref = nurseryXRef s

--------------------------------------------------------------------------------
loadStream :: MonadError NurseryException m
           => XRef
           -> Stream
           -> Nursery m BL.ByteString
loadStream xref stream
    = do mLen <- dict ^!? dictKey "Length"
                      . act (resolveIfRef xref)
                      . _Number
                      . _Natural
         case mLen of
             Nothing
                 -> throwError NurseryInvalidStreamObject
             Just len
                 -> do driveSeek pos
                       bs <- driveGetLazy $ fromIntegral len
                       let filt = dict ^? dictKey "Filter" . _Name

                       case filt of
                           Nothing -> return bs
                           Just x | "FlateDecode" <- x -> return $ decompress bs
                                  | otherwise          -> return $ bs
  where
    dict = stream ^. streamDict
    pos  = stream ^. streamPos

--------------------------------------------------------------------------------
serveReferences :: Monad m => NurseryState -> Nursery m (NResp, NurseryState)
serveReferences s
    = return (RReferences rs, s)
  where
    rs = M.keys $ xrefUTable $ nurseryXRef s

--------------------------------------------------------------------------------
getHeader :: MonadError NurseryException m => Nursery m Header
getHeader
    = do hE <- driveParse 8 parseHeader
         case hE of
             Left e  -> throwError $ NurseryParsingException Nothing e
             Right h -> return h

--------------------------------------------------------------------------------
getInfo :: MonadError NurseryException m => XRef -> Nursery m Dictionary
getInfo xref = perform action trailer
  where
    trailer = xrefTrailer xref

    action = dictKey "Info"
             . _Ref
             . act (resolveObject xref)
             . _Dict

--------------------------------------------------------------------------------
getRoot :: MonadError NurseryException m => XRef -> Nursery m Root
getRoot xref
    = maybe (trailerRoot xref) (streamRoot xref) xstreamM
  where
    xstreamM = xrefStream xref

--------------------------------------------------------------------------------
trailerRoot :: MonadError NurseryException m => XRef -> Nursery m Root
trailerRoot xref
    = do mR <- trailer ^!? action
         case mR of
             Nothing -> throwError NurseryRootNotFound
             Just r  -> return r
  where
    trailer = xrefTrailer xref

    action
        = dictKey "Root"
          . _Ref
          . act (resolveObject xref)
          . _Dict

--------------------------------------------------------------------------------
streamRoot :: MonadError NurseryException m
           => XRef
           -> XRefStream
           -> Nursery m Root
streamRoot xref xstream
    = do mR <- dict ^!? action
         case mR of
             Nothing -> throwError NurseryRootNotFound
             Just r  -> return r
  where
    dict = xrefStreamDict xstream

    action
        = dictKey "Root"
          . _Ref
          . act (resolveObject xref)
          . _Dict

--------------------------------------------------------------------------------
getPages :: MonadError NurseryException m => XRef -> Root -> Nursery m Pages
getPages xref root
    = do mP <- root ^!? action
         case mP of
             Nothing -> throwError NurseryPagesNotFound
             Just p  -> return p
  where
    action
        = dictKey "Pages"
          . _Ref
          . act (resolveObject xref)
          . _Dict

--------------------------------------------------------------------------------
resolveIfRef :: MonadError NurseryException m
             => XRef
             -> Object
             -> Nursery m Object
resolveIfRef xref (Ref i g) = resolveObject xref (i,g)
resolveIfRef _ obj          = return obj

--------------------------------------------------------------------------------
resolveObject :: MonadError NurseryException m
              => XRef
              -> Reference
              -> Nursery m Object
resolveObject xref ref
    = do driveTop
         driveForward
         loop (S.singleton ref) ref
  where

    entries = xrefUTable xref

    loop visited cRef
        = case M.lookup cRef entries of
              Nothing
                  -> resolveCompressedObject xref ref
              Just e
                  -> do let offset = uObjOff e
                        driveSeek offset
                        r <- parsing cRef

                        let pRef = (r ^. _1, r ^. _2)
                        when (cRef /= pRef) $
                            throwError $ NurseryWrongObject cRef pRef

                        case r ^. _3 of
                            Ref nidx ngen
                                | S.member (nidx,ngen) visited
                                  -> throwError $
                                     NurseryCyclicDependency (nidx,ngen)
                                | otherwise
                                  -> let nRef     = (nidx,ngen)
                                         nVisited = S.insert nRef visited in
                                     loop nVisited nRef
                            obj -> return obj

    parsing cRef
        = driveParseObject bufferSize >>=
              either (throwError . NurseryParsingException (Just cRef)) return

--------------------------------------------------------------------------------
resolveCompressedObject :: MonadError NurseryException m
                        => XRef
                        -> Reference
                        -> Nursery m Object
resolveCompressedObject xref ref@(idx,gen)
    = case M.lookup ref centries of
          Nothing
              -> throwError $ NurseryUnresolvedObject idx gen
          Just cObj
              -> let objsNum = cObjNum cObj
                     sRef    = (objsNum, 0) in
                 do sObj <- resolveObject xref sRef
                    let mPos = sObj ^? _Stream
                    case mPos of
                        Nothing
                            -> throwError NurseryExpectedStreamObject
                        Just stream
                            -> do bs <- loadStream xref stream
                                  o  <- validateObjStm stream bs
                                  lookupObjStm ref o
  where
    centries = xrefCTable xref

--------------------------------------------------------------------------------
validateObjStm :: MonadError NurseryException m
               => Stream
               -> BL.ByteString
               -> m ObjStm
validateObjStm stream bs
    = maybe (throwError NurseryInvalidObjStm) return action
  where
    action
        = do typ   <- dict ^? dictKey "Type" . _Name
             when (typ /= "ObjStm") Nothing
             n     <- dict ^? dictKey "N" . _Number . _Natural
             first <- dict ^? dictKey "First" . _Number . _Natural
             reg   <- registery $ fromIntegral n
             let extends = dict ^? dictKey "Extends" . _Ref

                 info = ObjStm
                        { objStmCount   = n
                        , objStmFirst   = first
                        , objStmExtends = extends
                        , objStmOffset  = pos
                        , objStmStream  = bs
                        , objStmReg     = reg
                        }

             return info

    registery n
        = case PL.parse (parseRegistery n) bs of
              PL.Done _ r -> Just r
              _           -> Nothing

    parseRegistery n
        = execStateT (replicateM_ n step) M.empty

    step = do lift skipSpace
              num <- lift decimal
              _   <- lift space
              off <- lift decimal

              modify' (M.insert num off)

    dict = stream ^. streamDict
    pos  = stream ^. streamPos

--------------------------------------------------------------------------------
lookupObjStm :: MonadError NurseryException m
             => Reference
             -> ObjStm
             -> Nursery m Object
lookupObjStm (idx,_) stm
    = case M.lookup (fromIntegral idx) reg of
          Nothing
              -> throwError $ NurseryUnresolvedObjectInObjStm idx
          Just off
              -> do let skipN   = fromIntegral (first+off)
                        stream' = BL.drop skipN stream

                    case PL.parse parser stream' of
                        PL.Fail _ _ e
                            -> throwError $ NurseryParsingExceptionInObjStm e
                        PL.Done _ obj
                            -> return obj
  where
    parser = parseDict <|> parseArray

    reg    = objStmReg stm
    stream = objStmStream stm
    first  = objStmFirst stm

--------------------------------------------------------------------------------
withNursery :: MonadError NurseryException m
            => Client' NReq NResp m a
            -> Drive m a
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
-- Utilities
--------------------------------------------------------------------------------
liftError :: MonadError NurseryException m => ExceptT XRefException m a -> m a
liftError action
    = runExceptT action >>=
          either (throwError . NurseryXRefException) return

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
