{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.XRef
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.XRef
    ( XRef(..)
    , XRefException(..)
    , CObj(..)
    , FObj(..)
    , UObj(..)
    , XRefStream(..)
    , getXRef
    , getXRefPos
    ) where

--------------------------------------------------------------------------------
import           Prelude hiding (take)
import           Data.Bits (shiftL)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict       as M
import           Data.Maybe (fromMaybe)
import           Data.Word
import           Data.Typeable

--------------------------------------------------------------------------------
import           Codec.Compression.Zlib
import           Codec.Compression.Zlib.Internal
import           Control.Lens
import           Control.Monad.Catch (Exception, MonadThrow(..))
import           Control.Monad.State.Strict
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Lazy as PL
import           Data.Attoparsec.ByteString.Char8
import           Pipes.Safe ()

--------------------------------------------------------------------------------
import Data.Rakhana.Internal.Parsers
import Data.Rakhana.Internal.Types
import Data.Rakhana.Tape
import Data.Rakhana.Util.Drive

--------------------------------------------------------------------------------
data Entry
    = FreeObject Integer Int
    | UsedObject Integer Int
    | CompressedObject Integer Int
    deriving Show

--------------------------------------------------------------------------------
data XRef
    = XRef
      { xrefFirstNumber :: !Int
      , xrefObjectCount :: !Int
      , xrefUTable      :: !UTable
      , xrefFTable      :: !FTable
      , xrefCTable      :: !CTable
      , xrefTrailer     :: !Dictionary
      , xrefStream      :: !(Maybe XRefStream)
      }
    deriving Show

--------------------------------------------------------------------------------
data Predictor
    = Png_Up
    | Predictor_Unsupported Integer
    deriving Show

--------------------------------------------------------------------------------
data DecodeParms
    = DecodeParms
      { decodeParmsColumns   :: !Integer
      , decodeParmsPredictor :: !Predictor
      }
    deriving Show

--------------------------------------------------------------------------------
data XRefStream
    = XRefStream
      { xrefStreamLength      :: !Int
      , xrefStreamSize        :: !Integer
      , xrefStreamFirstNumber :: !Integer
      , xrefStreamEntryCount  :: !Integer
      , xrefStreamPrev        :: !(Maybe Integer)
      , xrefStreamW           :: !(Integer, Integer, Integer)
      , xrefStreamEntryWidth  :: !Integer
      , xrefStreamDecodeParms :: !(Maybe DecodeParms)
      , xrefStreamFilter      :: !(Maybe Filter)
      , xrefStreamPos         :: !Integer
      , xrefStreamDict        :: !Dictionary
      }
      deriving Show

--------------------------------------------------------------------------------
data UObj
    = UObj
      { uObjOff :: !Integer
      , uObjGen :: !Int
      }
    deriving Show

--------------------------------------------------------------------------------
data CObj
    = CObj
      { cObjNum :: !Int
      , cObjIdx :: !Integer
      }
    deriving Show

--------------------------------------------------------------------------------
data FObj
    = FObj
      { fObjNxtNum :: !Int
      , fObjGen    :: !Int
      }
    deriving Show

--------------------------------------------------------------------------------
type FTable = M.Map (Int,Int) FObj
type UTable = M.Map (Int,Int) UObj
type CTable = M.Map (Int,Int) CObj

--------------------------------------------------------------------------------
data ObjType
    = Free
    | Used
    | Compressed
    deriving Show

--------------------------------------------------------------------------------
data XRefException
    = XRefParsingException String
    | InvalidXRefStream
    | UnsupportedFilter B.ByteString
    | UnsupportedPredictor Integer
    | ZLibException String String
    deriving (Show, Typeable)

--------------------------------------------------------------------------------
instance Exception XRefException

--------------------------------------------------------------------------------
data ExtractState
    = ExtractState
      { _extractType   :: !ObjType
      , _extractOffset :: !Integer
      , _extractGen    :: !Int
      }

--------------------------------------------------------------------------------
data UnpredictState
    = UnpredictState
      { _unpredictPrev   :: ![Word8]
      , _unpredictId     :: !Int
      , _unpredictUTable :: !UTable
      , _unpredictFTable :: !FTable
      , _unpredictCTable :: !CTable
      }

--------------------------------------------------------------------------------
data NopredictState
    = NopredictState
      { _nopredictId     :: !Int
      , _nopredictUTable :: !UTable
      , _nopredictFTable :: !FTable
      , _nopredictCTable :: !CTable
      }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
makeLenses ''ExtractState
makeLenses ''UnpredictState
makeLenses ''NopredictState

--------------------------------------------------------------------------------
bufferSize :: Int
bufferSize = 4096

--------------------------------------------------------------------------------
getXRefPos :: MonadThrow m => Drive m Integer
getXRefPos
    = do driveBottom
         driveBackward
         skipEOL
         parseEOF
         skipEOL
         p <- parseXRefPosInteger
         skipEOL
         parseStartXRef
         return p

--------------------------------------------------------------------------------
getXRef :: Monad m => Header -> Integer -> Drive m (Either XRefException XRef)
getXRef h pos
    = do rE <- crossRef pos
         case rE of
             Left e
                 | headerMaj h == 1 && headerMin h < 5
                   -> return $ Left e
                 | otherwise
                   -> crossRefStream pos
             _ -> return rE

--------------------------------------------------------------------------------
crossRef :: Monad m => Integer -> Drive m (Either XRefException XRef)
crossRef pos
    = do driveTop
         driveForward
         driveSeek pos
         eR <- parseRepeatedly bufferSize parseXRef
         return $ either (Left . XRefParsingException) Right eR

--------------------------------------------------------------------------------
crossRefStream :: Monad m => Integer -> Drive m (Either XRefException XRef)
crossRefStream offset = loop (offset, Nothing)
  where
    loop (off, newerRefM)
        = do xrefE <- crossRefStreamStep off
             case xrefE of
                 Left e -> return $ Left e
                 Right xref
                     -> let prevM = xrefStream xref >>= xrefStreamPrev
                            upd nRef
                                = let nUTable
                                          = M.union (xrefUTable nRef)
                                            (xrefUTable xref)
                                      nCTable
                                          = M.union (xrefCTable nRef)
                                            (xrefCTable xref) in

                                  nRef { xrefUTable = nUTable
                                       , xrefCTable = nCTable
                                       } in
                        case prevM of
                            Nothing ->
                                let updRef = maybe xref upd newerRefM in
                                return $ Right updRef
                            Just prev
                                -> do xrefE' <- loop (prev, Just xref)
                                      case xrefE' of
                                          Left e'
                                              -> return $ Left e'
                                          Right xref'
                                              -> let updRef = maybe xref' upd
                                                              newerRefM
                                                 in return $ Right updRef

--------------------------------------------------------------------------------
crossRefStreamStep :: Monad m => Integer -> Drive m (Either XRefException XRef)
crossRefStreamStep offset
    = do streamE <- parseXRefStream offset
         let xstreamE = streamE >>= validateXRefStream
         case xstreamE of
             Left e -> return $ Left e
             Right xstream
                 -> do let len  = xrefStreamLength xstream
                           filt = xrefStreamFilter xstream
                           pos  = xrefStreamPos xstream
                       driveSeek pos
                       bs <- driveGetLazy len
                       let dbsE   = decodeBS filt bs
                           dparms = xrefStreamDecodeParms xstream
                           mPred  = fmap decodeParmsPredictor dparms
                           res =
                               case mPred of
                                   Nothing  -> dbsE >>= noPredict xstream
                                   Just prd -> dbsE >>= unpredict prd xstream
                       return res

--------------------------------------------------------------------------------
parseXRefStream :: Monad m => Integer -> Drive m (Either XRefException Stream)
parseXRefStream offset
    = do driveTop
         driveForward
         driveSeek offset
         rE <- driveParseObjectE 128
         case rE of
             Left e  -> return $ Left $ XRefParsingException e
             Right r ->
                 let xstream = r ^. _3
                     expt    = XRefParsingException "Expected a XRef Stream" in
                 return $ maybe (Left $ expt) Right (xstream ^? _Stream)

--------------------------------------------------------------------------------
getFilter :: Dictionary -> Maybe Filter
getFilter dict
    = dict ^? dictKey "Filter" . _Name . to toFilt
  where
    toFilt "FlateDecode" = FlateDecode
    toFilt x             = Filter_Unsupported x

--------------------------------------------------------------------------------
decodeParms :: Dictionary -> Maybe DecodeParms
decodeParms dict
    = do parms <- dict  ^? dictKey "DecodeParms" . _Dict
         col   <- parms ^? dictKey "Columns" . _Number . _Natural
         prd   <- parms ^? dictKey "Predictor" . _Number . _Natural . to toPred
         return DecodeParms
                { decodeParmsColumns   = col
                , decodeParmsPredictor = prd
                }
  where
    toPred 12 = Png_Up
    toPred x  = Predictor_Unsupported x

--------------------------------------------------------------------------------
decompressErrorStr :: DecompressError -> String
decompressErrorStr TruncatedInput     = "TruncatedInput"
decompressErrorStr DictionaryRequired = "DictionaryRequired"
decompressErrorStr DataError          = "DataError"

--------------------------------------------------------------------------------
zlibDecompress :: L.ByteString -> Either XRefException L.ByteString
zlibDecompress bs
    = foldDecompressStream go (Right L.Empty)
      (\code msg -> Left $ ZLibException (decompressErrorStr code) msg) $
      decompressWithErrors zlibFormat defaultDecompressParams bs
  where
    go b aE = fmap (\b' -> L.Chunk b b') aE

--------------------------------------------------------------------------------
decodeBS :: Maybe Filter -> L.ByteString -> Either XRefException L.ByteString
decodeBS (Just filt) bs
    = case filt of
           FlateDecode          -> zlibDecompress bs
           Filter_Unsupported x -> Left $ UnsupportedFilter x
decodeBS _  bs
    = Right bs

--------------------------------------------------------------------------------
unpredict :: Predictor
          -> XRefStream
          -> L.ByteString
          -> Either XRefException XRef
unpredict p xstream input
    = case p of
          Png_Up                  -> unpredictPngUp xstream input
          Predictor_Unsupported x -> Left $ UnsupportedPredictor x

--------------------------------------------------------------------------------
noPredict :: XRefStream -> L.ByteString -> Either XRefException XRef
noPredict xstream input
    = case PL.parse parser input of
          PL.Fail _ _ e -> Left $ XRefParsingException e
          PL.Done _ bs  -> Right bs
  where
    width       = fromIntegral $ xrefStreamEntryWidth xstream
    firstNumber = fromIntegral $ xrefStreamFirstNumber xstream
    ecount      = fromIntegral $ xrefStreamEntryCount xstream
    start       = NopredictState
                  { _nopredictId     = firstNumber - 1
                  , _nopredictUTable = M.empty
                  , _nopredictFTable = M.empty
                  , _nopredictCTable = M.empty
                  }

    parser = evalStateT aState start


    aState = do replicateM_ ecount action
                utable <- use nopredictUTable
                ftable <- use nopredictFTable
                ctable <- use nopredictCTable

                return XRef
                       { xrefFirstNumber = firstNumber
                       , xrefObjectCount = ecount
                       , xrefUTable      = utable
                       , xrefFTable      = ftable
                       , xrefCTable      = ctable
                       , xrefTrailer     = M.empty
                       , xrefStream      = Just xstream
                       }

    action
        = do oid <- nopredictId <+= 1
             row <- lift step

             let (typ, off, gen) = extractTableEntry xstream row
                 ref  = (oid,gen)
                 cref = (oid, 0)
                 fobj = FObj (fromIntegral off) gen
                 uobj = UObj off gen
                 cobj = CObj (fromIntegral off) (fromIntegral gen)

             case typ of
                 Free       -> nopredictFTable.at ref  ?= fobj
                 Used       -> nopredictUTable.at ref  ?= uobj
                 Compressed -> nopredictCTable.at cref ?= cobj

    step = do bs <- take width
              let row = B.unpack bs
              return row

--------------------------------------------------------------------------------
unpredictPngUp :: XRefStream -> L.ByteString -> Either XRefException XRef
unpredictPngUp xstream input
    = case PL.parse parser input of
          PL.Fail _ _ e -> Left $ XRefParsingException e
          PL.Done _ bs  -> Right bs
  where
    width       = fromIntegral $ xrefStreamEntryWidth xstream
    firstNumber = fromIntegral $ xrefStreamFirstNumber xstream
    ecount       = fromIntegral $ xrefStreamEntryCount xstream
    start       = UnpredictState
                  { _unpredictPrev   = replicate width 0
                  , _unpredictId     = firstNumber - 1
                  , _unpredictUTable = M.empty
                  , _unpredictFTable = M.empty
                  , _unpredictCTable = M.empty
                  }

    parser = evalStateT aState start

    aState = do replicateM_ ecount action
                utable <- use unpredictUTable
                ftable <- use unpredictFTable
                ctable <- use unpredictCTable

                return XRef
                       { xrefFirstNumber = firstNumber
                       , xrefObjectCount = ecount
                       , xrefUTable      = utable
                       , xrefFTable      = ftable
                       , xrefCTable      = ctable
                       , xrefTrailer     = M.empty
                       , xrefStream      = Just xstream
                       }
    action
        = do prev    <- use unpredictPrev
             newPrev <- lift $ step prev
             oid     <- unpredictId <+= 1

             unpredictPrev .= newPrev

             let (typ, off, gen) = extractTableEntry xstream newPrev
                 ref  = (oid, gen)
                 cref = (oid, 0)
                 fobj = FObj (fromIntegral off) gen
                 uobj = UObj off gen
                 cobj = CObj (fromIntegral off) (fromIntegral gen)

             case typ of
                 Free       -> unpredictFTable.at ref  ?= fobj
                 Used       -> unpredictUTable.at ref  ?= uobj
                 Compressed -> unpredictCTable.at cref ?= cobj

    step prev
        = do _  <- anyWord8
             bs <- take width
             let newPrev = zipWith (+) (B.unpack bs) prev
             return newPrev

--------------------------------------------------------------------------------
extractTableEntry :: XRefStream -> [Word8] -> (ObjType, Integer, Int)
extractTableEntry xstream arr
    = mkEntry $ execState (traverse_ action $ zip [1..width] arr) start
  where
    start = ExtractState Free 0 0

    mkEntry s
        = let off = s ^. extractOffset
              gen = s ^. extractGen
              typ = s ^. extractType in
          (typ, off, gen)

    action (i,w)
        | i == 1
          = case w of
              0x00 -> extractType .= Free
              0x01 -> extractType .= Used
              0x02 -> extractType .= Compressed
              _    -> error $ "Invalid entry type " ++ show w
        | i <= oLen+1
          = extractOffset += (fromIntegral w) `shiftL` (8*(oLen-i+1))
        | i <= oLen+gLen+1
          = extractGen += (fromIntegral w) `shiftL` (8*(gLen-i+1))
        | otherwise
          = return ()

    (_,c2,c3) = xrefStreamW xstream
    oLen      = fromIntegral c2
    gLen      = fromIntegral c3
    width     = fromIntegral $ xrefStreamEntryWidth xstream

--------------------------------------------------------------------------------
validateXRefStream :: Stream -> Either XRefException XRefStream
validateXRefStream s
    = maybe (Left InvalidXRefStream) Right action
  where
    action
        = do typ          <- dict ^? dictKey "Type" . _Name
             when (typ /= "XRef") Nothing
             size         <- dict ^? dictKey "Size"   . _Number . _Natural
             len          <- dict ^? dictKey "Length" . _Number . _Natural
             w@(c1,c2,c3) <- getW
             let xstream = XRefStream
                           { xrefStreamLength      = fromIntegral len
                           , xrefStreamSize        = size
                           , xrefStreamFirstNumber = fromMaybe 0 firstNumber
                           , xrefStreamEntryCount  = fromMaybe size entryCount
                           , xrefStreamPrev        = getPrev
                           , xrefStreamW           = w
                           , xrefStreamDecodeParms = decodeParms dict
                           , xrefStreamFilter      = getFilter dict
                           , xrefStreamEntryWidth  = c1 + c2 + c3
                           , xrefStreamPos         = s ^. streamPos
                           , xrefStreamDict        = dict
                           }
             return xstream


    dict = s ^. streamDict

    firstNumber
        = do ar <- dict ^? dictKey "Index" . _Array
             ar ^? nth 0 . _Number . _Natural

    entryCount
        = do ar <- dict ^? dictKey "Index" . _Array
             ar ^? nth 1 . _Number . _Natural

    getPrev = dict ^? dictKey "Prev" . _Number . _Natural

    getW = do ar    <- dict ^? dictKey "W" . _Array
              one   <- ar   ^? nth 0 . _Number . _Natural
              two   <- ar   ^? nth 1 . _Number . _Natural
              three <- ar   ^? nth 2 . _Number . _Natural
              return (one, two, three)

--------------------------------------------------------------------------------
skipEOL :: Monad m => Drive m ()
skipEOL
    = do bs <- drivePeek 1
         case B8.uncons bs of
             Just (c, _)
                 | isSpace c -> driveDiscard 1 >> skipEOL
                 | otherwise -> return ()
             _ -> return ()

--------------------------------------------------------------------------------
parseEOF :: MonadThrow m => Drive m ()
parseEOF
    = do bs <- driveGet 5
         case bs of
             "%%EOF" -> return ()
             _       -> throwM $ XRefParsingException "Expected %%EOF"

--------------------------------------------------------------------------------
parseXRefPosInteger :: MonadThrow m => Drive m Integer
parseXRefPosInteger = go []
  where
    go cs = do bs <- drivePeek 1
               case B8.uncons bs of
                   Just (c,_)
                       | isDigit c -> driveDiscard 1 >> go (c:cs)
                       | otherwise -> return $ read cs
                   _ -> return $ read cs

--------------------------------------------------------------------------------
parseStartXRef :: MonadThrow m => Drive m ()
parseStartXRef
    = do bs <- driveGet 9
         case bs of
             "startxref" -> return ()
             _           -> throwM $ XRefParsingException "Expected startxref"

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------
tableXRef :: Parser ()
tableXRef
    = do _ <- string "xref"
         pdfEndOfLine

--------------------------------------------------------------------------------
parseXRef :: Parser XRef
parseXRef
    = do skipSpace
         tableXRef
         (fnum, ecount)   <- parseSubsectionHeader
         (ftable, utable) <- parseTableEntries fnum
         trailer          <- parseTrailerAfterTable
         return XRef
                { xrefFirstNumber = fnum
                , xrefObjectCount = ecount
                , xrefUTable      = utable
                , xrefFTable      = ftable
                , xrefCTable      = M.empty
                , xrefTrailer     = trailer
                , xrefStream      = Nothing
                }

--------------------------------------------------------------------------------
parseSubsectionHeader :: Parser (Int, Int)
parseSubsectionHeader
    = do start <- decimal
         skipSpace
         ecount <- decimal
         pdfEndOfLine
         return (start, ecount)

--------------------------------------------------------------------------------
parseTrailerAfterTable :: Parser Dictionary
parseTrailerAfterTable
    = do skipSpace
         _ <- string "trailer"
         pdfEndOfLine
         skipSpace
         Dict d <- parseDict
         return d

--------------------------------------------------------------------------------
parseTableEntries :: Int -> Parser (FTable, UTable)
parseTableEntries firstNumber
    = loop start
  where
    loop (i, ftable, utable)
        = do mT <- optional parseTableEntry
             case mT of
                 Nothing -> return (ftable, utable)
                 Just (off, gen, used)
                     | used
                       -> let key     = (i, gen)
                              obj     = UObj off gen
                              utable' = M.insert key obj utable in
                          loop (i+1, ftable, utable')
                     | otherwise
                       -> let key     = (i, gen)
                              obj     = FObj (fromIntegral off) gen
                              ftable' = M.insert key obj ftable in
                          loop (i+1, ftable', utable)

    start :: (Int, FTable, UTable)
    start = (firstNumber, M.empty, M.empty)

--------------------------------------------------------------------------------
parseTableEntry :: Parser (Integer, Int, Bool)
parseTableEntry
    = do skipSpace
         offset <- decimal
         skipSpace
         gen <- decimal
         skipSpace
         c <- anyChar
         case c of
             'n' -> return (offset, gen, True)
             'f' -> return (offset, gen, False)
             _   ->
                 let msg = "error parsing XRef table entry: unknown char: " ++
                           [c] in
                 fail msg
