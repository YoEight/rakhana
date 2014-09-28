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

--------------------------------------------------------------------------------
import           Codec.Compression.Zlib
import           Codec.Compression.Zlib.Internal
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.State.Strict
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Lazy as PL
import           Data.Attoparsec.ByteString.Char8

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
    deriving Show

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
data NoUnpredictState
    = NoUnpredictState
      { _noUnpredictId     :: !Int
      , _noUnpredictUTable :: !UTable
      , _noUnpredictFTable :: !FTable
      , _noUnpredictCTable :: !CTable
      }

--------------------------------------------------------------------------------
data EntriesParse
    = EntriesParse
      { _entriesParseId     :: !Int
      , _entriesParseFTable :: !FTable
      , _entriesParseUTable :: !UTable
      }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
makeLenses ''ExtractState
makeLenses ''UnpredictState
makeLenses ''NoUnpredictState
makeLenses ''EntriesParse

--------------------------------------------------------------------------------
bufferSize :: Int
bufferSize = 4096

--------------------------------------------------------------------------------
getXRefPos :: MonadError XRefException m => Drive m Integer
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
getXRef :: MonadError XRefException m => Header -> Integer -> Drive m XRef
getXRef h pos
    = do driveTop
         driveForward

         catchError (normalXRef pos) $ \e ->
             if headerMaj h == 1 && headerMin h < 5
             then throwError e
             else streamXRef pos

--------------------------------------------------------------------------------
normalXRef :: MonadError XRefException m => Integer -> Drive m XRef
normalXRef pos
    = do driveSeek pos
         xref <- parsing
         let dict  = xrefTrailer xref
             mPrev = dict ^? dictKey "Prev" . _Number . _Natural

         case mPrev of
             Nothing
                 -> return xref
             Just prev
                 -> do xrefPrev <- normalXRef prev
                       let nUTable
                               = M.union (xrefUTable xref)
                                 (xrefUTable xrefPrev)

                           newXRef
                               = xref { xrefUTable = nUTable }

                       return newXRef
  where
    parsing
        = driveParse bufferSize parseXRef >>=
              either (throwError . XRefParsingException) return

--------------------------------------------------------------------------------
streamXRef :: MonadError XRefException m => Integer -> Drive m XRef
streamXRef offset = loop (offset, Nothing)
  where
    loop (off, newerRefM)
        = do xref <- streamXRefStep off
             let prevXRefPos = xrefStream xref >>= xrefStreamPrev
                 updateXRef nRef
                     = let nUTable
                               = M.union (xrefUTable nRef)
                                 (xrefUTable xref)
                           nCTable
                               = M.union (xrefCTable nRef)
                                 (xrefCTable xref) in

                       nRef { xrefUTable = nUTable
                            , xrefCTable = nCTable
                            }

             case prevXRefPos of
                 Nothing
                     -> return $ maybe xref updateXRef newerRefM
                 Just prev
                     -> loop (prev, Just xref) >>= \xref' ->
                            return $ maybe xref' updateXRef newerRefM

--------------------------------------------------------------------------------
streamXRefStep :: MonadError XRefException m => Integer -> Drive m XRef
streamXRefStep offset
    = do stream  <- parseXRefStream offset
         xstream <- validateXRefStream stream
         let len    = xrefStreamLength xstream
             filt   = xrefStreamFilter xstream
             pos    = xrefStreamPos xstream
             dparms = xrefStreamDecodeParms xstream
             mPred  = fmap decodeParmsPredictor dparms

         driveSeek pos
         bs  <- driveGetLazy len
         dbs <- decodeByteString filt bs

         case mPred of
             Nothing  -> noUnpredict xstream dbs
             Just prd -> unpredict prd xstream dbs

--------------------------------------------------------------------------------
parseXRefStream :: MonadError XRefException m => Integer -> Drive m Stream
parseXRefStream offset
    = do driveSeek offset
         (_,_,obj) <- parsing

         maybe
             (throwError $ XRefParsingException "Expected a XRef Stream")
             return
             (obj ^? _Stream)
  where
    parsing
        = driveParseObject 128 >>=
              either (throwError . XRefParsingException) return

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
zlibDecompress :: MonadError XRefException m => L.ByteString -> m L.ByteString
zlibDecompress bs
    = foldDecompressStream (liftM . L.Chunk) (return L.Empty)
      (\code -> throwError . ZLibException (decompressErrorStr code))
      (decompressWithErrors zlibFormat defaultDecompressParams bs)

--------------------------------------------------------------------------------
decodeByteString :: MonadError XRefException m
                 => Maybe Filter
                 -> L.ByteString
                 -> m L.ByteString
decodeByteString (Just filt) bs
    = case filt of
           FlateDecode          -> zlibDecompress bs
           Filter_Unsupported x -> throwError $ UnsupportedFilter x
decodeByteString _ bs
    = return bs

--------------------------------------------------------------------------------
unpredict :: MonadError XRefException m
          => Predictor
          -> XRefStream
          -> L.ByteString
          -> m XRef
unpredict p xstream input
    = case p of
          Png_Up                  -> unpredictPngUp xstream input
          Predictor_Unsupported x -> throwError $ UnsupportedPredictor x

--------------------------------------------------------------------------------
noUnpredict :: MonadError XRefException m
            => XRefStream
            -> L.ByteString
            -> m XRef
noUnpredict xstream input
    = case PL.parse parser input of
          PL.Fail _ _ e -> throwError $ XRefParsingException e
          PL.Done _ bs  -> return bs
  where
    width       = fromIntegral $ xrefStreamEntryWidth xstream
    firstNumber = fromIntegral $ xrefStreamFirstNumber xstream
    ecount      = fromIntegral $ xrefStreamEntryCount xstream
    start       = NoUnpredictState
                  { _noUnpredictId     = firstNumber - 1
                  , _noUnpredictUTable = M.empty
                  , _noUnpredictFTable = M.empty
                  , _noUnpredictCTable = M.empty
                  }

    parser = evalStateT aState start


    aState = do replicateM_ ecount action
                utable <- use noUnpredictUTable
                ftable <- use noUnpredictFTable
                ctable <- use noUnpredictCTable

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
        = do oid <- noUnpredictId <+= 1
             row <- lift step
             (typ, off, gen) <- either fail return
                                (extractTableEntry xstream row)

             let ref  = (oid,gen)
                 cref = (oid, 0)
                 fobj = FObj (fromIntegral off) gen
                 uobj = UObj off gen
                 cobj = CObj (fromIntegral off) (fromIntegral gen)

             case typ of
                 Free       -> noUnpredictFTable.at ref  ?= fobj
                 Used       -> noUnpredictUTable.at ref  ?= uobj
                 Compressed -> noUnpredictCTable.at cref ?= cobj

    step = do bs <- take width
              let row = B.unpack bs
              return row

--------------------------------------------------------------------------------
unpredictPngUp :: MonadError XRefException m
               => XRefStream
               -> L.ByteString
               -> m XRef
unpredictPngUp xstream input
    = case PL.parse parser input of
          PL.Fail _ _ e -> throwError $ XRefParsingException e
          PL.Done _ bs  -> return bs
  where
    width       = fromIntegral $ xrefStreamEntryWidth xstream
    firstNumber = fromIntegral $ xrefStreamFirstNumber xstream
    ecount      = fromIntegral $ xrefStreamEntryCount xstream
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
             (typ, off, gen) <- either fail return
                                (extractTableEntry xstream newPrev)

             let ref  = (oid, gen)
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
extractTableEntry :: XRefStream
                  -> [Word8]
                  -> Either String (ObjType, Integer, Int)
extractTableEntry xstream arr
    = fmap mkEntry $ execStateT action start
  where
    action = traverse_ step $ zip [1..width] arr

    start = ExtractState Free 0 0

    mkEntry s
        = let off = s ^. extractOffset
              gen = s ^. extractGen
              typ = s ^. extractType in
          (typ, off, gen)

    step (i,w)
        | i == 1
          = case w of
              0x00 -> extractType .= Free
              0x01 -> extractType .= Used
              0x02 -> extractType .= Compressed
              _    -> lift $ Left $ "Invalid entry type " ++ show w
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
validateXRefStream :: MonadError XRefException m => Stream -> m XRefStream
validateXRefStream s
    = maybe (throwError InvalidXRefStream) return action
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
parseEOF :: MonadError XRefException m => Drive m ()
parseEOF
    = do bs <- driveGet 5
         case bs of
             "%%EOF" -> return ()
             _       -> throwError $ XRefParsingException "Expected %%EOF"

--------------------------------------------------------------------------------
parseXRefPosInteger :: Monad m => Drive m Integer
parseXRefPosInteger = go []
  where
    go cs = do bs <- drivePeek 1
               case B8.uncons bs of
                   Just (c,_)
                       | isDigit c -> driveDiscard 1 >> go (c:cs)
                       | otherwise -> return $ read cs
                   _ -> return $ read cs

--------------------------------------------------------------------------------
parseStartXRef :: MonadError XRefException m => Drive m ()
parseStartXRef
    = do bs <- driveGet 9
         case bs of
             "startxref" -> return ()
             _           -> throwError $
                            XRefParsingException "Expected startxref"

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
         (ftable, utable) <- parseTableEntries fnum ecount
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
parseTableEntries :: Int -> Int -> Parser (FTable, UTable)
parseTableEntries firstNumber n
    = parser
  where
    parser = fmap end $ execStateT action start
    action = replicateM_ n step

    step = do (off, gen, used) <- lift parseTableEntry
              eid              <- entriesParseId <+= 1
              let key = (eid, gen)
              when (off /= 0) $
                  if used
                  then entriesParseUTable.at key ?= UObj off gen
                  else let nxt = fromIntegral off in
                       entriesParseFTable.at key ?= FObj nxt gen

    start = EntriesParse
            { _entriesParseId     = firstNumber - 1
            , _entriesParseFTable = M.empty
            , _entriesParseUTable = M.empty
            }

    end s = (s ^. entriesParseFTable, s ^.  entriesParseUTable)

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
