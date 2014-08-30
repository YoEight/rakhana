{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Tape
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Tape
    ( Drive
    , Direction(..)
    , TReq
    , TResp
    , Tape
    , driveBottom
    , driveBackward
    , driveForward
    , driveGetSeek
    , driveDiscard
    , driveGet
    , driveGetLazy
    , driveModifySeek
    , drivePeek
    , driveSeek
    , driveTop
    , fileTape
    , runDrive
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (void)
import           System.IO

--------------------------------------------------------------------------------
import Pipes
import Pipes.Core

--------------------------------------------------------------------------------
type Tape m a  = Server' TReq TResp m a
type Drive m a = Client' TReq TResp m a

--------------------------------------------------------------------------------
data Direction
    = Forward
    | Backward

--------------------------------------------------------------------------------
data TReq
    = Seek Integer
    | GetSeek
    | Top
    | Bottom
    | Get Int
    | GetLazy Int
    | Direction Direction
    | Peek Int
    | Discard Int

--------------------------------------------------------------------------------
data TResp
    = Unit
    | Binary B.ByteString
    | BinaryLazy BL.ByteString
    | RSeek Integer

--------------------------------------------------------------------------------
data TapeState
    = TapeState
      { tapeStateDirection :: !Direction
      , tapeStatePos       :: !Integer
      , tapeStateFilePath  :: !FilePath
      , tapeStateHandle    :: !Handle
      }

--------------------------------------------------------------------------------
initTapeState :: FilePath -> Handle -> TapeState
initTapeState path h
    = TapeState
      { tapeStateDirection = Forward
      , tapeStatePos       = 0
      , tapeStateFilePath  = path
      , tapeStateHandle    = h
      }

--------------------------------------------------------------------------------
tapeLoop :: Monad m
         => (TapeState -> TReq -> Tape m (TResp, TapeState))
         -> TapeState
         -> TReq
         -> Tape m r
tapeLoop k s rq
    = do (r, s') <- k s rq
         rq'     <- respond r
         tapeLoop k s' rq'

--------------------------------------------------------------------------------
newTapeState :: FilePath -> IO TapeState
newTapeState path
    = fmap (initTapeState path) $ openBinaryFile path ReadMode

--------------------------------------------------------------------------------
fileTape :: MonadIO m => FilePath -> Tape m r
fileTape path
    = do s <- liftIO $ newTapeState path
         r <- respond Unit
         tapeLoop dispatch s r
  where
    dispatch s Top           = tapeTop s
    dispatch s Bottom        = tapeBottom s
    dispatch s (Seek i)      = tapeSeek s i
    dispatch s GetSeek       = tapeGetSeek s
    dispatch s (Get i)       = tapeGet s i
    dispatch s (GetLazy i)   = tapeGetLazy s i
    dispatch s (Direction o) = tapeDirection s o
    dispatch s (Peek i)      = tapePeek s i
    dispatch s (Discard i)   = tapeDiscard s i

--------------------------------------------------------------------------------
tapeTop :: MonadIO m => TapeState -> Tape m (TResp, TapeState)
tapeTop s
    = do liftIO $ hSeek h AbsoluteSeek 0
         return (Unit, s { tapeStatePos = 0 })
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeBottom :: MonadIO m => TapeState -> Tape m (TResp, TapeState)
tapeBottom s
    = do liftIO $ hSeek h SeekFromEnd 0
         return (Unit, s { tapeStatePos = 0 })
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeSeek :: MonadIO m => TapeState -> Integer -> Tape m (TResp, TapeState)
tapeSeek s i
    = do case d of
             Backward -> liftIO $ hSeek h SeekFromEnd i
             Forward  -> liftIO $ hSeek h AbsoluteSeek i
         return (Unit, s { tapeStatePos = i })
  where
    h = tapeStateHandle s
    d = tapeStateDirection s

--------------------------------------------------------------------------------
tapeGetSeek :: MonadIO m => TapeState -> Tape m (TResp, TapeState)
tapeGetSeek s = return (RSeek i, s)
  where
    i = tapeStatePos s

--------------------------------------------------------------------------------
tapeGet :: MonadIO m => TapeState -> Int -> Tape m (TResp, TapeState)
tapeGet s i
    = case o of
          Forward  -> getForward
          Backward -> getBackward
  where
    p = tapeStatePos s
    h = tapeStateHandle s
    o = tapeStateDirection s

    getForward
        = liftIO $
          do let p' = p + (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             b <- B.hGet h i
             return (Binary b, s')

    getBackward
        = liftIO $
          do let p' = p - (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             hSeek h SeekFromEnd $ fromIntegral p'
             b <- B.hGet h i
             return (Binary b, s')

--------------------------------------------------------------------------------
tapeGetLazy :: MonadIO m => TapeState -> Int -> Tape m (TResp, TapeState)
tapeGetLazy s i
    = case o of
          Forward  -> getForward
          Backward -> getBackward
  where
    p = tapeStatePos s
    h = tapeStateHandle s
    o = tapeStateDirection s

    getForward
        = liftIO $
          do let p' = p + (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             b <- BL.hGet h i
             return (BinaryLazy b, s')

    getBackward
        = liftIO $
          do let p' = p - (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             hSeek h SeekFromEnd $ fromIntegral p'
             b <- BL.hGet h i
             return (BinaryLazy b, s')

--------------------------------------------------------------------------------
tapeDirection :: MonadIO m => TapeState -> Direction -> Tape m (TResp, TapeState)
tapeDirection s o
    = return (Unit, s')
  where
    s' = s { tapeStateDirection = o }

--------------------------------------------------------------------------------
tapePeek :: MonadIO m => TapeState -> Int -> Tape m (TResp, TapeState)
tapePeek s i
    = case o of
          Forward  -> peekForward
          Backward -> peekBackward
  where
    p = tapeStatePos s
    h = tapeStateHandle s
    o = tapeStateDirection s

    peekForward
        = liftIO $
          do bs <- B.hGet h i
             hSeek h AbsoluteSeek p
             return (Binary bs, s)

    peekBackward
        = liftIO $
          do let p' = p - (fromIntegral i)
             hSeek h SeekFromEnd p'
             b <- B.hGet h i
             return (Binary b,s)

--------------------------------------------------------------------------------
tapeDiscard :: MonadIO m => TapeState -> Int -> Tape m (TResp, TapeState)
tapeDiscard s i
    = case o of
          Forward  -> discardForward
          Backward -> discardBackward
  where
    p = tapeStatePos s
    h = tapeStateHandle s
    o = tapeStateDirection s

    discardForward
        = liftIO $
          do let p' = p + (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             hSeek h AbsoluteSeek p'
             return (Unit, s')

    discardBackward
        = liftIO $
          do let p' = p - (fromIntegral i)
                 s' = s { tapeStatePos = p' }
             hSeek h SeekFromEnd p'
             return (Unit, s')

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
driveSeek :: Monad m => Integer -> Drive m ()
driveSeek i = void $ request $ Seek i

--------------------------------------------------------------------------------
driveGetSeek :: Monad m => Drive m Integer
driveGetSeek
    = do RSeek i <- request GetSeek
         return i

--------------------------------------------------------------------------------
driveModifySeek :: Monad m => (Integer -> Integer) -> Drive m ()
driveModifySeek k
    = do i <- driveGetSeek
         driveSeek $ k i

--------------------------------------------------------------------------------
driveTop :: Monad m => Drive m ()
driveTop = void $ request Top

--------------------------------------------------------------------------------
driveBottom :: Monad m => Drive m ()
driveBottom = void $ request Bottom

--------------------------------------------------------------------------------
driveGet :: Monad m => Int -> Drive m B.ByteString
driveGet i
    = do Binary b <- request $ Get i
         return b

--------------------------------------------------------------------------------
driveGetLazy :: Monad m => Int -> Drive m BL.ByteString
driveGetLazy i
    = do BinaryLazy b <- request $ GetLazy i
         return b

--------------------------------------------------------------------------------
driveDirection :: Monad m => Direction -> Drive m ()
driveDirection d = void $ request $ Direction d

--------------------------------------------------------------------------------
driveForward :: Monad m => Drive m ()
driveForward = driveDirection Forward

--------------------------------------------------------------------------------
driveBackward :: Monad m => Drive m ()
driveBackward = driveDirection Backward

--------------------------------------------------------------------------------
drivePeek :: Monad m => Int -> Drive m B.ByteString
drivePeek i
    = do Binary b <- request $ Peek i
         return b

--------------------------------------------------------------------------------
driveDiscard :: Monad m => Int -> Drive m ()
driveDiscard i = void $ request $ Discard i

--------------------------------------------------------------------------------
runDrive :: Monad m  => (forall r. Tape m r) -> Drive m a  -> m a
runDrive tape drive = runEffect (tape >>~ const drive)
