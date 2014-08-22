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
    , Req
    , Resp
    , Tape
    , driveBottom
    , driveDirection
    , driveDiscard
    , driveGet
    , drivePeek
    , driveSeek
    , driveTop
    , fileTape
    , runDrive
    ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as L
import           Data.Functor (void)
import           System.IO

--------------------------------------------------------------------------------
import Pipes
import Pipes.Core

--------------------------------------------------------------------------------
type Tape m a  = Server' Req Resp m a
type Drive m a = Client' Req Resp m a

--------------------------------------------------------------------------------
data Direction
    = Forward
    | Backward

--------------------------------------------------------------------------------
data Req
    = Seek Integer
    | Top
    | Bottom
    | Get Int
    | Direction Direction
    | Peek Int
    | Discard Int

--------------------------------------------------------------------------------
data Resp
    = Unit
    | Binary L.ByteString

--------------------------------------------------------------------------------
data TapeState
    = TapeState
      { tapeStateDirection :: !Direction
      , tapeStatePos       :: !Int
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
         => (TapeState -> Req -> Tape m (Resp, TapeState))
         -> TapeState
         -> Req
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
    dispatch s (Get i)       = tapeGet s i
    dispatch s (Direction o) = tapeDirection s o
    dispatch s (Peek i)      = tapePeek s i
    dispatch s (Discard i)   = tapeDiscard s i

--------------------------------------------------------------------------------
tapeTop :: MonadIO m => TapeState -> Tape m (Resp, TapeState)
tapeTop s
    = do liftIO $ hSeek h AbsoluteSeek 0
         return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeBottom :: MonadIO m => TapeState -> Tape m (Resp, TapeState)
tapeBottom s
    = do liftIO $ hSeek h SeekFromEnd 0
         return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeSeek :: MonadIO m => TapeState -> Integer -> Tape m (Resp, TapeState)
tapeSeek s i
    = do liftIO $ hSeek h AbsoluteSeek i
         return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeGet :: MonadIO m => TapeState -> Int -> Tape m (Resp, TapeState)
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
          do let p' = p + i
                 s' = s { tapeStatePos = p' }
             b <- L.hGet h i
             return (Binary b, s')

    getBackward
        = liftIO $
          do let p' = p - i
                 s' = s { tapeStatePos = p' }
             hSeek h SeekFromEnd $ fromIntegral p'
             b <- L.hGet h i
             return (Binary b, s')

--------------------------------------------------------------------------------
tapeDirection :: MonadIO m => TapeState -> Direction -> Tape m (Resp, TapeState)
tapeDirection s o
    = return (Unit, s')
  where
    s' = s { tapeStateDirection = o }

--------------------------------------------------------------------------------
tapePeek :: MonadIO m => TapeState -> Int -> Tape m (Resp, TapeState)
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
          do bs <- L.hGet h i
             hSeek h AbsoluteSeek $ fromIntegral p
             return (Binary bs, s)

    peekBackward
        = liftIO $
          do let p' = p - i
             hSeek h SeekFromEnd $ fromIntegral p'
             b <- L.hGet h i
             return (Binary b,s)

--------------------------------------------------------------------------------
tapeDiscard :: MonadIO m => TapeState -> Int -> Tape m (Resp, TapeState)
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
          do let p' = p + i
                 s' = s { tapeStatePos = p' }
             hSeek h AbsoluteSeek $ fromIntegral p'
             return (Unit, s')

    discardBackward
        = liftIO $
          do let p' = p - i
                 s' = s { tapeStatePos = p' }
             hSeek h SeekFromEnd $ fromIntegral p'
             return (Unit, s')

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
driveSeek :: Monad m => Integer -> Drive m ()
driveSeek i = void $ request $ Seek i

--------------------------------------------------------------------------------
driveTop :: Monad m => Drive m ()
driveTop = void $ request Top

--------------------------------------------------------------------------------
driveBottom :: Monad m => Drive m ()
driveBottom = void $ request Bottom

--------------------------------------------------------------------------------
driveGet :: Monad m => Int -> Drive m L.ByteString
driveGet i
    = do Binary b <- request $ Get i
         return b

--------------------------------------------------------------------------------
driveDirection :: Monad m => Direction -> Drive m ()
driveDirection d = void $ request $ Direction d

--------------------------------------------------------------------------------
drivePeek :: Monad m => Int -> Drive m L.ByteString
drivePeek i
    = do Binary b <- request $ Peek i
         return b

--------------------------------------------------------------------------------
driveDiscard :: Monad m => Int -> Drive m ()
driveDiscard i = void $ request $ Discard i

--------------------------------------------------------------------------------
runDrive :: Monad m
         => (forall r. Tape m r)
         -> Drive m a
         -> m a
runDrive tape drive = runEffect (tape >>~ const drive)
