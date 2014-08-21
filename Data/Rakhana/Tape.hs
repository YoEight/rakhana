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
    , driveGet
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
type Tape = forall r. Server' Req Resp IO r
type Drive a = Client' Req Resp IO a

--------------------------------------------------------------------------------
type TapeHandler = TapeState -> Req  -> Server' Req Resp IO (Resp, TapeState)

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
tapeLoop :: TapeHandler -> TapeState -> Req -> Tape
tapeLoop k s rq
    = do (r, s') <- k s rq
         rq'     <- respond r
         tapeLoop k s' rq'

--------------------------------------------------------------------------------
newTapeState :: FilePath -> IO TapeState
newTapeState path
    = fmap (initTapeState path) $ openBinaryFile path ReadMode

--------------------------------------------------------------------------------
fileTape :: FilePath -> Tape
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

--------------------------------------------------------------------------------
tapeTop :: TapeState -> Server' Req Resp IO (Resp, TapeState)
tapeTop s = do liftIO $ hSeek h AbsoluteSeek 0
               return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeBottom :: TapeState -> Server' Req Resp IO (Resp, TapeState)
tapeBottom s = do liftIO $ hSeek h SeekFromEnd 0
                  return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeSeek :: TapeState -> Integer -> Server' Req Resp IO (Resp, TapeState)
tapeSeek s i = do liftIO $ hSeek h AbsoluteSeek i

                  return (Unit, s)
  where
    h = tapeStateHandle s

--------------------------------------------------------------------------------
tapeGet :: TapeState -> Int -> Server' Req Resp IO (Resp, TapeState)
tapeGet s i
    = case o of
          Forward  -> getForward
          Backward -> getBackward
  where
    p = tapeStatePos s
    h = tapeStateHandle s
    o = tapeStateDirection s

    getForward
        = do let p' = p + i
                 s' = s { tapeStatePos = p' }
             b <- liftIO $ L.hGet h i
             return (Binary b, s')

    getBackward
        = do let p' = p - i
                 s' = s { tapeStatePos = p' }
             liftIO $ hSeek h SeekFromEnd $ fromIntegral p'
             b <- liftIO $ L.hGet h i
             return (Binary b, s')

--------------------------------------------------------------------------------
tapeDirection :: TapeState -> Direction -> Server' Req Resp IO (Resp, TapeState)
tapeDirection s o
    = return (Unit, s')
  where
    s' = s { tapeStateDirection = o }

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
driveSeek :: Integer -> Drive ()
driveSeek i = void $ request $ Seek i

--------------------------------------------------------------------------------
driveTop :: Drive ()
driveTop = void $ request Top

--------------------------------------------------------------------------------
driveBottom :: Drive ()
driveBottom = void $ request Bottom

--------------------------------------------------------------------------------
driveGet :: Int -> Drive L.ByteString
driveGet i
    = do Binary b <- request $ Get i
         return b

--------------------------------------------------------------------------------
driveDirection :: Direction -> Drive ()
driveDirection d = void $ request $ Direction d

--------------------------------------------------------------------------------
runDrive :: Tape -> Drive a -> IO a
runDrive tape drive = runEffect (tape >>~ const drive)
