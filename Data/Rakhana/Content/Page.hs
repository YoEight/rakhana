{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Rakhana.Content.Page
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Rakhana.Content.Page
    ( Content(..)
    , Page
    , documentPageCount
    , lookupPage
    , pageContent
    , pageDictionary
    ) where

--------------------------------------------------------------------------------
import Prelude hiding (sequence)
import Control.Applicative
import Data.Maybe
import Data.Traversable (sequence)

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Vector as V

--------------------------------------------------------------------------------
import Data.Rakhana.Content.Expr
import Data.Rakhana.Content.Operator
import Data.Rakhana.Internal.Types
import Data.Rakhana.Nursery

--------------------------------------------------------------------------------
newtype Page = Page Dictionary deriving Show

--------------------------------------------------------------------------------
data Content
    = ContentStream Stream
    | ContentArray (V.Vector Stream)
    deriving Show

--------------------------------------------------------------------------------
pageDictionary :: Page -> Dictionary
pageDictionary (Page d) = d

--------------------------------------------------------------------------------
documentPageCount :: Monad m => Playground m Integer
documentPageCount
    = do pages <- nurseryGetPages
         let pcount = pages ^? dictKey "Count" . _Number . _Natural

         return $ fromMaybe 0 pcount

--------------------------------------------------------------------------------
pageContent :: Monad m => Page -> Playground m (Maybe Content)
pageContent (Page d)
    = do mRes <- d ^!? dictKey "Contents" . _Ref . act nurseryResolve
         case mRes of
             Just obj
                 -> case obj of
                        AStream s -> return $ Just $ ContentStream s
                        Array v   -> fmap (fmap ContentArray) $ vectorStreams v
                        _         -> return Nothing
             _   -> return Nothing

--------------------------------------------------------------------------------
lookupPage :: Monad m => Int -> Playground m (Maybe Page)
lookupPage pageNum
    = do count <- documentPageCount
         pages <- nurseryGetPages

         if pageNum < 1 || pageNum_integer > count
             then return Nothing
             else fmap (fmap Page) $ lookupPage_ pageNum 0 pages
  where
    pageNum_integer = fromIntegral pageNum

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
findKid :: Monad m
        => Int
        -> Integer
        -> V.Vector Object
        -> Playground m (Maybe (Dictionary, Integer))
findKid pageNum threshold v
    = loop threshold 0
  where
    len             = V.length v
    pageNum_integer = fromIntegral pageNum

    loop curThreshold idx
        | idx == len
          = return Nothing
        | otherwise
          = v ^!? nth idx . _Ref . act nurseryResolve . _Dict >>= \mPage ->
                case mPage of
                    Just page
                        -> let mCount = page ^? dictKey "Count"
                                             . _Number
                                             . _Natural in
                           case mCount of
                               Just count
                                   | pageNum_integer > curThreshold &&
                                     pageNum_integer <= curThreshold + count
                                     -> return $ Just (page, curThreshold)
                                   | otherwise
                                     -> loop (curThreshold + count) (idx + 1)
                               _ -> return Nothing
                    _ -> return Nothing

--------------------------------------------------------------------------------
getKidsAndCount :: Dictionary -> Maybe (V.Vector Object, Integer)
getKidsAndCount dict
    = (,) <$> (dict ^? dictKey "Kids" . _Array)
          <*> (dict ^? dictKey "Count" . _Number . _Natural)

--------------------------------------------------------------------------------
lookupPage_ :: Monad m
            => Int
            -> Integer
            -> Dictionary
            -> Playground m (Maybe Dictionary)
lookupPage_ pageNum threshold node
    = case getKidsAndCount node of
          Just (kids, count)
              | V.length kids == fromIntegral count
                -> let idx = pageNum - 1 - (fromIntegral threshold)
                   in kids ^!? nth idx . _Ref . act nurseryResolve . _Dict
              | otherwise
                -> findKid pageNum threshold kids >>= \res ->
                       case res of
                           Just (kid, newThreshold)
                               -> lookupPage_ pageNum newThreshold kid
                           _   -> return Nothing
          _ -> return Nothing

--------------------------------------------------------------------------------
vectorStreams :: Monad m
              => V.Vector Object
              -> Playground m (Maybe (V.Vector Stream))
vectorStreams v
    = fmap sequence $ traverse go v
  where
    go obj
        = do mRes <- obj ^!? _Ref . act nurseryResolve
             case mRes of
                 Just res
                     -> case res of
                            AStream s -> return $ Just s
                            _         -> return Nothing
                 _   -> return Nothing
