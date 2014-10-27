{-# LANGUAGE FlexibleContexts   #-}
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
    , contentExtractText
    ) where

--------------------------------------------------------------------------------
import           Prelude hiding (sequence)
import           Control.Applicative
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.Foldable (for_)
import           Data.Maybe
import           Data.Monoid ((<>), mempty)
import           Data.Traversable (sequence)

--------------------------------------------------------------------------------
import           Control.Monad.State hiding (sequence)
import           Control.Lens
import qualified Data.Vector as V
import           Pipes
import           Pipes.Prelude (toListM)

--------------------------------------------------------------------------------
import Data.Rakhana.Content.Expr
import Data.Rakhana.Content.Operator
import Data.Rakhana.Content.Parsers
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
contentExtractText :: Monad m => Content -> Playground m L.ByteString
contentExtractText (ContentStream s)
    = do bytes <- nurseryLoadStreamData s
         let effect = exprs bytes >-> operators >-> textExtractor
             action = runEffect effect
         res <- execStateT action mempty
         return $ toLazyByteString res
contentExtractText _
    = return ""

--------------------------------------------------------------------------------
operators :: Monad m => Pipe Expr Operator m r
operators = go []
  where
    go stack
        = do expr <- await
             case expr of
                 Obj obj
                     -> go (obj:stack)
                 Op op
                     -> let oper = Operator
                                   { operatorOp       = op
                                   , operatorOperands = reverse stack
                                   } in
                        yield oper >> go []

--------------------------------------------------------------------------------
textExtractor :: MonadState Builder m => Consumer' Operator m ()
textExtractor = for cat go
  where
    go op
        = case operatorOp op of
              Op_Tj         -> tjGetString $ operatorOperands op
              Op_apostrophe -> apoGetString $ operatorOperands op
              Op_quote      -> quoteGetString $ operatorOperands op
              Op_TJ         -> tJGetString $ operatorOperands op
              _             -> return ()

    tjGetString [Bytes bs]
        = modify $ \b -> b <> byteString bs
    tjGetString _
        = return ()

    apoGetString ops@[Bytes bs]
        = modify (\b -> b <> char8 '\n') >> tjGetString ops
    apoGetString _
        = return ()

    quoteGetString [_,_, bytes@(Bytes bs)]
        = apoGetString [bytes]
    quoteGetString _
        = return ()

    tJGetString [Array objs]
        = for_ objs $ \obj ->
              case obj of
                  bs@(Bytes _) -> tjGetString [bs]
                  _            -> return ()
    tJGetString _
        = return ()

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
