--------------------------------------------------------------------------------
-- |
-- Module : Simple
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Simple where

--------------------------------------------------------------------------------
import Data.Foldable (for_)

--------------------------------------------------------------------------------
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except
import Data.Rakhana

--------------------------------------------------------------------------------
playground :: Playground (ExceptT NurseryException IO) ()
playground
    = do header <- nurseryGetHeader
         info   <- nurseryGetInfo
         pages  <- nurseryGetPages
         refs   <- nurseryGetReferences

         liftIO $
             do print header
                print info
                print pages

         for_ refs $ \ref ->
             do obj <- nurseryResolve ref
                liftIO $ print obj

--------------------------------------------------------------------------------
main :: IO ()
main = do rE <- runExceptT $
                    runDrive
                    (fileTape "samples/IdiomLite.pdf")
                    (withNursery playground)

          case rE of
              Left e  -> print e
              Right _ -> return ()
