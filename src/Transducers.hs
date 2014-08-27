{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall -fno-warn-unused-matches #-}
module Transducers (
  module M,
  module Fold,
  main,
) where

import Prelude hiding (mapM)
import qualified Transducers.Fold as Fold
import Transducers.Transducers as M

import Control.Applicative
import Control.Monad.Trans

testIt :: (Functor m, MonadIO m) => Transducer Int String m ()
testIt =
    tfilter (\x -> mod x 2 == 0)
    ><> tmap (fromIntegral :: Int -> Double)
    ><> treplicate 3
    ><> tmap (show :: Double -> String)
    ><> mapM (\x -> x <$ liftIO (print (length x)))
    ><> unfold Just (fmap (,Nothing))
          -- identity unfold, see if we can
          -- remove it.  Yes we can!
{-# INLINE testIt #-}

runTest :: (MonadIO m, Functor m) => Transducer () () m ()
runTest = yieldList [1,2,3] ><> testIt ><> Fold.mapM_ (liftIO . print)
{-# INLINE runTest #-}

main :: IO ()
main = runTrans runTest >> return ()
