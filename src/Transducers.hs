{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall -fno-warn-unused-matches #-}
module Transducers (
  module M,
  main,
) where

import Prelude hiding (mapM)
import Transducers.Fold as Fold
import Transducers.Transducers as M

import Control.Applicative
import Control.Exception (SomeException)
import Control.Monad.Trans

testIt :: (Functor m, MonadIO m) => Transducer e Int String m ()
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

runTest :: (MonadIO m, Functor m) => Transducer SomeException Int () m ()
runTest = yieldList [1,2,3] ><> testIt ><> Fold.mapM_ (liftIO . print)
{-# INLINE runTest #-}

main :: IO ()
main = runTrans runTest >> return ()
