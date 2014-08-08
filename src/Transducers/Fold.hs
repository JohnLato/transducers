{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Transducers.Fold (
  Fold (..),
  Folding(..),

  foldM,
  mapM_,
) where

import Prelude hiding (mapM_)

data Fold i m a = forall s. Fold s (s -> i -> m s) (s -> a)

class Folding f where
    type Input f
    type FMonad f
    liftFold :: (Input f ~ i, FMonad f ~ m) => Fold i m a -> f a

instance Folding (Fold i m) where
    type Input (Fold i m) = i
    type FMonad (Fold i m) = m
    liftFold = id

foldM
    :: (Folding f, Input f ~ i, FMonad f ~ m, Monad m)
    => (a -> i -> m a)
    -> a
    -> f a
foldM f s0 = liftFold $ Fold s0 f id

mapM_
    :: (Folding f, Input f ~ i, FMonad f ~ m)
    => (i -> m ()) -> f ()
mapM_ f = liftFold $ Fold () (\() i -> f i) id
