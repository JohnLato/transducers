{-# LANGUAGE ExistentialQuantification #-}

module Transducers.Fold (
  Fold (..),
  foldM,
  mapM_,
) where

import Prelude hiding (mapM_)

data Fold i m a = forall s. Fold s (s -> i -> m s) (s -> a)

foldM :: Monad m => (a -> i -> m a) -> a -> Fold i m a
foldM f s0 = Fold s0 f id

mapM_ :: (i -> m ()) -> Fold i m ()
mapM_ f = Fold () (\() i -> f i) id
