{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Transducers.Fold (
  Fold (..),
  Folding(..),
  feed,

  foldM,
  mapM_,

  initFoldM
) where

import Prelude hiding (mapM_)
import Control.Monad (liftM)

data Fold i m a = forall s. Fold s (s -> i -> m s) (s -> m a)

instance Functor m => Functor (Fold i m) where
    {-# INLINE fmap #-}
    fmap f (Fold s0 step mkOut) = Fold s0 step (fmap f . mkOut)

class Functor f => Folding f where
    type Input f
    type FMonad f :: * -> *
    liftFold :: (Input f ~ i, FMonad f ~ m) => Fold i m a -> f a

instance Functor m => Folding (Fold i m) where
    type Input (Fold i m) = i
    type FMonad (Fold i m) = m
    liftFold = id

-- | TODO: make this part of the Folding class
feed :: Monad m => i -> Fold i m a -> m (Fold i m a)
feed i (Fold s f mkOut) = do
    s' <- f s i
    return (Fold s' f mkOut)

initFoldM :: (Folding f, Input f ~ i, FMonad f ~ m, Monad m)
    => m s -> (s -> i -> m s) -> f s
initFoldM mkS0 f = liftFold $ Fold Nothing f' mkOut
  where
    f'2 s i = Just `liftM` f s i
    f' Nothing i = do
        s0 <- mkS0
        f'2 s0 i
    f' (Just s) i = f'2 s i
    mkOut Nothing = mkS0
    mkOut (Just s) = return s
{-# INLINE initFoldM #-}

foldM
    :: (Folding f, Input f ~ i, FMonad f ~ m, Monad m)
    => (a -> i -> m a)
    -> a
    -> f a
foldM f s0 = liftFold $ Fold s0 f return

mapM_
    :: (Folding f, Input f ~ i, FMonad f ~ m, Monad m)
    => (i -> m ()) -> f ()
mapM_ f = liftFold $ Fold () (\() i -> f i) return
