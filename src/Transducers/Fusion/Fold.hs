{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall #-}
module Transducers.Fusion.Fold (
  f_map,
  f_mapM,
  f_filter,
  f_dropWhileM,
  f_mealyM,
) where

import Prelude hiding (mapM_)
import Transducers.Fold

{-# INLINE [1] f_map #-}
f_map :: Monad m => (i -> o) -> Fold o m a -> Fold i m a
f_map f (Fold s0 fStep mkOut) = Fold s0 f' mkOut
  where
    {-# INLINE [0] f' #-}
    f' s i = fStep s (f i)

{-# INLINE [1] f_mapM #-}
f_mapM :: Monad m => (i -> m o) -> Fold o m a -> Fold i m a
f_mapM f (Fold s0 fStep mkOut) = Fold s0 f' mkOut
  where
    {-# INLINE [0] f' #-}
    f' s i = f i >>= \o -> fStep s o

f_dropWhileM :: Monad m => (i -> m Bool) -> Fold i m a -> Fold i m a
f_dropWhileM p (Fold s0 fStep mkOut) = Fold (True,s0) f' (mkOut.snd)
  where
    {-# INLINE [0] f' #-}
    f' (False,s) i = fStep s i >>= \s' -> return (False,s')
    f' (True,s) i = do
        dropIt <- p i
        if dropIt then return (True,s) else do
            s' <- fStep s i
            return (False,s')


{-# INLINE [1] f_filter #-}
f_filter :: Monad m => (i -> Bool) -> Fold i m a -> Fold i m a
f_filter p (Fold s0 fStep mkOut) = Fold s0 f' mkOut
  where
    {-# INLINE f' #-}
    f' s i = if p i then fStep s i else return s

{-# INLINE [1] f_mealyM #-}
f_mealyM :: (Functor m, Monad m) => s -> (s -> i -> m (o,s)) -> Fold o m a -> Fold i m a
f_mealyM s0 mStep (Fold fs0 fStep mkOut) = Fold (s0,fs0) f' (mkOut.snd)
  where
    {-# INLINE f' #-}
    f' (ms,fs) i = do
        (o,ms') <- mStep ms i
        fs' <- fStep fs o
        return (ms',fs')

{-# INLINE [1] f_zip #-}
f_zip :: (Functor m, Monad m) => Fold o m a -> Fold o m b -> Fold o m (a,b)
f_zip (Fold l0 lStep lOut) (Fold r0 rStep rOut) = Fold (l0,r0) go mkOut
  where
    go (l0,r0) i = do
        l1 <- lStep l0 i
        r1 <- rStep r0 i
        return (l1,r1)
