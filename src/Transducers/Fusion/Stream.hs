{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE  ViewPatterns #-}

{-# OPTIONS -Wall -fno-warn-unused-matches #-}
module Transducers.Fusion.Stream (
  SPEC (..),
  RStream (..),
  RStep (..),

  r_map,
  r_filter,
  r_mapM,
  r_mealyM,
  r_dropWhileM,
  emptyStream,
  r_yieldList,
  r_fold,
  r_scan,
  runStreamF,

  r_flatten,
  r_flattenList,
  r_flattenMaybe,
  r_replicate,
  r_unfold,
) where

import Transducers.Fold (Fold)
import qualified Transducers.Fold as Fold

import Control.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Identity (runIdentityT)

import GHC.Exts

--------------------------------------------------
-- helpers and regular stream

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

--------------------------------------------------
-- Stream with support for resumable exceptions.
-- It seems that we don't actually need a regular stream,
-- GHC can manage the extra constructor just fine.

-- | Resumable stream
data RStream e m o a =
    forall s. RStream s (s -> m (RStep e s o a))

data RStep e s o a =
    RStep a s
  | RSkip s
  | Die e s
  | RFinal o

--------------------------------------------------
-- Pure Stream transformers
-- keep these separate from monadic variants because the types
-- are simpler (and ghc has a better change of making rules match)

{-# INLINE [1] r_map #-}
-- | map between streams
r_map :: Monad m => (a -> b) -> RStream e m o a -> RStream e m o b
r_map f (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = do
        st <- step s
        case st of
          RStep a s' -> return $ RStep (f a) s'
          RSkip s'   -> return $ RSkip s'
          Die err s' -> return $ Die err s'
          RFinal o   -> return $ RFinal o

{-# INLINE [1] r_filter #-}
-- | filter a stream
r_filter :: Monad m => (a -> Bool) -> RStream e m o a -> RStream e m o a
r_filter p (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = step s >>= \case
      RStep a s' -> return $ if p a then RStep a s' else RSkip s'
      RSkip s'   -> return $ RSkip s'
      Die err s' -> return $ Die err s'
      RFinal o   -> return $ RFinal o

--------------------------------------------------
-- Monadic Stream transformers

{-# INLINE [1] r_mapM #-}
-- | monadic map between streams
r_mapM :: Monad m => (a -> m b) -> RStream e m o a -> RStream e m o b
r_mapM f (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = do
        st <- step s
        case st of
          RStep a s' -> f a >>= \o -> return $ RStep o s'
          RSkip s'   -> return $ RSkip s'
          Die err s' -> return $ Die err s'
          RFinal o   -> return $ RFinal o

{-# INLINE [0] r_mealyM #-}
r_mealyM :: Monad m => s -> (s -> i -> m (a,s)) -> RStream e m o i -> RStream e m o a
r_mealyM s0 f (RStream rs0 step) = RStream (s0,rs0) go
  where
    {-# INLINE [0] go #-}
    go (s,rs) = do
        st <- step rs
        case st of
            RStep i rs' -> f s i >>= \(o,s') -> return $ RStep o (s',rs')
            RSkip rs'   -> return $ RSkip (s,rs')
            Die err rs' -> return $ Die err (s,rs')
            RFinal o    -> return $ RFinal o

{-# INLINE [1] r_dropWhileM #-}
r_dropWhileM :: Monad m => (i -> m Bool) -> RStream e m o i -> RStream e m o i
r_dropWhileM p (RStream rs0 step) = RStream (True,rs0) go
  where
    {-# INLINE [0] go #-}
    go (check,rs) = do
        st <- step rs
        case st of
            RStep i rs'
                | check == False -> return $ RStep i (check,rs')
                | otherwise -> do
                    dropIt <- p i
                    return $ if dropIt then RSkip (dropIt,rs') else RStep i (dropIt,rs')
            RSkip rs'   -> return $ RSkip (check,rs')
            Die err rs' -> return $ Die err (check,rs')
            RFinal o    -> return $ RFinal o

--------------------------------------------------
-- flatteners

{-# INLINE r_replicate #-}
r_replicate :: Monad m => Int -> RStream e m o a -> RStream e m o a
r_replicate n0 = r_flatten (n0,) go
  where
    go (0,_) = RFinal ()
    go (n,a) = RStep a (n-1,a)

{-# INLINE r_unfold #-}
r_unfold :: Monad m => (i -> s) -> (s -> Maybe (a,s))
        -> RStream e m o i -> RStream e m o a
r_unfold mkS unf = r_flatten mkS step
  where
    step s = case unf s of
        Just (o,s') -> RStep o s'
        Nothing     -> RFinal ()

{-# INLINE r_flattenList #-}
r_flattenList :: Monad m => RStream e m o [a] -> RStream e m o a
r_flattenList = r_flatten id uncons
  where
    uncons [] = RFinal ()
    uncons (x:xs) = RStep x xs

{-# INLINE r_flattenMaybe #-}
r_flattenMaybe :: Monad m => RStream e m o (Maybe a) -> RStream e m o a
r_flattenMaybe = r_flatten id uncons
  where
    uncons (Just x) = RStep x Nothing
    uncons Nothing = RFinal ()


-- use this instead of 'Either a (a,b)' to avoid the extra
-- boxing of the tuple
data FlattenState a b =
    Outer a
  | Inner a b

{-# INLINE [1] r_flatten #-}
r_flatten
    :: Monad m => (a -> s) -> (s -> RStep e s () b)
    -> RStream e m o a -> RStream e m o b
r_flatten mkS0 flattenStep (RStream s0 step) = RStream (Outer s0) go
  where
    {-# INLINE [0] go #-}
    go (Outer s) = step s >>= \case
        RStep a outer -> let x = mkS0 a
                         in x `seq` return $ RSkip $ Inner outer x
        RSkip s'   -> return $ RSkip $ Outer s'
        Die   e s' -> return $ Die e $ Outer s'
        RFinal o   -> return $ RFinal o
    go (Inner outer s) = case flattenStep s of
        RStep b s' -> return $ RStep b (Inner outer s')
        RSkip s'   -> return $ RSkip (Inner outer s')
        Die e s'   -> return $ Die e (Inner outer s')
        RFinal _   -> return $ RSkip $ Outer outer

--------------------------------------------------
-- Stream producers

emptyStream :: Monad m => RStream e m () a
emptyStream = RStream () (\() -> return $ RFinal ())

-- | r_yieldList takes an input stream that is entirely ignored.
-- This is so it can use the same fusion functions as the regular
-- stream transformers
r_yieldList :: Monad m => [a] -> RStream e m z i -> RStream e m () a
r_yieldList xs0 _ = RStream xs0 go
  where
    go (x:xs) = return $ RStep x xs
    go []     = return $ RFinal ()

-- | transform a Fold into a stream scan.
{-# INLINE [1] r_scan #-}
r_scan
    :: (MonadTrans t, Monad m, Monad (t m))
    => Fold i m a -> RStream e (t m) b i -> RStream e (t m) b a
r_scan (Fold.Fold s0 ff fout) (RStream r0 rstep) = RStream (s0,r0) go
  where
    {-# INLINE [0] go #-}
    go (fstate,rstate) = rstep rstate >>= \case
        RStep a s' -> do
            fstate' <- lift $ ff fstate a
            out <- lift $ fout fstate'
            return $ RStep out (fstate',s')
        RSkip s' -> return $ RSkip (fstate,s')
        Die e s' -> return $ Die e (fstate,s')
        RFinal b -> return $ RFinal b


-- | transform a Fold into a stream function.
{-# INLINE [1] r_fold #-}
r_fold
    :: (MonadTrans t, Monad m, Monad (t m))
    => Fold i m a -> RStream e (t m) b i -> RStream e (t m) a i
r_fold (Fold.Fold s0 ff fout) (RStream r0 rstep) = RStream (s0,r0) go
  where
    {-# INLINE [0] go #-}
    go (fstate,rstate) = rstep rstate >>= \case
        RStep a s' -> do
            fstate' <- lift $ ff fstate a
            return $ RStep a (fstate',s')
        RSkip s' -> return $ RSkip (fstate,s')
        Die e s' -> return $ Die e (fstate,s')
        RFinal _ -> do
            outval <- lift $ fout fstate
            return $ RFinal outval

-- run a stream and return the output value
{-# INLINE [1] runStreamF #-}
runStreamF
    :: (Monad m, Exception e)
    => (forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () i -> RStream e (t m) o b)
    -> m o
runStreamF streamf = case streamf emptyStream of
    RStream s0 step -> loop SPEC s0
      where
        loop !sPEC s = runIdentityT (step s) >>= \case
          RStep x s' -> loop SPEC s'
          RSkip s'   -> loop SPEC s'
          Die e _    -> throw e
          RFinal o   -> return o
