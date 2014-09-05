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
module Transducers.Stream (
  SPEC (..),
  RStream (..),
  RStep (..),

  rmap,
  rfilter,
  rmapM,
  emptyStream,
  ryieldList,
  rfold,
  runStreamF,

  rflatten,
  rflattenList,
  rreplicate,
  runfold,
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
data RStream e m a =
    forall s. RStream s (s -> m (RStep e s a))

data RStep e s a =
    RStep a s
  | RSkip s
  | Die e s
  | RFinal

--------------------------------------------------
-- Pure Stream transformers
-- keep these separate from monadic variants because the types
-- are simpler (and ghc has a better change of making rules match)

{-# INLINE [1] rmap #-}
-- | map between streams
rmap :: Monad m => (a -> b) -> RStream e m a -> RStream e m b
rmap f (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = do
        st <- step s
        case st of
          RStep a s' -> return $ RStep (f a) s'
          RSkip s'   -> return $ RSkip s'
          Die err s' -> return $ Die err s'
          RFinal     -> return RFinal

{-# INLINE [1] rfilter #-}
-- | filter a stream
rfilter :: Monad m => (a -> Bool) -> RStream e m a -> RStream e m a
rfilter p (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = step s >>= \case
      RStep a s' -> return $ if p a then RStep a s' else RSkip s'
      RSkip s'   -> return $ RSkip s'
      Die err s' -> return $ Die err s'
      RFinal     -> return RFinal

--------------------------------------------------
-- Monadic Stream transformers

{-# INLINE [1] rmapM #-}
-- | monadic map between streams
rmapM :: Monad m => (a -> m b) -> RStream e m a -> RStream e m b
rmapM f (RStream s0 step) = RStream s0 go
  where
    {-# INLINE [0] go #-}
    go s = do
        st <- step s
        case st of
          RStep a s' -> f a >>= \o -> return $ RStep o s'
          RSkip s'   -> return $ RSkip s'
          Die err s' -> return $ Die err s'
          RFinal     -> return RFinal

--------------------------------------------------
-- flatteners

{-# INLINE rreplicate #-}
rreplicate :: Monad m => Int -> RStream e m a -> RStream e m a
rreplicate n0 = rflatten (n0,) go
  where
    go (0,_) = RFinal
    go (n,a) = RStep a (n-1,a)

{-# INLINE runfold #-}
runfold :: Monad m => (i -> s) -> (s -> Maybe (o,s))
        -> RStream e m i -> RStream e m o
runfold mkS unf = rflatten mkS step
  where
    step s = case unf s of
        Just (o,s') -> RStep o s'
        Nothing     -> RFinal

{-# INLINE rflattenList #-}
rflattenList :: Monad m => RStream e m [a] -> RStream e m a
rflattenList = rflatten id uncons
  where
    uncons [] = RFinal
    uncons (x:xs) = RStep x xs

-- use this instead of 'Either a (a,b)' to avoid the extra
-- boxing of the tuple
data FlattenState a b =
    Outer a
  | Inner a b

{-# INLINE [1] rflatten #-}
rflatten
    :: Monad m => (a -> s) -> (s -> RStep e s b)
    -> RStream e m a -> RStream e m b
rflatten mkS0 flattenStep (RStream s0 step) = RStream (Outer s0) go
  where
    {-# INLINE [0] go #-}
    go (Outer s) = step s >>= \case
        RStep a outer -> let x = mkS0 a
                         in x `seq` return $ RSkip $ Inner outer x
        RSkip s'   -> return $ RSkip $ Outer s'
        Die   e s' -> return $ Die e $ Outer s'
        RFinal     -> return $ RFinal
    go (Inner outer s) = case flattenStep s of
        RStep b s' -> return $ RStep b (Inner outer s')
        RSkip s'   -> return $ RSkip (Inner outer s')
        Die e s'   -> return $ Die e (Inner outer s')
        RFinal     -> return $ RSkip $ Outer outer

--------------------------------------------------
-- Stream producers

emptyStream :: Monad m => RStream e m a
emptyStream = RStream () (\() -> return RFinal)

-- | ryieldList takes an input stream that is entirely ignored.
-- This is so it can use the same fusion functions as the regular
-- stream transformers
ryieldList :: Monad m => [a] -> RStream e m i -> RStream e m a
ryieldList xs0 _ = RStream xs0 go
  where
    go (x:xs) = return $ RStep x xs
    go []     = return RFinal

-- | transform a Fold into a stream function.  Outputs the fold value at each
-- step, so this makes a scan.
{-# INLINE [1] rfold #-}
rfold
    :: (MonadTrans t, Monad m, Monad (t m))
    => Fold i m a -> RStream e (t m) i -> RStream e (t m) a
rfold (Fold.Fold s0 ff fout) (RStream r0 rstep) = RStream (s0,r0) go
  where
    {-# INLINE [0] go #-}
    go (fstate,rstate) = rstep rstate >>= \case
        RStep a s' -> do
            fstate' <- lift $ ff fstate a
            out <- lift $ fout fstate'
            return $ RStep out (fstate',s')
        RSkip s' -> return $ RSkip (fstate,s')
        Die e s' -> return $ Die e (fstate,s')
        RFinal   -> return RFinal

-- run a stream and return the last value.
{-# INLINE [1] runStreamF #-}
runStreamF
    :: (Monad m, Exception e)
    => o
    -> (forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) i -> RStream e (t m) o)
    -> m o
runStreamF o0 streamf = case streamf emptyStream of
    RStream s0 step -> loop SPEC o0 s0
      where
        loop !sPEC prev s = runIdentityT (step s) >>= \case
          RStep x s' -> loop SPEC x s'
          RSkip s'   -> loop SPEC prev s'
          Die e _    -> throw e
          RFinal     -> return prev
