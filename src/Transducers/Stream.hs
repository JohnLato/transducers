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
data RStream m a =
    forall s. RStream s (s -> m (RStep s a))

data RStep s a = 
    RStep a s
  | RSkip s
  | Die SomeException s
  | RFinal

--------------------------------------------------
-- Pure Stream transformers
-- keep these separate from monadic variants because the types
-- are simpler (and ghc has a better change of making rules match)

{-# INLINE [1] rmap #-}
-- | map between streams
rmap :: Monad m => (a -> b) -> RStream m a -> RStream m b
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
rfilter :: Monad m => (a -> Bool) -> RStream m a -> RStream m a
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
rmapM :: Monad m => (a -> m b) -> RStream m a -> RStream m b
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
-- Stream producers

emptyStream :: Monad m => RStream m a
emptyStream = RStream () (\() -> return RFinal)

-- | ryieldList takes an input stream that is entirely ignored.
-- This is so it can use the same fusion functions as the regular
-- stream transformers
ryieldList :: Monad m => [a] -> RStream m i -> RStream m a
ryieldList xs0 _ = RStream xs0 go
  where
    go (x:xs) = return $ RStep x xs
    go []     = return RFinal

-- | transform a Fold into a stream function.  Outputs the fold value at each
-- step, so this makes a scan.
{-# INLINE [1] rfold #-}
rfold
    :: (MonadTrans t, Monad m, Monad (t m))
    => Fold i m a -> RStream (t m) i -> RStream (t m) a
rfold (Fold.Fold s0 ff fout) (RStream r0 rstep) = RStream (s0,r0) go
  where
    {-# INLINE [0] go #-}
    go (fstate,rstate) = rstep rstate >>= \case
        RStep a s' -> do
            fstate' <- lift $ ff fstate a
            return $ RStep (fout fstate') (fstate',s')
        RSkip s' -> return $ RSkip (fstate,s')
        Die e s' -> return $ Die e (fstate,s')
        RFinal   -> return RFinal

-- run a stream and return the last value.  This function cannot fail because
-- it has an initial value, but it returns a Maybe so the type matches with
-- runTrans
{-# INLINE [1] runStreamF #-}
runStreamF :: Monad m => o -> (forall t. (MonadTrans t, Monad (t m)) => RStream (t m) i -> RStream (t m) o) -> m (Maybe o)
runStreamF o0 streamf = case streamf emptyStream of
    RStream s0 step -> loop SPEC o0 s0
      where
        loop !sPEC prev s = runIdentityT (step s) >>= \case
          RStep x s' -> loop SPEC x s'
          RSkip s'   -> loop SPEC prev s'
          Die e _    -> throw e
          RFinal     -> return $ Just prev
