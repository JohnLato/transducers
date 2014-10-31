{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall #-}
module Transducers.Par (
  parT,
  parF,
) where

import Transducers.Fold
import Transducers.FreeMonad
import Transducers.Fusion.Stream
import Transducers.Transducers
import Control.Concurrent.Async

parF :: Fold i IO a -> Fold i IO a
parF (Fold s0 f mkOut) = Fold (return s0) step (>>= mkOut)
  where
    step s i = do
        result <- s
        s' <- async $ f result i
        return $ wait s'

--TODO: make a parR for streams

parT :: Transducer e i o IO a -> Transducer e i o IO a
parT (Trs tr0) = careful tr0
  where
    drive x = case toView x of
        Impure (TLift m) -> m >>= drive
        _ -> return x
    careful tr = case toView tr of
        Pure a           -> return a
        Impure (Try f)   -> tryAwait >>= careful . f
        Impure (TLift m) -> Trs . fromView . Impure . TLift $ do
            res <- async $ m >>= drive
            return . fromView . Impure . TLift $ wait res
        Impure (Panic e m) -> panic e >> careful m
        Impure (Yield o m) -> yield o >> careful m
{-# NOINLINE [0] parT #-}

{-# RULES
"parT/parF" forall f. parT (tfold f) = tfold (parF f)
"parT/foldOverR" forall f. parT (foldOverR (r_fold f)) = tfold (parF f)
  -- need this because tfold might get replaced before the first rule
  -- fires
    #-}
