{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wall -fno-warn-unused-matches #-}
module Transducers.Transducers (
  TransducerF (..),
  Transducer(..),

  -- * low-level API
  yield,
  await,
  tryAwait,
  panic,
  (><>),
  (<><),
  
  -- * main API
  tmap,
  tfilter,
  mapM,
  tfold,

  yieldList,
  runTrans,

) where

import Prelude hiding (mapM)
import Transducers.Fold (Fold)
import qualified Transducers.Fold as Fold
import Transducers.FreeMonad
import Transducers.Stream

import Control.Applicative
import Control.Exception
import Control.Monad ((>=>), forever, liftM, when)
import Control.Monad.Trans

--------------------------------------------------
-- types

-- Functor to represent the main Transducer type
data TransducerF i o m a =
    Await (i -> a)
  | Try (Maybe i -> a)
  | Yield o a
  | Panic SomeException a
  | TLift (m a)  -- is this ok?  Maybe it should be moved to FreeMonad?
  deriving Functor

newtype Transducer i o m a =
    Trs { unTRS :: FreeMonad (TransducerF i o m) a}
    deriving (Functor, Monad)

instance MonadTrans (Transducer i o) where
    lift m = Trs $ fromView (Impure (TLift $ liftM return m))

instance (Functor m, Monad m) => Fold.Folding (Transducer i o m) where
    type Input (Transducer i o m) = i
    type FMonad (Transducer i o m) = m
    {-# INLINE liftFold #-}
    liftFold = tfold

--------------------------------------------------
-- primitive API

yield :: o -> Transducer i o m ()
yield x = Trs . fromView $ Impure $ Yield x (return ())

await :: Transducer i o m i
await = Trs . fromView $ Impure (Await return)

tryAwait :: Transducer i o m (Maybe i)
tryAwait = Trs . fromView $ Impure (Try return)

panic :: (Exception e) => e -> Transducer i o m ()
panic e = Trs . fromView . Impure $ Panic (toException e) (return ())

--------------------------------------------------
-- composition

infixr 9 ><>, <><

(<><) :: (Functor m) => Transducer b c m y -> Transducer a b m x -> Transducer a c m y
r <>< l = l ><> r
{-# INLINE (<><) #-}

(><>) :: (Functor m) => Transducer a b m x -> Transducer b c m y -> Transducer a c m y
l0' ><> r0' = Trs $ go (unTRS l0') (unTRS r0')
  where
    go l0 r0 = case (toView l0, toView r0) of
        (Pure _ , _)                 -> dropInputs r0
        (_ , Pure a)                 -> dropOutputs l0 >> return a
        (Impure (Panic e a) , _)     -> fromView $ Impure (Panic e (a `go` r0))
        (_ , Impure (Panic e r))     -> fromView $ Impure (Panic e (l0 `go` r))
        (Impure (TLift m) , _)       -> fromView $ Impure (TLift $ (`go` r0) <$> m)
        (_ , Impure (TLift m))       -> fromView $ Impure (TLift $ (l0 `go`) <$> m)
        (_ , Impure (Yield o nextR)) -> unTRS (yield o) >> (l0 `go` nextR)
        (Impure (Await f) , _)       -> fromView $ Impure (Await $ (`go` r0) <$> f)
        (Impure (Try f)   , _)       -> fromView $ Impure (Try $ (`go` r0) <$> f)
        (Impure (Yield o nextL) , Impure (Await f)) -> nextL `go` f o
        (Impure (Yield o nextL) , Impure (Try f))   -> nextL `go` f (Just o)
{-# NOINLINE [0] (><>) #-}


dropInputs
    :: (Functor m)
    => FreeMonad (TransducerF i o m) a
    -> FreeMonad (TransducerF x o m) a
dropInputs t = case toView t of
    Pure a -> return a
    Impure (Await _) -> forever $ unTRS await
    Impure (Try f)   -> dropInputs $ f Nothing
    Impure (Yield o a) -> fromView $ Impure (Yield o (dropInputs a))
    Impure (Panic e a) -> fromView $ Impure (Panic e (dropInputs a))
    Impure (TLift m)   -> fromView $ Impure (TLift (dropInputs <$> m))

dropOutputs
    :: Functor m
    => FreeMonad (TransducerF i o m) a
    -> FreeMonad (TransducerF i x m) a
dropOutputs t = case toView t of
    Pure a -> return a
    Impure (Yield _ a) -> dropOutputs a
    Impure (Await f)   -> fromView $ Impure (Await $ dropOutputs <$> f)
    Impure (Try f)     -> fromView $ Impure (Try $ dropOutputs <$> f)
    Impure (Panic e a) -> fromView $ Impure (Panic e $ dropOutputs a)
    Impure (TLift m)   -> fromView $ Impure (TLift $ dropOutputs <$> m)

--------------------------------------------------
-- higher-level API

tmap :: (Functor m,Monad m) => (i -> o) -> Transducer i o m ()
tmap f = forever $ await >>= yield . f

tfilter :: (Functor m, Monad m) => (i -> Bool) -> Transducer i i m ()
tfilter p = forever $ await >>= \x -> when (p x) (yield x)

mapM :: Monad m => (i -> m o) -> Transducer i o m ()
mapM f = forever $ await >>= (lift . f >=> yield)

tfold :: (Functor m, Monad m) => Fold i m a -> Transducer i o m a
tfold (Fold.Fold s0 f outf) = loop s0
  where
    loop s = tryAwait >>= \case
        Nothing -> return $ outf s
        Just x -> lift (f s x) >>= loop

-- yieldList doesn't actually need the Monad constraint, but it's necessary for the yieldListR rule to work.
yieldList :: Monad m => [a] -> Transducer i a m ()
yieldList = mapM_ yield
{-# INLINE [0] yieldList #-}

--------------------------------------------------
-- Fusion stuff


replaceFold :: (Functor m, Monad m) => Fold i m a -> Transducer i () m a
replaceFold f@(Fold.Fold s0 _ fOut) = foldOverR (fOut s0) (rfold f)
{-# INLINE replaceFold #-}

foldOverR
  :: (Functor m, Monad m)
  => a
  -> (forall t. (MonadTrans t, Monad (t m)) => RStream (t m) i -> RStream (t m) a)
  -> Transducer i () m a
foldOverR a0 streamf = case streamf instream of
    RStream s0 step ->
        let loop !sPEC prev s = step s >>= \case
                RStep o s' -> loop SPEC o s'
                RSkip s'   -> loop SPEC prev s'
                Die e s'   -> panic e >> loop SPEC prev s'
                RFinal     -> return prev
        in loop SPEC a0 s0
  where
    instream = RStream () instep
    instep () = do
        maybe RFinal (flip RStep ()) <$> tryAwait
{-# NOINLINE [0] foldOverR #-}

-- Approach to fusion:
--    first we transform a Transducer to a stream function
--      'RStream m i -> RStream m o'
--    which is lifted into a Transducer by 'overR'
--    This is slightly lossy, so we can only perform it when we know
--    something about how the transducer's return value is used.
--
--    next collapse composed 'overR/overR' pairs into a single 'overR'.
--    This lets GHC see a bunch of composed, non-recursive stream
--    transformers, which it can optimize well (a la stream fusion).
--
--    finally, if we see the driver running a 'foldOverR', collapse that
--    too.
--
--    Currently this only works well if users stick to provided functions
--    or manually lower to streams.  Work on general fusion is ongoing..
{-# RULES
"overR/overR" forall (x :: forall t. (MonadTrans t, Monad (t m)) => RStream (t m) a -> RStream (t m) b) (y:: forall t. (MonadTrans t, Monad (t m)) => RStream (t m) b -> RStream (t m) c). (><>) (overR x) (overR y) = overR (y . x)

"overR/foldOverR" forall (x :: forall t. (MonadTrans t, Monad (t m)) => RStream (t m) a -> RStream (t m) b) y0 (y:: forall t. (MonadTrans t, Monad (t m)) => RStream (t m) b -> RStream (t m) c). overR x ><> foldOverR y0 y = foldOverR y0 (y . x)

"runTrans/foldOverR" forall o0 (f :: forall t. (MonadTrans t, Monad (t m)) => RStream (t m) a -> RStream (t m) b). runTrans (foldOverR o0 f) = runStreamF o0 f
    #-}

{-# RULES
"lower/tmap" forall f. tmap f = overR (rmap f)
"lower/tfilter" forall p. tfilter p = overR (rfilter p)
"lower/mapM" forall f. mapM f = overR (rmapM (lift . f))
"lower/yieldList" forall xs. yieldList xs = overR (ryieldList xs)
"lower/tfold" forall f. tfold f = replaceFold f
  -- I think I can do this more directly, maybe.
    #-}

-- so underR/overR means that Transducer is isomorphic to a stream transformer
-- function.  But the stream version gives better fusion, while Transducers are
-- probably easier to think about (and probably better for certain
-- recursive constructs)
-- I don't have much use for 'underR' ATM.  Maybe I'll think of something...
underR :: (Functor m, Monad m) => Transducer i o m a -> RStream m i -> RStream m o
underR m (RStream s0 stepInStream) = RStream (unTRS m,s0) getNext
  where
    getNext (toView -> Pure _,_) = return RFinal
    getNext (aw@(toView -> (Impure (Await f))),s) = stepInStream s >>= \case
        RStep i s' -> getNext (f i,s')
        RSkip s' -> getNext (aw,s')
        Die e s' -> getNext (unTRS (panic e) >> aw,s')
        RFinal   -> return RFinal
    getNext (aw@(toView -> (Impure (Try f))),s) = stepInStream s >>= \case
        RStep i s' -> getNext (f $ Just i, s')
        RSkip s'   -> getNext (aw, s')
        Die e s'   -> getNext (unTRS (panic e) >> aw, s')
        RFinal     -> getNext (f Nothing, s)
    getNext (toView -> Impure (Yield o a),s) = return $ RStep o (a,s)
    getNext (toView -> Impure (TLift n),s)   = n >>= getNext . (,s)
    getNext (toView -> Impure (Panic e k),s) = return $ Die e (k,s)
    getNext (_,_)   = error "underR: ghc is stupid, this case isn't really missing..."

overR
    :: (Monad m)
    => (forall t. (MonadTrans t, Monad (t m)) => RStream (t m) a -> RStream (t m) b)
    -> Transducer a b m ()
overR streamf = case streamf instream of
    RStream s0 step ->
        let loop !sPEC s = step s >>= \case
                RStep o s' -> yield o >> loop SPEC s'
                RSkip s'   -> loop SPEC s'
                Die e s'   -> panic e >> loop SPEC s'
                RFinal     -> return ()
        in loop SPEC s0
  where
    instream = RStream () instep
    instep _ = do
        maybe RFinal (flip RStep ()) <$> tryAwait
{-# NOINLINE [0] overR #-}

{-# NOINLINE [0] runTrans #-}
runTrans :: (Functor m, Monad m) => Transducer i o m a -> m (Maybe a)
runTrans t0 = go SPEC $ unTRS t0
  where
    go !sPEC t = case toView t of
      Pure a -> return $ Just a
      Impure (Await _) -> return Nothing
      Impure (Try f)   -> go SPEC $ f Nothing
      Impure (Panic e _) -> throw e
      Impure (TLift m)   -> m >>= go SPEC
      Impure (Yield _ m) -> go SPEC m
