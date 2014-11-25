{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
  feed2,
  
  -- * main API
  (><>),
  (<><),
  tmap,
  tfilter,
  dropWhileM,
  mapM,
  mealyM,
  tfold,
  tscanl,
  flatten,
  treplicate,
  unfold,
  zip,
  sequence_,
  Fold.mapM_,
  Fold.foldM,

  yieldList,
  feed,
  mstep,
  runTrans,

  -- ** fusion stuff
  foldOverR,
  underR,
) where

import Prelude hiding (zip,mapM,sequence_)
import Transducers.Fold (Fold)
import qualified Transducers.Fold as Fold
import Transducers.Fusion.Fold
import Transducers.Fusion.Stream
import Transducers.FreeMonad

import Control.Applicative
import Control.Exception
import Control.Monad ((>=>), liftM, when, replicateM_)
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Data.Foldable as Foldable

--------------------------------------------------
-- types

-- Functor to represent the main Transducer type
data TransducerF e i o m a =
    Try (Maybe i -> a)
  | Yield o a
  | TLift (m a)  -- is this ok?  Maybe it should be moved to FreeMonad?
  | Panic e a
  deriving Functor

newtype Transducer e i o m a =
    Trs { unTRS :: FreeMonad (TransducerF e i o m) a}
    deriving (Applicative, Monad)

instance Functor (Transducer e i o m) where
    {-# INLINE fmap #-}
    fmap f = t_fmap f
    {-# INLINE (<$) #-}
    (<$) a = t_fmap (const a)
    -- TODO: might be good to have a special function for this case
    -- on Transducers (still should try to rewrite over Fold though)

t_fmap :: (a -> b) -> Transducer e i o m a -> Transducer e i o m b
t_fmap f = Trs . fmap f . unTRS
{-# INLINE [1] t_fmap #-}

instance MonadTrans (Transducer e i o) where
    lift m = Trs $ fromView (Impure (TLift $ liftM return m))

instance MonadIO m => MonadIO (Transducer e i o m) where
    liftIO = lift . liftIO

instance (Functor m, Monad m) => Fold.Folding (Transducer e i o m) where
    type Input (Transducer e i o m) = i
    type FMonad (Transducer e i o m) = m
    {-# INLINE liftFold #-}
    liftFold = tfold

--------------------------------------------------
-- primitive API

yield :: o -> Transducer e i o m ()
yield x = Trs . fromView $ Impure $ Yield x (return ())

await :: Transducer e i o m i
await = Trs . fromView . Impure . Try $ maybe (unTRS await) return

tryAwait :: Transducer e i o m (Maybe i)
tryAwait = Trs . fromView $ Impure (Try return)

panic :: e -> Transducer e i o m ()
panic e = Trs . fromView . Impure $ Panic e (return ())

idT :: Monad m => Transducer e a a m ()
idT = foreach yield
--------------------------------------------------
-- composition

infixr 9 ><>, <><

(<><) :: (Functor m) => Transducer e b c m y -> Transducer e a b m x -> Transducer e a c m y
r <>< l = l ><> r
{-# INLINE (<><) #-}

(><>) :: (Functor m) => Transducer e a b m x -> Transducer e b c m y -> Transducer e a c m y
l0' ><> r0' = Trs $ go (unTRS l0') (unTRS r0')
  where
     go l0 r0 = case (toView l0, toView r0) of
         (_ , Pure a)                 -> return a
         (_ , Impure (Yield o nextR)) -> unTRS (yield o) >> (l0 `go` nextR)
         (_ , Impure (Panic e r))     -> fromView $ Impure (Panic e (l0 `go` r))
         (_ , Impure (TLift m))       -> fromView $ Impure (TLift $ (l0 `go`) <$> m)
         (Impure (Panic e a) , _)     -> fromView $ Impure (Panic e (a `go` r0))
         (Impure (TLift m) , _)       -> fromView $ Impure (TLift $ (`go` r0) <$> m)
         (Impure (Try f)   , _)       -> fromView $ Impure (Try $ (`go` r0) <$> f)
         (Impure (Yield o nextL) , Impure (Try f))   -> nextL `go` f (Just o)
         (Pure _                 , Impure (Try f))   -> l0 `go` f Nothing
{-# NOINLINE [0] (><>) #-}

--------------------------------------------------
-- higher-level API

tmap :: (Functor m,Monad m) => (i -> o) -> Transducer e i o m ()
tmap f = foreach $ yield . f
{-# NOINLINE [0] tmap #-}

tfilter :: (Functor m, Monad m) => (i -> Bool) -> Transducer e i i m ()
tfilter p = foreach $ \x -> when (p x) (yield x)
{-# NOINLINE [0] tfilter #-}

mapM :: Monad m => (i -> m o) -> Transducer e i o m ()
mapM f = foreach $ lift . f >=> yield
{-# NOINLINE [0] mapM #-}

tfold :: (Functor m, Monad m) => Fold i m a -> Transducer e i o m a
tfold (Fold.Fold s0 f outf) = loop s0
  where
    loop s = tryAwait >>= \case
        Nothing -> lift $ outf s
        Just x -> lift (f s x) >>= loop
{-# INLINE [0] tfold #-}

{-# RULES
"<trx> fmap/tfold" forall f g. t_fmap f (tfold g) = tfold (fmap f g)
"<trx> fmap/fmap"  forall f g h. t_fmap f (t_fmap g h) = t_fmap (f . g) h
    #-}

tscanl :: (Functor m, Monad m) => Fold i m a -> Transducer e i a m ()
tscanl (Fold.Fold s0 f outf) = loop s0
  where
    loop s = tryAwait >>= \case
        Nothing -> return ()
        Just i -> do
            !s' <- lift $ f s i
            yield =<< lift (outf s')
            loop s'
{-# NOINLINE [0] tscanl #-}

-- TODO: this should return the final state
mealyM :: (Functor m, Monad m) => s -> (s -> i -> m (o,s)) -> Transducer e i o m ()
mealyM s0 f = loop s0
  where
    loop s = tryAwait >>= \case
        Nothing -> return ()
        Just i -> do
            (o,s') <- lift $ f s i
            yield o
            loop s'
{-# NOINLINE [0] mealyM #-}

dropWhileM
    :: (Functor m, Monad m)
    => (i -> m Bool)
    -> Transducer e i i m ()
dropWhileM p = dropLoop
  where
    dropLoop = tryAwait >>= \case
        Nothing -> return ()
        Just i -> lift (p i) >>= \case
            True  -> dropLoop
            False -> yield i >> idT
{-# NOINLINE [0] dropWhileM #-}

-- | Zip two transducers together, so that inputs are applied to both.
zip :: (Functor m, Monad m) => Transducer e i o m a -> Transducer e i o m b -> Transducer e i o m (a,b)
zip l0 r0 = loop l0 r0
  where
    loop l r = do
        i <- tryAwait
        l' <- feed1 i l
        r' <- feed1 i r
        case (tryGetTrans l', tryGetTrans r') of
            (Just a, _) -> (a,) <$> r'
            (_,Just b)  -> (,b) <$> l'
            (Nothing,Nothing) -> loop l' r'
{-# NOINLINE [0] zip #-}

sequence_ :: (Functor m, Monad m) => [Transducer e i o m a] -> Transducer e i o m ()
sequence_ xs = foldr (((<$) () .) . zip) (Fold.liftFold f_null) xs
{-# INLINE sequence_ #-}

{-# RULES
"<trx> zip/fold"  forall f g. zip (tfold f) (tfold g) = tfold (f_zip f g)
    #-}

-- yieldList doesn't actually need the Monad constraint, but it's necessary for the yieldListR rule to work.
yieldList :: Monad m => [i] -> Transducer e i i m ()
yieldList xs = mapM_ yield xs >> idT
{-# INLINE [0] yieldList #-}

feed
    :: (Functor m, Monad m)
    => i -> Transducer e i o m a -> Transducer e i o m a
feed i (Trs tr0) = Trs $ loop tr0
  where
    loop tr = case toView tr of
        Impure (Try f)   -> f (Just i)
        Impure f -> fromView $ Impure (loop <$> f)
        Pure _ -> tr

feed1
    :: (Functor m, Monad m)
    => Maybe i -> Transducer e i o m a -> Transducer e i' o m (Transducer e i o m a)
feed1 i (Trs tr0) = loop tr0
  where
    loop tr = case toView tr of
        Impure (Try f)     -> return . Trs $ f i
        Impure (Yield o m) -> yield o >> loop m
        Impure (TLift m)   -> lift m >>= loop
        Impure (Panic e m) -> panic e >> loop m
        Pure a             -> return $ return a

feed2
    :: (Functor m, Monad m)
    => i -> Transducer e i o m a -> m ([o], Transducer e i o m a, Maybe e)
feed2 i (Trs tr0) = loop [] tr0
  where
    loop os tr = case toView tr of
        Impure (Try f)     -> return (reverse os, Trs $ f (Just i), Nothing)
        Impure (Yield o m) -> loop (o:os) m
        Impure (TLift m)   -> m >>= loop os
        Impure (Panic e m) -> return (reverse os, Trs m, Just e)
        Pure a             -> return (reverse os, return a, Nothing)

-- attempt to step the transducer by performing any monadic actions
-- TODO: generalize this to dump any 'o's somewhere
mstep :: (Functor m, Monad m) => Transducer e i o m a -> m (Transducer e i o m a)
mstep (Trs tr0) = loop tr0
  where
    loop tr = case toView tr of
        Impure (TLift m) -> m >>= loop
        _                -> return (Trs tr)

--------------------------------------------------
-- concat/flatten-type things

treplicate :: Monad m => Int -> Transducer e i i m ()
treplicate n = foreach $ replicateM_ n . yield
{-# NOINLINE [0] treplicate #-}

unfold
    :: Monad m => (i -> s) -> (s -> Maybe (o,s))
    -> Transducer e i o m ()
unfold mkS unf = foreach $ loop SPEC . mkS
  where
    loop !sPEC s = case unf s of
        Just (o,s') -> yield o >> loop SPEC s'
        Nothing -> return ()
{-# NOINLINE [0] unfold #-}

-- | If (t i) is already existing, this isn't as optimal as it could be.
-- Need to add some enumFromTo/replicate/etc. functions so
-- everything fuses away.
flatten :: (Foldable.Foldable t, Monad m) => Transducer e (t i) i m ()
flatten = foreach $ Foldable.mapM_ yield
{-# INLINE [0] flatten #-}

{-# RULES
"<trx> flatten/list"  flatten = overR r_flattenList
"<trx> flatten/maybe" flatten = overR r_flattenMaybe
"<trx> treplicate" forall n. treplicate n = overR (r_replicate n)
"<trx> unfold" forall mkS unf. unfold mkS unf = overR (r_unfold mkS unf)
    #-}

--------------------------------------------------
-- Fusion stuff

-- producer-flow

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
"<trx> overR/overR" forall (x :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) () b) (y:: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () b -> RStream e (t m) o c). (><>) (overR x) (overR y) = overR (y . x)

"<trx> runTrans/foldOverR" forall (f :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) o2 b). runTrans (foldOverR f) = runStreamF f

"<trx> overR/fold" forall (x :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) o b) y. (><>) (overR x) (tfold y) = foldOverR (r_fold y . x)
"<trx> overR/foldOverR" forall (x :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) () b) (y:: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () b -> RStream e (t m) o3 c). overR x ><> foldOverR y = foldOverR (y . x)

    #-}

-- so underR/overR means that Transducer is isomorphic to a stream transformer
-- function.  But the stream version gives better fusion, while Transducers are
-- probably easier to think about (and probably better for certain
-- recursive constructs)
-- I don't have much use for 'underR' ATM.  Maybe I'll think of something...
underR :: (Functor m, Monad m) => Transducer e i o m a -> RStream e m a i -> RStream e m a o
underR m (RStream s0 stepInStream) = RStream (unTRS m,s0) getNext
  where
    getNext (toView -> Pure a,_) = return $ RFinal a
    getNext (aw@(toView -> (Impure (Try f))),s) = stepInStream s >>= \case
        RStep i s' -> getNext (f $ Just i, s')
        RSkip s'   -> getNext (aw, s')
        Die e s'   -> getNext (unTRS (panic e) >> aw, s')
        RFinal a   -> getNext (f Nothing >> return a, s)
    getNext (toView -> Impure (Yield o a),s) = return $ RStep o (a,s)
    getNext (toView -> Impure (TLift n),s)   = n >>= getNext . (,s)
    getNext (toView -> Impure (Panic e k),s) = return $ Die e (k,s)
    getNext (_,_)   = error "underR: ghc is stupid, this case isn't really missing..."

overR
    :: (Monad m)
    => (forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) o b)
    -> Transducer e a b m o
overR streamf = case streamf instream of
    RStream s0 step ->
        let loop !sPEC s = step s >>= \case
                RStep o s' -> yield o >> loop SPEC s'
                RSkip s'   -> loop SPEC s'
                Die e s'   -> panic e >> loop SPEC s'
                RFinal o   -> return o
        in loop SPEC s0
  where
    instream = RStream () instep
    instep _ = do
        maybe (RFinal ()) (flip RStep ()) <$> tryAwait
{-# NOINLINE [0] overR #-}

-- like overR, but specializes the stream output to (), and doesn't ever
-- actually yield anything.
foldOverR
  :: (Functor m, Monad m)
  => (forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () i -> RStream e (t m) a o)
  -> Transducer e i () m a
  -- TODO: investigate turning this into a producer that outputs values
foldOverR streamf = case streamf instream of
    RStream s0 step ->
        let loop !sPEC s = step s >>= \case
                RStep o s' -> loop SPEC s'
                RSkip s'   -> loop SPEC s'
                Die e s'   -> panic e >> loop SPEC s'
                RFinal a   -> return a
        in loop SPEC s0
  where
    instream = RStream () instep
    instep () = do
        maybe (RFinal ()) (flip RStep ()) <$> tryAwait
{-# NOINLINE [0] foldOverR #-}


#define STREAMMATCH(RNAME,VARS,RHS) "<trx> stream/RNAME" forall VARS (g :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) y b). (><>) (RNAME VARS) (overR g) = overR (g .  RHS)
#define STREAMMATCH2(RNAME,VARS,RHS) "<trx> l-stream/RNAME" forall VARS (g :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) () b). (><>) (overR g) (RNAME VARS) = overR (RHS . g)
#define STREAMMATCH3(RNAME,VARS,RHS) "<trx> l-stream/RNAME reassoc" forall VARS (g :: forall t. (MonadTrans t, Monad (t m)) => RStream e (t m) () a -> RStream e (t m) () b) h. (><>) (overR g) ((><>) (RNAME VARS) h) = overR (RHS . g) ><> h
#define FOLDMATCH(RNAME,VARS,RHS) "<trx> RNAME/fold" forall VARS g. (><>) (RNAME VARS) (tfold g) = tfold (RHS g)

{-# RULES
STREAMMATCH(tfilter,p,r_filter p)
STREAMMATCH(tmap,f,r_map f)
STREAMMATCH(mapM,f,r_mapM (lift . f))
STREAMMATCH(dropWhileM,p,r_dropWhileM (lift . p))
STREAMMATCH(mealyM,s f,r_mealyM s (\s' i -> lift (f s' i)))

STREAMMATCH2(tfilter,p,r_filter p)
STREAMMATCH2(tmap,f,r_map f)
STREAMMATCH2(mapM,f,r_mapM (lift . f))
STREAMMATCH2(dropWhileM,p,r_dropWhileM (lift . p))
-- STREAMMATCH2(mealyM,s f,r_mealyM s (\s' i -> lift (f s' i)))

STREAMMATCH3(tfilter,p,r_filter p)
STREAMMATCH3(tmap,f,r_map f)
STREAMMATCH3(mapM,f,r_mapM (lift . f))
STREAMMATCH3(dropWhileM,p,r_dropWhileM (lift . p))

"<trx> produce/yieldList" forall xs. yieldList xs = overR (r_yieldList xs)

FOLDMATCH(tfilter,p,f_filter p)
FOLDMATCH(tmap,f,f_map f)
FOLDMATCH(mapM,f,f_mapM f)
FOLDMATCH(dropWhileM,p,f_dropWhileM p)
FOLDMATCH(mealyM,s f,f_mealyM s f)

  -- I think I can do this more directly, maybe.
"<trx> prod/tscanl" forall f. tscanl f = overR (r_scan f)
    #-}
-- TODO: fuse feed

{-
-- consumer-flow
overF
    :: Monad m
    => (forall t b. (MonadTrans t, Monad (t m)) => Fold o (t m) b -> Fold i (t m) a)
    -> Transducer e i o m a
overF foldf = case foldf outfold of
    Fold.Fold s0 f mkOut ->
      let loop !sPEC s = tryAwait >>= \case
              Just i  -> f s i >>= loop SPEC
              Nothing -> mkOut s
      in loop SPEC s0
  where
    outfold = Fold.Fold () outstep (const $ return ())
    outstep _ o = yield o
{-# NOINLINE [0] overF #-}
-}

--------------------------------------------------

-- run a transducer, ignoring all output values
{-# NOINLINE [0] runTrans #-}
runTrans :: (Functor m, Monad m, Exception e) => Transducer e i o m a -> m a
runTrans t0 = go SPEC $ unTRS t0
  where
    go !sPEC t = case toView t of
      Pure a -> return a
      Impure (Try f)   -> go SPEC $ f Nothing
      Impure (Panic e _) -> throw e
      Impure (TLift m)   -> m >>= go SPEC
      Impure (Yield _ m) -> go SPEC m

-- | Attempt to retrieve the output value of a transducer without performing
-- any extra evaluations.
tryGetTrans :: (Functor m, Monad m) => Transducer e i o m a -> Maybe a
tryGetTrans t0 = case toView $ unTRS t0 of
    Pure a -> Just a
    _ -> Nothing

--------------------------------------------------
-- Internal

foreach
  :: (Monad m) => (i -> Transducer e i o m ()) -> Transducer e i o m ()
foreach f = loop
  where
    loop = tryAwait >>= \case
        Nothing -> return ()
        Just x -> f x >> loop
{-# INLINE foreach #-}
