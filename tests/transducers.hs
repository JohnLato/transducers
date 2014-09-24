{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fenable-rewrite-rules -ddump-rule-firings -ddump-to-file #-}

import Control.Applicative
import Control.Arrow (first)
import qualified Control.Exception as Ex
import Control.Monad.Writer
import qualified Data.Foldable as Fold
import Data.IORef
import Data.Maybe
import qualified Data.Sequence as Q
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Property as QC
import Text.PrettyPrint
import Text.Show.Functions ()

import qualified NoRewrite as NR
import qualified Transducers.Fold as TrFold
import Transducers.FreeMonad
import Transducers.Par
import Transducers.Transducers as Tr

main :: IO ()
main = defaultMain
  [ testProperty "yield" prop_yield
  , testProperty "tryAwait" prop_tryAwait
  , testProperty "panic" prop_panic
  , testProperty "return" prop_return
  , testProperty "bind" prop_bind
  , testProperty "bind_assoc" prop_bind_assoc
  , testProperty "comp" prop_comp
  -- , testProperty "comp_assoc" _prop_comp_assoc
  -- , testProperty "comp_left_id" _prop_comp_left_id
  -- , testProperty "comp_right_id" _prop_comp_right_id
  , testProperty "tmap" prop_tmap
  , testProperty "tfilter" prop_tfilter
  , testProperty "mapM" prop_mapM
  , testProperty "mapM" prop_dropWhileM
  , testProperty "tfold" prop_tfold
  , testProperty "tscanl" prop_tscanl
  , testProperty "feed" prop_feed
  , testProperty "rewrite_tmap" prop_rewrite_tmap
  , testProperty "rewrite_tfilter" prop_rewrite_tfilter
  , testProperty "rewrite_flatten" prop_rewrite_flatten
  , testProperty "rewrite_tscanl" prop_rewrite_tscanl
  , testProperty "rewrite_tfold" prop_rewrite_tfold
  , testProperty "rewrite_mapM" prop_rewrite_mapM
  , testProperty "rewrite_dropWhileM" prop_rewrite_dropWhileM
  , testProperty "flatten/tfilter" prop_flatten_tfilter
  , testProperty "par" prop_par
  ]

-- | A monad where we can easily observe all side effects.
type TestMonad = Writer (Q.Seq SideEffect)
-- | A side effect in 'TestMonad'.
type SideEffect = Int

-- | Result of executing a transducer
data Trace i o a = Trace
  [i] -- ^ remaining input
  [Event o] -- ^ history of events
  (Maybe a) -- ^ result, or Nothing if the input ends prematurely
  deriving (Eq, Show)

-- | A thing a transducer can do.
data Event o
  = TryE
  | YieldE o
  | PanicE TestException
  | TLiftE (Q.Seq SideEffect)
  deriving (Show, Eq)

-- | An exception type for testing. We only allow this type of exception,
-- because other exception types are not comparable in general.
newtype TestException = TestException Int
  deriving (Typeable, Show, Eq)
instance Ex.Exception TestException

instance Arbitrary TestException where
  arbitrary = TestException <$> arbitrary

-- | A type-restricted version of 'Transducer', together with a string
-- representation.
data TestTransducer = TestTransducer
  { testTransducerExpression :: String
  , unTestTransducer :: Transducer Ex.SomeException Int Int TestMonad Int
  }

instance Show TestTransducer where
  show = testTransducerExpression

instance Arbitrary TestTransducer where
  arbitrary = fmap (uncurry TestTransducer . first renderPDoc) $
    arbitraryTransducerWith $ \gen -> do
    (doc, val) <- gen
    effects <- arbitrary
    let doc' =
          opl 1 ">>"
            (app (lit "tell") $ app (lit "Q.fromList") (lit $ show effects))
            (app (lit "return") doc)
    return (doc', val <$ tell (Q.fromList effects))

-- | Pretty-printed expression with operator precedence.
type PDoc = Int -> Doc

-- | Turn a PDoc into String.
renderPDoc :: PDoc -> String
renderPDoc = render . ($0)

-- | Left-associative operator
opl :: Int -> String -> PDoc -> PDoc -> PDoc
opl prec opr x y p = parensIf (prec < p) $ fsep
  [ x prec
  , text opr
  , y (prec + 1)
  ]

-- | Literal
lit :: String -> PDoc
lit str _ = text str

-- | Function application
app :: PDoc -> PDoc -> PDoc
app x y p = parensIf (10 < p) $ fsep
  [ x 10
  , y 11
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

arbitraryTransducerWith
  :: forall o a i m
  .  (Arbitrary o, Arbitrary a, Show a, Show o)
  => (forall r. QC.Gen (PDoc, r) -> QC.Gen (PDoc, m r))
  -> QC.Gen (PDoc, Transducer Ex.SomeException i o m a)
arbitraryTransducerWith genM = fun1 "Trs" Trs <$> go
  where
    go :: QC.Gen (PDoc, FreeMonad (TransducerF Ex.SomeException i o m) a)
    go = fmap (fun1 "fromView" fromView) $ QC.sized $ \size ->
      if size == 0
        then fun1 "Pure" Pure . addDoc <$> arbitrary
        else fmap (fun1 "Impure" Impure) $ QC.resize (size-1) $ QC.oneof
          [ fun1 "Try" Try . mkConst <$> go
          , fun2 "Yield" Yield . addDoc <$> arbitrary <*> go
          , fun2 "Panic" Panic
              <$> (fun1 "Ex.toException" Ex.toException
                  . fun1 "TestException" TestException . addDoc <$> arbitrary)
              <*> go
          , fun1 "TLift" TLift <$> genM go
          ]

    mkConst = fun1 "const" const
    addDoc x = (\_ -> parens $ text (show x), x)

    fun1 str f (doc, val) = (app (lit str) doc, f val)
    fun2 str f (doc0, val0) (doc1, val1)
      = (app (app (lit str) doc0) doc1, f val0 val1)

--arbitraryFunction :: CoArbitrary a => QC.Gen b -> QC.Gen (a -> b)
--arbitraryFunction gen = QC.promote (`QC.coarbitrary` gen)

-- | Run a transducer using the given input.
exec :: Transducer Ex.SomeException i o TestMonad a -> [i] -> Trace i o a
exec = execWith True

-- | Run a transducer using the given input. Does not assume the EOF
-- at the end of the input.
_execPartial :: Transducer Ex.SomeException i o TestMonad a -> [i] -> Trace i o a
_execPartial = execWith False

execWith :: Bool -> Transducer Ex.SomeException i o TestMonad a -> [i] -> Trace i o a
execWith terminate trans0 is0 = Trace remaining (Fold.toList events) out
  where
    ((out, remaining), events) = runWriter $ go (unTRS trans0) is0

    go trans is = case toView trans of
      Pure x -> return (Just x, is)
      Impure act -> case act of
        Try cont -> do
          emit TryE
          case is of
            []
              | terminate -> go (cont Nothing) []
              | otherwise -> return (Nothing, is)
            i:rest -> go (cont (Just i)) rest
        Yield o cont -> do
          emit $ YieldE o
          go cont is
        Panic e cont -> case Ex.fromException e of
          Just myEx -> do
            emit $ PanicE myEx
            go cont is
          Nothing -> error $ "exec: unknown exception " ++ show e
        TLift action -> do
          let !(cont, sideEffects) = runWriter action
          emit $ TLiftE sideEffects
          go cont is

emit :: MonadWriter (Q.Seq a) m => a -> m ()
emit = tell . Q.singleton

-- | Run a transducer, and returns a summary of execution
summary
  :: Transducer Ex.SomeException i o TestMonad a
  -> (Bool, [i])
  -> ([i], [o], [SideEffect], Maybe a)
summary trans (terminate, inp) = (remaining, out, effects, end)
  where
    !(Trace remaining evts end) = execWith terminate trans inp
    out = [o | YieldE o <- evts ]
    effects = Fold.toList $ mconcat [e | TLiftE e <- evts ]

output' :: Transducer Ex.SomeException i o TestMonad a -> (Bool, [i]) -> (Bool, [o])
output' trans inp = (isJust end, out)
    where
      !(_, out, _, end) = summary trans inp

output :: Transducer Ex.SomeException i o TestMonad a -> (Bool, [i]) -> [o]
output trans inp = snd $ output' trans inp

-- Primitives

prop_yield :: Int -> [Int] -> Bool
prop_yield x is = exec (yield x) is == Trace is [YieldE x] (Just ())

prop_tryAwait :: [Int] -> Bool
prop_tryAwait is = exec tryAwait is == case is of
  i:rest -> Trace rest [TryE::Event ()] (Just (Just i))
  [] -> Trace is [TryE] (Just Nothing)

prop_panic :: TestException -> [Int] -> Bool
prop_panic ex is = exec (panic (Ex.toException ex)) is
  == Trace is [PanicE ex::Event()] (Just ())

-- Transducers as a monad

prop_return :: Int -> [Int] -> Bool
prop_return x is = exec (return x) is == Trace is ([]::[Event()]) (Just x)

prop_bind :: TestTransducer -> (Int -> TestTransducer) -> [Int] -> Bool
prop_bind (TestTransducer _ x) f is
  = exec (x >>= unTestTransducer . f) is == expected
  where
    expected = let
      r0@(Trace is1 evts0 out0) = exec x is
      in case out0 of
        Nothing -> r0
        Just val -> let
          !(Trace is2 evts1 out1) = exec (unTestTransducer $ f val) is1
          in Trace is2 (evts0 ++ evts1) out1

prop_bind_assoc
  :: TestTransducer
  -> (Int -> TestTransducer)
  -> (Int -> TestTransducer)
  -> [Int]
  -> Bool
prop_bind_assoc (TestTransducer _ x) f g is =
  exec (x >>= (unTestTransducer . f) >>= (unTestTransducer . g)) is ==
  exec (x >>= (\v -> unTestTransducer (f v) >>= (unTestTransducer . g))) is

-- Composition

prop_comp
  :: TestTransducer -> TestTransducer -> [Int] -> Bool
prop_comp (TestTransducer _ x) (TestTransducer _ y) is =
  output (x ><> y) (True, is) == output y (output' x (True, is))
  -- Note: this is not a complete specification of (><>), because it
  -- doesn't care how other events are ordered except for YieldEs.

-- does not hold, see counterexamples below.
_prop_comp_assoc
  :: TestTransducer -> TestTransducer -> TestTransducer -> [Int] -> Bool
_prop_comp_assoc (TestTransducer _ x) (TestTransducer _ y) (TestTransducer _ z) is =
  exec (x ><> (y ><> z)) is == exec ((x ><> y) ><> z) is

-- | The identity transducer
identity :: (Monad m) => Transducer Ex.SomeException i i m ()
identity = tryAwait >>= Fold.mapM_ (\item -> yield item >> identity)

-- does not hold, because (identity ><> x) asks for input even if x does not.
_prop_comp_left_id
  :: TestTransducer -> [Int] -> Bool
_prop_comp_left_id (TestTransducer _ x) is = exec (identity ><> x) is == exec x is

-- does not hold
_prop_comp_right_id
  :: TestTransducer -> [Int] -> Bool
_prop_comp_right_id (TestTransducer _ x) is
  = exec (x ><> identity) is == exec (void x) is

-- Higher-level API

prop_tmap :: [Int] -> Bool
prop_tmap is =
  summary (tmap (+1)) (True, is) == ([], map (+1) is, [], Just ())

prop_tfilter :: [Int] -> Bool
prop_tfilter is =
  summary (tfilter even) (True, is) == ([], filter even is, [], Just ())

prop_mapM :: [Int] -> Bool
prop_mapM is =
  summary (Tr.mapM f) (True, is) == ([], map (+1) is, is, Just ())
  where
    f x = do
      emit x
      return $ x + 1

prop_dropWhileM :: [Int] -> Bool
prop_dropWhileM is =
  summary (Tr.dropWhileM f) (True, is) == ([], rest, dropped, Just ())
  where
    (dropped,rest) = span even is
    f x = do
        -- dropWhileM has to run the predicate until it fails, so
        -- here we check the predicate before emitting the tested value
        when (even x) $ emit x
        return $ even x

prop_tfold :: [Int] -> Bool
prop_tfold is = summary (tfold (TrFold.foldM f 0)) (True, is)
  == ([], []::[()], is, Just (sum is))
  where
    f x y = do
      emit y
      return $! x + y

prop_tscanl :: [Int] -> Bool
prop_tscanl is = summary (tscanl (TrFold.foldM f 0)) (True, is)
  == ([], scanl1 (+) is, is, Just ())
  where
    f x y = do
      emit y
      return $! x + y

prop_feed :: TestTransducer -> Int -> [Int] -> Bool
prop_feed (TestTransducer _ x) i is =
  output (feed i x) (True,is) == output x (True, i:is)

-- Fusion

prop_rewrite_tmap :: [Int] -> Bool
prop_rewrite_tmap iss
  = exec (tmap (+1)) iss == exec (NR.tmap (+1)) iss

prop_rewrite_tfilter :: [Int] -> Bool
prop_rewrite_tfilter iss
  = exec (tfilter even) iss == exec (NR.tfilter even) iss

prop_rewrite_flatten :: [[Int]] -> Bool
prop_rewrite_flatten iss
  = exec flatten iss == exec NR.flatten iss

prop_rewrite_tscanl :: [Int] -> Bool
prop_rewrite_tscanl iss
  = exec (tscanl (TrFold.foldM f 0)) iss
    == exec (NR.tscanl (TrFold.foldM f 0)) iss
  where
    f x y = do
      emit y
      return $! x + y

-- does not hold, bug?
prop_rewrite_tfold :: [Int] -> Bool
prop_rewrite_tfold iss
  = exec (tfold (TrFold.foldM f 0)) iss
    == (exec (NR.tfold (TrFold.foldM f 0)) iss :: Trace Int () Int)
  where
    f x y = do
      emit y
      return $! x + y

prop_rewrite_mapM :: [Int] -> Bool
prop_rewrite_mapM iss
  = exec (Tr.mapM f) iss == exec (NR.mapM f) iss
  where
    f x = do
      emit x
      return $! x + 1

prop_rewrite_dropWhileM :: [Int] -> Bool
prop_rewrite_dropWhileM iss
  = exec (Tr.dropWhileM f) iss == exec (NR.dropWhileM f) iss
  where
    f x = do
      emit x
      return $ even x

prop_flatten_tfilter :: [[Int]] -> Bool
prop_flatten_tfilter iss
  = exec (flatten ><> tfilter even) iss
  == exec (flatten ><> noFusion (tfilter even)) iss

noFusion :: a -> a
noFusion = id
{-# NOINLINE noFusion #-}

-- Parallel

prop_par :: [Int] -> QC.Property
prop_par is =
  QC.morallyDubiousIOProperty $ do
    historyRef <- newIORef Q.empty
    return $ QC.forAll (arbitraryIOTrans historyRef) $ \(IOTrans _ iot) ->
      QC.morallyDubiousIOProperty $
      (==)
        <$> evalIOTrans historyRef iot is
        <*> evalIOTrans historyRef (parT iot) is

evalIOTrans
  :: IORef (Q.Seq Int)
  -> Transducer Ex.SomeException i o IO a
  -> [i]
  -> IO ([o], [Int], Maybe a)
evalIOTrans historyRef trans input = do
  writeIORef historyRef Q.empty
  outRef <- newIORef Q.empty
  resultRef <- newIORef Nothing
  runIOTrans $
    yieldList input
      ><> (trans >>= lift . writeIORef resultRef . Just)
      ><> Tr.tfold (TrFold.mapM_ $ \a -> modifyIORef outRef (Q.|>a))
  out <- readIORef outRef
  effects <- readIORef historyRef
  result <- readIORef resultRef
  return (Fold.toList out, Fold.toList effects, result)

runIOTrans :: Transducer Ex.SomeException i o IO a -> IO ()
runIOTrans (Trs j) = loop j
  where
    loop x = case toView x of
      Pure _ -> return ()
      Impure (Yield _ cont) -> loop cont
      Impure (Try cont) -> loop (cont Nothing)
      Impure (Panic _ cont) -> loop cont
      Impure (TLift a) -> a >>= loop

arbitraryIOTrans :: IORef (Q.Seq Int) -> QC.Gen IOTrans
arbitraryIOTrans historyRef = fmap make $ arbitraryTransducerWith $ \gen -> do
  (doc, val) <- gen
  effect <- arbitrary
  let doc' =
        opl 1 ">>"
          (app (lit "write") $ lit $ show effect)
          (app (lit "return") doc)
  return (doc', val <$ modifyIORef historyRef (Q.|>effect))
  where
    make (doc, trans) = IOTrans (renderPDoc doc) trans

data IOTrans = IOTrans String (Transducer Ex.SomeException Int Int IO Int)

instance Show IOTrans where
  show (IOTrans s _) = s

-- Counterexamples

-- prop_comp_assoc
_ce_trans30, _ce_trans31, _ce_trans32 :: Transducer Ex.SomeException Int Int TestMonad Int
_ce_trans30 = Trs (fromView (Impure (Try (const (fromView (Pure (1)))))))
_ce_trans31 = Trs (fromView (Pure 1))
_ce_trans32 = Trs
  (fromView
 (Impure
  (Try
   (const
    (fromView
     (Impure
      (TLift
       (tell (Q.fromList [1]) >> return (fromView (Pure (0)))))))))))
