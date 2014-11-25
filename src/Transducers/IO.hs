{-# LANGUAGE DeriveDataTypeable #-}
module Transducers.IO (
    SeekException (..),
    Transducers.IO.withFile,
    enumHandle,
) where

import Transducers.Fold
import Transducers.FreeMonad
import Transducers.Transducers
import Control.Monad.IO.Class
import Control.Exception
import Data.Typeable
import System.IO
import qualified Data.ByteString as B

data SeekException = SeekException SeekMode Integer
    deriving (Typeable, Show)

instance Exception SeekException

{-# INLINE [1] enumHandle #-}
enumHandle :: Int -> Handle -> Transducer SeekException B.ByteString o IO a -> Transducer SeekException () o IO a
enumHandle ck h (Trs tr0) = loop tr0
  where
    {-# INLINE [0] loop #-}
    loop tr = case toView tr of
        Pure a -> return a
        Impure (Try f)     -> do
            b <- liftIO $ B.hGetSome h ck
            if B.null b then loop $ f Nothing else loop $ f (Just b)
        Impure (Yield o m) -> yield o >> loop m
        Impure (TLift m)   -> liftIO m >>= loop
        Impure (Panic (SeekException sm off) m) -> liftIO (hSeek h sm off) >> loop m

{-# INLINE [1] withFile #-}
withFile :: Int -> FilePath -> Transducer SeekException B.ByteString o IO a -> IO a
withFile ck fp (Trs tr0) = System.IO.withFile fp ReadMode $ \h -> loop h tr0
  where
    {-# INLINE [0] loop #-}
    loop h tr = case toView tr of
        Pure a -> return a
        Impure (Try f)     -> do
            b <- B.hGetSome h ck
            if B.null b then loop h $ f Nothing else loop h $ f (Just b)
        Impure (Yield _ m) -> loop h m
        Impure (TLift m)   -> m >>= loop h
        Impure (Panic (SeekException sm off) m) -> hSeek h sm off >> loop h m

{-# INLINE [1] f_withFile #-}
f_withFile :: Int -> FilePath -> Fold B.ByteString IO a -> IO a
f_withFile ck fp (Fold s0 fStep mkOut) = System.IO.withFile fp ReadMode $ \h -> go h s0
  where
    {-# INLINE [0] go #-}
    go h s = do
        b <- B.hGetSome h ck
        if B.null b then mkOut s else fStep s b >>= go h

{-# RULES
"<trx> withFile/Fold" forall n fp f. Transducers.IO.withFile n fp (tfold f) = f_withFile n fp f
-- TODO: withFile/Stream
    #-}

-- TODO: probably the individual loops here should use spec-constr
