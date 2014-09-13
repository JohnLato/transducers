{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module NoRewrite where

import qualified Transducers.Transducers as T

{-# NOINLINE tmap #-}
tmap = T.tmap
{-# NOINLINE tfilter #-}
tfilter = T.tfilter
{-# NOINLINE tscanl #-}
tscanl = T.tscanl
{-# NOINLINE tfold #-}
tfold = T.tfold
{-# NOINLINE mapM #-}
mapM = T.mapM
{-# NOINLINE flatten #-}
flatten = T.flatten
{-# NOINLINE unfold #-}
unfold = T.unfold
