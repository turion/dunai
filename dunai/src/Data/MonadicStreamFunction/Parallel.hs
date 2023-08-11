-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Versions of arrow combinators that run things in parallel using 'par', if
-- possible.
module Data.MonadicStreamFunction.Parallel where

-- External imports
import Control.Arrow (arr, (>>>))
import GHC.Conc      (par, pseq)

-- Internal imports
import Data.MonadicStreamFunction              ()
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- | Run two 'MSF's in parallel, taking advantage of parallelism if possible.
-- This is the parallel version of '***'.
(*|*) :: Monad m => MSF m a b -> MSF m c d -> MSF m (a, c) (b, d)
MSF s1_ t1 *|* MSF s2_ t2 = MSF (s1_, s2_) $ \(a, c) (s1, s2) -> do
  (b, s1') <- t1 a s1
  (d, s2') <- t2 c s2
  b `par` d `pseq` return ((b, d), (s1', s2'))

-- | Parallel version of '&&&'.
(&|&) :: Monad m => MSF m a b -> MSF m a c -> MSF m a (b, c)
msf1 &|& msf2 = arr (\a -> (a, a)) >>> (msf1 *|* msf2)
