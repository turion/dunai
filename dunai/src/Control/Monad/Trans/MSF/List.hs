{-# LANGUAGE CPP #-}
-- The following warning id disabled so that we do not see warnings during
-- compilation caused by the intentional use of ListT.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'MSF's with a list monadic layer.
--
-- This module contains functions to work with MSFs that include a 'ListT'
-- monadic layer. MSFs on a list monad may produce multiple outputs and
-- continuations, or none. This enables the possibility for spawning new MSFs,
-- or stopping MSFs, at will.
--
-- A common use case is to be able to dynamically spawn new interactive
-- elements in applications (e.g., a game object that splits in two, or that
-- fires to an enemy).
--
-- WARNING: the ListT transformer is considered dangerous, and imposes
-- additional constraints on the inner monad in order for the combination of
-- the monad and the transformer to be a monad. Use at your own risk.
module Control.Monad.Trans.MSF.List
    {-# WARNING "This module uses the ListT transformer, which is considered deprecated." #-}
    ( module Control.Monad.Trans.MSF.List
    , module Control.Monad.Trans.List
    )
  where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad.Trans.List hiding (liftCallCC, liftCatch)

-- Internal imports
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF), morphGG, morphGS, morphGS')
import Control.Monad.Trans.State
import Data.Traversable (for)
import Data.MonadicStreamFunction (liftTransS, ArrowPlus (..), ArrowZero (zeroArrow))

-- * List monad

-- | Run an 'MSF' in the 'ListT' transformer (i.e., multiple MSFs producing
-- each producing one output), by applying the input stream to each MSF in the
-- list transformer and concatenating the outputs of the MSFs together.
--
-- An MSF in the ListT transformer can spawn into more than one MSF, or none,
-- so the outputs produced at each individual step are not guaranteed to all
-- have the same length.
widthFirst :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
widthFirst = morphGG $ \(c, transition) -> ([c], \a c' -> fmap unzip . runListT $ ListT (return c') >>= transition a)
{-# INLINE widthFirst #-}

-- | Build an 'MSF' in the 'ListT' transformer by broadcasting the input stream
-- value to each MSF in a given list.
sequenceS :: Monad m => [MSF m a b] -> MSF (ListT m) a b
sequenceS = foldr ((<+>) . liftTransS) zeroArrow
{-# INLINE sequenceS #-}

-- FIXME: Could easily extend to any traversable
-- | Apply an 'MSF' to every input.
mapMSF :: Monad m => MSF m a b -> MSF m [a] [b]
mapMSF = morphGS' $ \transition as -> runStateT $ for as $ \a -> StateT (transition a)
{-# INLINE mapMSF #-}
