{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Instance of 'ArrowPlus' for Monadic Stream Functions ('MSF').
--
-- Import this module to include that (orphan) instance.
--
-- This is only defined for monads that are instances of 'MonadPlus'.
module Data.MonadicStreamFunction.Instances.ArrowPlus where

-- External imports
import Control.Applicative (Alternative (..))
import Control.Arrow       (ArrowPlus (..), ArrowZero (..), second)
import Control.Monad       (MonadPlus, mplus, mzero)

-- Internal imports
import Data.MonadicStreamFunction.Core         ()
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- | Instance of 'ArrowZero' for Monadic Stream Functions ('MSF'). The monad
-- must be an instance of 'MonadPlus'.
instance (Monad m, MonadPlus m) => ArrowZero (MSF m) where
  zeroArrow = MSF () $ const $ const mzero

-- | Instance of 'ArrowPlus' for Monadic Stream Functions ('MSF'). The monad
-- must be an instance of 'MonadPlus'.
instance (Monad m, MonadPlus m) => ArrowPlus (MSF m) where
  MSF s1_ t1 <+> MSF s2_ t2 = MSF (Left (s1_, s2_)) go
    where
      go a (Left (s1, s2)) = go a (Right (Left s1)) `mplus` go a (Right (Right s2))
      go a (Right (Left s1)) = second (Right . Left) <$> t1 a s1
      go a (Right (Right s2)) = second (Right . Right) <$> t2 a s2

  -- MSF s1_ t1 <+> MSF s2_ t2 = MSF (s1_, s2_) $ \a (s1, s2) -> (second (, s2) <$> t1 a s1) `mplus` (second (s1, ) <$> t2 a s2)

-- | Instance of 'Alternative' for Monadic Stream Functions ('MSF'),
-- implemented using the 'ArrowZero' and 'ArrowPlus' instances.
instance (Functor m, Monad m, MonadPlus m) => Alternative (MSF m a) where
  empty = zeroArrow
  (<|>) = (<+>)
