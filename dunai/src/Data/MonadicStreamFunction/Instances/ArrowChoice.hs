{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Instance of 'ArrowChoice' for Monadic Stream Functions ('MSF').
--
-- Import this module to include that (orphan) instance.
module Data.MonadicStreamFunction.Instances.ArrowChoice where

-- External imports
import Control.Arrow (ArrowChoice (..))

-- Internal imports
import Data.MonadicStreamFunction.Core         ()
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- | 'ArrowChoice' instance for MSFs.
instance Monad m => ArrowChoice (MSF m) where
  left :: MSF m a b -> MSF m (Either a c) (Either b c)
  left (MSF s_ t) = MSF s_ f
    where
      f (Left a) s = do
        (b, s') <- t a s
        return (Left b, s')
      f (Right c) s = return (Right c, s)
