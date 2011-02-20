{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Max
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Max monoid
----------------------------------------------------------------------

module Data.Max (Max(..)) where


import Data.Monoid (Monoid(..))

import Test.QuickCheck (Arbitrary, CoArbitrary)
import Test.QuickCheck.Checkers (EqProp)


-- | Ordered monoid under 'max'.
newtype Max a = Max { getMax :: a }
	deriving (Eq, Ord, Bounded, Read, Show, EqProp, Arbitrary, CoArbitrary)

instance (Ord a, Bounded a) => Monoid (Max a) where
	mempty = Max minBound
	Max a `mappend` Max b = Max (a `max` b)
