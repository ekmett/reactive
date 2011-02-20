{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.AddBounds
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Add bounds to an ordered type
----------------------------------------------------------------------

module Data.AddBounds (AddBounds(..)) where

import Control.Applicative (pure,(<$>))

import Data.Unamb (unamb)

import Data.AffineSpace

-- Testing
import Test.QuickCheck
import Test.QuickCheck.Checkers


-- | Wrap a type into one having new least and greatest elements,
-- preserving the existing ordering.
data AddBounds a = MinBound | NoBound a | MaxBound
	deriving (Eq {-, Ord-}, Read, Show)

instance Bounded (AddBounds a) where
	minBound = MinBound
	maxBound = MaxBound


-- Normally, I'd derive 'Ord' as well, but there's a sticky point.  The
-- derived instance uses the default definition of 'min', which is uses
-- '(<=)' and thus cannot exploit any partial information.  So, define our
-- own 'min' in terms of 'min' on @a@.
-- Examples:
--   (NoBound undefined) `min` (NoBound undefined) can return (NoBound _|_)
--   using this definition, but will not produce any output using the
--   default min.
--   
--   (NoBound a) `min` (NoBound b) can return partial information from
--   a `min` b while the default implementation cannot.

-- instance Ord a => Ord (AddBounds a) where
--   MinBound  <= _         = True
--   NoBound _ <= MinBound  = False
--   NoBound a <= NoBound b = a <= b
--   NoBound _ <= MaxBound  = True
--   MaxBound  <= MaxBound  = True
--   MaxBound  <= _         = False        -- given previous 
  
--   MinBound  `min` _         = MinBound
--   _         `min` MinBound  = MinBound
--   NoBound a `min` NoBound b = NoBound (a `min` b)
--   u         `min` MaxBound  = u
--   MaxBound  `min` v         = v
  
--   MinBound  `max` v         = v
--   u         `max` MinBound  = u
--   NoBound a `max` NoBound b = NoBound (a `max` b)
--   _         `max` MaxBound  = MaxBound
--   MaxBound  `max` _         = MaxBound


-- The definition above is too strict for some uses.  Here's a parallel
-- version.


-- Alternatively, make a non-parallel definition here and use 'pmin'
-- instead of 'min' where I want.


-- General recipe for Ord methods: use unamb to try two strategies.  The
-- first one, "justB", only examines b.  The second one first examines
-- only examines a and then examines both.  I take care that the two
-- strategies handle disjoint inputs.  I could instead let the second
-- strategy handle the first one redundantly, being careful that they
-- agree.

-- This instance is very like the one Richard Smith (lilac) constructed.
-- It fixes a couple of small bugs and follows a style that helps me see
-- that I'm covering all of the cases with the evaluation order I want.

instance Ord a => Ord (AddBounds a) where
  a <= b = justB b `unamb` (a <=* b)
   where
     justB MaxBound = True
     justB _        = undefined

     MinBound  <=* _         = True
     _         <=* MinBound  = False
     NoBound u <=* NoBound v = u <= v
     MaxBound  <=* NoBound _ = False
     _         <=* MaxBound  = undefined

  a `min` b = justB b `unamb` (a `min'` b)
   where
     justB MinBound    = MinBound
     justB MaxBound    = a
     justB (NoBound _) = undefined
     
     MinBound  `min'` _         = MinBound
     MaxBound  `min'` v         = v
     NoBound u `min'` NoBound v = NoBound (u `min` v)
     _         `min'` MinBound  = undefined
     _         `min'` MaxBound  = undefined

  a `max` b = justB b `unamb` (a `max'` b)
   where
     justB MaxBound    = MaxBound
     justB MinBound    = a
     justB (NoBound _) = undefined
     
     MaxBound  `max'` _         = MaxBound
     MinBound  `max'` v         = v
     NoBound u `max'` NoBound v = NoBound (u `max` v)
     _         `max'` MaxBound  = undefined
     _         `max'` MinBound  = undefined


-- instance Arbitrary a => Arbitrary (AddBounds a) where
--   arbitrary = frequency [ (1 ,pure MinBound)
--                         , (10, NoBound <$> arbitrary)
--                         , (1 ,pure MaxBound) ]
--   coarbitrary MinBound    = variant 0
--   coarbitrary (NoBound a) = variant 1 . coarbitrary a
--   coarbitrary MaxBound    = variant 2

instance Arbitrary a => Arbitrary (AddBounds a) where
  arbitrary = frequency [ (1 ,pure MinBound)
                        , (10, NoBound <$> arbitrary)
                        , (1 ,pure MaxBound) ]

instance CoArbitrary a => CoArbitrary (AddBounds a) where
  coarbitrary MinBound    = variant (0::Int)
  coarbitrary (NoBound a) = variant (1::Int) . coarbitrary a
  coarbitrary MaxBound    = variant (2::Int)

instance (EqProp a, Eq a) => EqProp (AddBounds a) where
  NoBound a =-= NoBound b = a =-= b
  u =-= v = u `eq` v


-- Hm.  I'm dissatisfied with this next instance.  I'd like to tweak my
-- type definitions to eliminate these partial definitions.

instance AffineSpace t => AffineSpace (AddBounds t) where
  type Diff (AddBounds t) = Diff t
  NoBound u .-. NoBound v = u .-. v
  -- I don't know what to do here
  _ .-. _ = error "(.-.) on AddBounds: only defined on NoBound args"
  NoBound u .+^ v = NoBound (u .+^ v)
  _ .+^ _ = error "(.+^) on AddBounds: only defined on NoBound args"
