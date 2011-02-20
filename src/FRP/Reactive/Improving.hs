{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Improving
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Improving values -- efficient version
----------------------------------------------------------------------

module FRP.Reactive.Improving
  (
    Improving(..), exactly, before, after, minI, maxI
  , batch
  ) where


import Data.Function (on)
import Text.Show.Functions ()
import Control.Applicative (pure,(<$>),liftA2)

import Data.Unamb (unamb,parCommute,pmin,pmax)

import Test.QuickCheck
-- import Test.QuickCheck.Instances
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances.Num


{----------------------------------------------------------
    Improving values
----------------------------------------------------------}

-- | An improving value.
data Improving a = Imp { exact :: a, compareI :: a -> Ordering }
  -- deriving Show

instance Show a => Show (Improving a) where
  show = ("Imp "++) . show . exact

-- | A known improving value (which doesn't really improve)
exactly :: Ord a => a -> Improving a
exactly a = Imp a (compare a)

-- | A value known to be @< x@.
before :: Ord a => a -> Improving a
before x = Imp undefined comp
 where
   comp y | x <= y    = LT
          | otherwise = error "before: comparing before"

-- | A value known to be @> x@.
after :: Ord a => a -> Improving a
after x = Imp undefined comp
 where
   comp y | x >= y    = GT
          | otherwise = error "after: comparing after"


instance Eq a => Eq (Improving a) where
  -- (==) = (==) `on` exact
  -- This version can prove inequality without having to know both values
  -- exactly.
  (==) = parCommute (\ u v -> u `compareI` exact v == EQ)

-- TODO: experiment with these two versions of (==).  The 'parCommute' one
-- can return 'False' sooner than the simpler def, but I doubt it'll
-- return 'True' any sooner.  

instance Ord a => Ord (Improving a) where
  min  = (result.result) fst minI
  (<=) = (result.result) snd minI
  max  = (result.result) fst maxI

-- | Efficient combination of 'min' and '(<=)'
minI :: Ord a => Improving a -> Improving a -> (Improving a,Bool)
~(Imp u uComp) `minI` ~(Imp v vComp) = (Imp uMinV wComp, uLeqV)
 where
   uMinV = if uLeqV then u else v
   -- u <= v: Try @v `compare` u /= LT@ and @u `compare` v /= GT@.
   uLeqV = (vComp u /= LT) `unamb` (uComp v /= GT)
   wComp = liftA2 pmin uComp vComp

--    -- (u `min` v) `compare` t: Try comparing according to whether u <= v,
--    -- or go with either answer if they agree, e.g., if both say GT.
--    -- And say GT if either comp says LT.
--    wComp t = (uCt `asAgree` LT `unamb` vCt `asAgree` LT) -- LT cases
--              `unamb` (uCt `min` vCt)                     -- EQ and GT case
--              where
--                uCt = uComp t
--                vCt = vComp t

-- | Efficient combination of 'max' and '(>=)'
maxI :: Ord a => Improving a -> Improving a -> (Improving a,Bool)
~(Imp u uComp) `maxI` ~(Imp v vComp) = (Imp uMaxV wComp, uGeqV)
 where
   uMaxV = if uGeqV then u else v
   -- u >= v: Try @v `compare` u /= GT@ and @u `compare` v /= LT@.
   uGeqV = (vComp u /= GT) `unamb` (uComp v /= LT)
   wComp = liftA2 pmax uComp vComp

--    -- (u `max` v) `compare` t: Try comparing according to whether u >= v,
--    -- or go with either answer if they agree, e.g., if both say LT.
--    -- And say LT if either comp says GT.
--    wComp t = (uCt `asAgree` GT `unamb` vCt `asAgree` GT) -- GT cases
--              `unamb` (uCt `max` vCt)                     -- EQ and LT case
--              where
--                uCt = uComp t
--                vCt = vComp t

-- TODO: reconsider these wComp tests and look for a smaller set.

-- TODO: factor commonality out of 'minI' and 'maxI' or combine into
-- a single function.

-- TODO: Are the lazy patterns at all helpful?


-- Experimental 'Bounded' instance.  I'm curious about it as an
-- alternative to using 'AddBounds'.  However, it seems to lose the
-- advantage of a knowably infinite value, which I use in a lot of
-- optimization, including filter/join.

-- instance Bounded (Improving a) where
--   minBound = error "minBound not defined on Improving"
--   maxBound = Imp (error "exact maxBound")
--                  (const GT)

instance (Ord a, Bounded a) => Bounded (Improving a) where
  minBound = exactly minBound
  maxBound = exactly maxBound

-- Hack: use 0 as lower bound
-- No, this one won't work, because I'll need to extract the exact value
-- in order to compare with maxBound

-- instance (Ord a, Num a) => Bounded (Improving a) where
--   minBound = exactly 0
--   maxBound = -- exactly maxBound
--              Imp (error "Improving maxBound evaluated")
--                  (const GT)


-- TODO: consider 'undefined' instead 'error', for 'unamb'.  However, we
-- lose valuable information if the 'undefined' gets forced with no
-- 'unamb' to handle it.  Maybe make 'unamb' handle more exceptions.


----


-- Modify the result of a function.  See
-- <http://conal.net/blog/semantic-editor-combinators>.
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)


----

-- For now, generate exactly-knowable values.
-- TODO: generate trickier improving values.

instance (Ord a, Arbitrary a) => Arbitrary (Improving a) where
  arbitrary   = exactly <$> arbitrary

instance (CoArbitrary a) => CoArbitrary (Improving a) where
  coarbitrary = coarbitrary . exact

instance Model (Improving a) a where model = exact

instance EqProp a => EqProp (Improving a) where
  (=-=) = (=-=) `on` exact

-- TODO: revisit (=-=).  Maybe it doesn't have to test for full equality.

genGE :: (Arbitrary a, Num a) => Improving a -> Gen (Improving a)
genGE i = add i <$> oneof [pure 0, positive]

-- I didn't use nonNegative in genGE, because I want zero pretty often,
-- especially for the antiSymmetric law.

add :: Num a => Improving a -> a -> Improving a
add (Imp x comp) dx = Imp (x + dx) (comp . subtract dx)

batch :: TestBatch
batch = ( "Reactive.Improving"
        , concatMap unbatch
           [ ordI, semanticOrdI, partial ]
        )
 where
   ordI = ord (genGE :: Improving NumT -> Gen (Improving NumT))
   semanticOrdI = semanticOrd (undefined :: Improving NumT) 

partial :: TestBatch
partial = ( "Partial"
          , [ ("min after" , property (minAL :: NumT -> NumT -> Bool))
            , ("max before", property (maxAL :: NumT -> NumT -> Bool))
            ]
          )

minAL :: Ord a => a -> a -> Bool
minAL x y = after  x `min` after  y >= exactly (x `min` y)

maxAL :: Ord a => a -> a -> Bool
maxAL x y = before x `max` before y <= exactly (x `max` y)


-- Now I realize that the Ord laws are implied by semantic Ord property,
-- assuming that the model satisfies the Ord laws.

