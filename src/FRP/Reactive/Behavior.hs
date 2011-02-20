{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, TypeOperators
           , StandaloneDeriving, GeneralizedNewtypeDeriving
           , TypeSynonymInstances, UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Behavior
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Reactive behaviors (continuous time)
----------------------------------------------------------------------

module FRP.Reactive.Behavior
  (
    BehaviorG, Behavior, Behaviour
  , time
  , stepper, switcher --, select
  , snapshotWith, snapshot, snapshot_, whenE
  , accumB, scanlB, monoidB, maybeB, flipFlop, countB
  , sumB, integral
  ) where

import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative,(<$>),pure)
-- import Control.Monad (join)

import Data.Default

import Data.Copointed

import Control.Compose ((:.)(..),unO)

import Data.VectorSpace
import Data.AffineSpace

import qualified FRP.Reactive.Reactive as R
import FRP.Reactive.Reactive
  ( ImpBounds, TimeT, EventG, ReactiveG
  , withTimeE,onceRestE,diffE,joinMaybes,result)
import FRP.Reactive.Fun
-- import FRP.Reactive.Improving
import FRP.Reactive.Internal.Behavior

-- type EventI    t = EventG    (Improving t)
-- type ReactiveI t = ReactiveG (Improving t)
-- type BehaviorI t = BehaviorG (Improving t) t

type EventI    t = EventG    (ImpBounds t)
type ReactiveI t = ReactiveG (ImpBounds t)
type BehaviorI t = BehaviorG (ImpBounds t) t

-- | Time-specialized behaviors.
-- Note: The signatures of all of the behavior functions can be generalized.  Is
-- the interface generality worth the complexity?
type Behavior = BehaviorI TimeT

-- Synonym for 'Behavior'
type Behaviour = Behavior


-- | The identity generalized behavior.  Has value @t@ at time @t@.
-- 
-- > time :: Behavior TimeT
time :: (Ord t) => BehaviorI t t
time = beh (pure (fun id))

-- Turn a reactive value into a discretly changing behavior.
rToB :: ReactiveI t a -> BehaviorI t a
rToB = beh . fmap pure

-- Then use 'rToB' to promote reactive value functions to behavior
-- functions.

-- | Discretely changing behavior, based on an initial value and a
-- new-value event.
-- 
-- >stepper :: a -> Event a -> Behavior a
stepper :: a -> EventI t a -> BehaviorI t a
stepper = (result.result) rToB R.stepper

-- Suggested by Robin Green:

-- stepper = select pure

-- -- | Use a key event to key into a behaviour-valued function
-- select :: (a -> Behavior b) -> a -> Event a -> Behavior b
-- select f a e = f a `switcher` (f <$> e)

-- Looking for a more descriptive name.

-- | Switch between behaviors.
-- 
-- > switcher :: Behavior a -> Event (Behavior a) -> Behavior a
switcher :: (Ord tr, Bounded tr) =>
            BehaviorG tr tf a
         -> EventG tr (BehaviorG tr tf a)
         -> BehaviorG tr tf a
b `switcher` eb = beh (unb b `R.switcher` (unb <$> eb))

-- | Snapshots a behavior whenever an event occurs and combines the values
-- using the combining function passed.  Take careful note of the order of
-- arguments and results.
-- 
-- > snapshotWith :: (a -> b -> c) -> Behavior b -> Event a -> Event c
snapshotWith :: (Ord t) =>
                (a -> b -> c)
             -> BehaviorI t b -> EventI t a -> EventI t c
snapshotWith h b e = f <$> (unb b `R.snapshot` withTimeE e)
 where
   f ((a,t),tfun) = h a (tfun `apply` t)


-- 'snapshotWith' is where tr meets tf.  withTimeE is specialized from
-- withTimeGE, converting the ITime into a TimeT.  This specialization
-- interferes with the generality of several functions in this module,
-- which are therefore now still using 'Behavior' instead of 'BehaviorG'.
-- Figure out how to get generality.


-- | Snapshot a behavior whenever an event occurs.  See also
-- 'snapshotWith'.  Take careful note of the order of arguments and
-- results.
-- 
-- > snapshot :: Behavior b -> Event a -> Event (a,b)
snapshot :: (Ord t) => BehaviorI t b -> EventI t a -> EventI t (a,b)
snapshot = snapshotWith (,)

-- TODO: tweak withTimeE so that 'snapshotWith' and 'snapshot' can have
-- more general types.  The problem is that withTimeE gives a friendlier
-- kind of time, namely known and finite.  Necessary?

-- Alternative implementations:
--   snapshotWith c e b = uncurry c <$> snapshot e b
--   snapshotWith c = (result.result.fmap) (uncurry c) snapshot

-- | Like 'snapshot' but discarding event data (often @a@ is '()').
-- 
-- > snapshot_ :: Behavior b -> Event a -> Event b
snapshot_ :: (Ord t) => BehaviorI t b -> EventI t a -> EventI t b
snapshot_ = snapshotWith (flip const)

-- Alternative implementations
-- e `snapshot_` src = snd <$> (e `snapshot` src)
-- snapshot_ = (result.result.fmap) snd snapshot

-- | Filter an event according to whether a reactive boolean is true.
-- 
-- > whenE :: Behavior Bool -> Event a -> Event a
whenE :: (Ord t) => BehaviorI t Bool -> EventI t a -> EventI t a
b `whenE` e = joinMaybes (h <$> (b `snapshot` e))
 where
   h (a,True)  = Just a
   h (_,False) = Nothing

-- TODO: Same comment about generality as with snapshot

-- | Behavior from an initial value and an updater event.  See also
-- 'accumE'.
-- 
-- > accumB :: a -> Event (a -> a) -> Behavior a
accumB :: a -> EventI t (a -> a) -> BehaviorI t a
accumB = (result.result) rToB R.accumR

-- -- | Like 'scanl' for behaviors.  See also 'scanlE'.
-- scanlB :: (a -> b -> a) -> a -> Event b -> Behavior a
-- scanlB = (result.result.result) rToB R.scanlR

-- -- | Accumulate values from a monoid-valued event.  Specialization of
-- -- 'scanlB', using 'mappend' and 'mempty'.  See also 'monoidE'.
-- monoidB :: Monoid a => Event a -> Behavior a
-- monoidB = result rToB R.monoidR


---- The next versions are more continuous:

-- type RF a = Reactive (Fun TimeT a)

-- scanlB :: forall a c. (Behavior a -> c -> Behavior a) -> Behavior a
--        -> Event c -> Behavior a
-- scanlB f b0 e = beh (scanlRF f' (unb b0) e)
--  where
--    f' :: RF a -> c -> RF a
--    f' r c = unb (f (beh r) c)

-- scanlRF :: (RF a -> c -> RF a) -> RF a -> Event c -> RF a
-- scanlRF h rf0 e = join (R.scanlR h rf0 e)

-- monoidB :: Monoid a => Event (Behavior a) -> Behavior a
-- monoidB = scanlB mappend mempty

-- -- I doubt the above definitions work well.  They accumulate reactives without
-- -- aging them.  See 'accumE'.


-- | Like 'scanl' for behaviors.  See also 'scanlE'.
-- 
-- > scanlB :: forall a. (Behavior a -> Behavior a -> Behavior a) -> Behavior a
-- >        -> Event (Behavior a) -> Behavior a

-- TODO: generalize scanlB's type

scanlB :: forall a b tr tf. (Ord tr, Bounded tr) =>
          (b -> BehaviorG tr tf a -> BehaviorG tr tf a)
       -> BehaviorG tr tf a
       -> EventG tr b -> BehaviorG tr tf a
scanlB plus zero = h
 where
   h :: EventG tr b -> BehaviorG tr tf a
   h e = zero `switcher` (g <$> onceRestE e)
   g :: (b, EventG tr b) -> BehaviorG tr tf a
   g (b, e') = b `plus` h e'


-- | Accumulate values from a monoid-valued event.  Specialization of
-- 'scanlB', using 'mappend' and 'mempty'.  See also 'monoidE'.
-- 
-- > monoidB :: Monoid a => Event (Behavior a) -> Behavior a
monoidB :: (Ord tr, Bounded tr, Monoid a) => EventG tr (BehaviorG tr tf a)
        -> BehaviorG tr tf a
monoidB = scanlB mappend mempty

-- | Like 'sum' for behaviors.
-- 
-- > sumB :: AdditiveGroup a => Event a -> Behavior a
sumB :: (Ord t, AdditiveGroup a) => EventI t a -> BehaviorI t a
sumB = result rToB R.sumR

-- | Start out blank ('Nothing'), latching onto each new @a@, and blanking
-- on each @b@.  If you just want to latch and not blank, then use
-- 'mempty' for the second event.
-- 
-- > maybeB :: Event a -> Event b -> Behavior (Maybe a)
maybeB :: (Ord t) =>
          EventI t a -> EventI t b -> BehaviorI t (Maybe a)
maybeB = (result.result) rToB R.maybeR

-- | Flip-flopping behavior.  Turns true whenever first event occurs and
-- false whenever the second event occurs.
-- 
-- > flipFlop :: Event a -> Event b -> Behavior Bool
flipFlop :: (Ord t) => EventI t a -> EventI t b -> BehaviorI t Bool
flipFlop = (result.result) rToB R.flipFlop

-- | Count occurrences of an event.  See also 'countE'.
-- 
-- > countB :: Num n => Event a -> Behavior n
countB :: (Ord t, Num n) => EventI t a -> BehaviorI t n
countB = result rToB R.countR

-- | Euler integral.
-- 
-- > integral :: (VectorSpace v, Scalar v ~ TimeT) =>
-- >             Event () -> Behavior v -> Behavior v
integral :: (VectorSpace v, AffineSpace t, Scalar v ~ Diff t, Ord t) =>
            EventI t a -> BehaviorI t v -> BehaviorI t v
integral t b = sumB (snapshotWith (*^) b (diffE (time `snapshot_` t)))

-- TODO: This integral definition is piecewise-constant.  Change to piecewise-linear.


-- TODO: find out whether this integral works recursively.  If not, then
-- fix the implementation, rather than changing the semantics.  (No
-- "delayed integral".)
-- 
-- Early experiments suggest that recursive integration gets stuck.
-- Chuan-kai Lin has come up with a new lazier R.snapshotWith, but it
-- leaks when the reactive value changes in between event occurrences.


---- Comonadic stuff

-- Orphan.  Move elsewhere

instance (Copointed g, Copointed f) => Copointed (g :. f) where
  copoint = copoint . copoint . unO

deriving instance (Monoid tr, Default tf) => Copointed (BehaviorG tr tf) 

-- ITime and TimeT are not currently monoids.  They can be when I wrap
-- them in the Sum monoid constructor, in which mempty = 0 and mappend =
-- (+).  This monoid change moves us from absolute to relative time.  What
-- do I do for never-occuring futures and terminating events?

-- 

-- instance (Ord t, Monoid t, Monoid (Improving t)) => Comonad (BehaviorI t) where
--   duplicate = duplicateB

-- duplicateB :: forall t a.
--               (Ord t, Monoid t, Monoid (Improving t)) =>
--               BehaviorI t -> BehaviorI t (BehaviorI t a) where
--   duplicate b@(_ `Stepper`) = bb0 `switcher` 
--    where
--      f0 `R.Stepper` e = unb b
--      bb0 = beh (pure (fun (\ t -> undefined)))

-- f0 :: T a

-- e :: E (T a)

-- duplicate f0 :: T (T a)


-- b :: B a

-- unb b :: R (T a)

-- dup b :: B (B a)

-- TODO: generalize to BehaviorG
-- TODO: something about Monoid (Improving t)

-- Standard instances for applicative functors

-- #define APPLICATIVE Behavior
-- #include "Num-inc.hs"
