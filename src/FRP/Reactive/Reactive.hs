{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, TypeOperators
           , FlexibleInstances, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Reactive
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Simple reactive values.  Adds some extra functionality on top of
-- "FRP.Reactive.PrimReactive"
----------------------------------------------------------------------

module FRP.Reactive.Reactive
  (
    module FRP.Reactive.PrimReactive
  , ImpBounds, exactNB, {-TimeFinite,-} TimeT, ITime, Future
  , traceF
    -- * Event
  , Event
  , withTimeE, withTimeE_
  , atTime, atTimes, listE
  , {-mbsEvent,-} zipE, scanlE, monoidE
  , firstRestE, firstE, restE
  , remainderR, snapRemainderE, onceRestE
  , withPrevE, withPrevEWith, withNextE, withNextEWith
  , mealy, mealy_, countE, countE_, diffE
    -- * Reactive values
  , Reactive
  , snapshot_, snapshot, whenE
  , scanlR, monoidR, eitherE, maybeR, flipFlop, countR
  , splitE, switchE
  , integral, sumR
    -- * Re-export
  , exact
    -- * Tests
  , batch
  ) where

import Control.Applicative
import Control.Arrow (first,second)
import Control.Monad
import Data.Monoid
import Debug.Trace (trace)

-- import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes ()

-- vector-space
import Data.VectorSpace
import Data.AffineSpace

-- TypeCompose
import Data.Zip (pairEdit)

import Data.Max
import Data.AddBounds
import FRP.Reactive.Future       hiding (batch)
import FRP.Reactive.PrimReactive hiding (batch)
import FRP.Reactive.Improving    hiding (batch)

-- -- | The type of finite time values
-- type TimeFinite = Double

-- | The type of time values with additional min & max elements.
type TimeT = Double
-- type TimeT = AddBounds TimeFinite

type ImpBounds t = Improving (AddBounds t)

-- | Exact & finite content of an 'ImpBounds'
exactNB :: ImpBounds t -> t
exactNB = unNo . exact
 where
   unNo (NoBound t) = t
   unNo _ = error "exactNB: unNo on MinBound or maxBound"

-- TODO: when I switch to relative time, I won't need MinBound, so
-- introduce a HasInfinity class and use infinity in place of maxBound

-- | Improving times, as used for time values in 'Event', 'Reactive',
-- and 'ReactiveB'.
type ITime = ImpBounds TimeT

-- type ITime = Improving TimeT

-- | Type of future values.  Specializes 'FutureG'.
type Future = FutureG ITime

-- -- | Sink, e.g., for an event handler
-- type Sink a = SinkG Time a


-- | Trace the elements of a functor type.
traceF :: Functor f => (a -> String) -> f a -> f a
traceF shw = fmap (\ a -> trace (shw a) a)

-- traceShowF :: (Functor f,Show a) => f a -> f a
-- traceShowF = traceF show


{--------------------------------------------------------------------
    Events
--------------------------------------------------------------------}

-- | Events, specialized to improving doubles for time
type Event = EventG ITime

-- | Access occurrence times in an event.  See 'withTimeGE' for more
-- general notions of time.
-- 
-- > withTimeE :: Event a -> Event (a, TimeT)
withTimeE :: Ord t =>
             EventG (ImpBounds t) d -> EventG (ImpBounds t) (d, t)
withTimeE e = second (exactNB.timeT) <$> withTimeGE e

-- | Access occurrence times in an event.  Discard the rest.  See also
-- 'withTimeE'.
-- 
-- > withTimeE_ :: Event a -> Event TimeT
withTimeE_ :: Ord t =>
              EventG (ImpBounds t) d -> EventG (ImpBounds t) t
withTimeE_ = (result.fmap) snd withTimeE

timeT :: Ord t => Time t -> t
timeT (Max t) = t

-- timeT (Max (NoBound t)) = t
-- timeT _                 = error "timeT: non-finite time"

-- | Single-occurrence event at given time.  See 'atTimes' and 'atTimeG'.
atTime ::  TimeT -> Event ()
atTime = atTimes . pure

-- atTime = atTimeG . exactly . NoBound

-- | Event occuring at given times.  See also 'atTime' and 'atTimeG'.
atTimes ::  [TimeT] -> Event ()
atTimes = atTimesG . fmap (exactly . NoBound)


-- | Convert a temporally monotonic list of timed values to an event.  See also
-- the generalization 'listEG'
listE :: [(TimeT,a)] -> Event a
listE = listEG . fmap (first (exactly . NoBound))

-- | Generate a pair-valued event, given a pair of initial values and a
-- pair of events.  See also 'pair' on 'Reactive'.  Not quite a 'zip',
-- because of the initial pair required.
zipE :: (Ord t, Bounded t) => (c,d) -> (EventG t c, EventG t d) -> EventG t (c,d)
zipE cd cde = cd `accumE` pairEdit cde

-- | Like 'scanl' for events.
scanlE :: (Ord t, Bounded t) => (a -> b -> a) -> a -> EventG t b -> EventG t a
scanlE f a e = a `accumE` (flip f <$> e)

-- | Accumulate values from a monoid-typed event.  Specialization of
-- 'scanlE', using 'mappend' and 'mempty'.
monoidE :: (Ord t, Bounded t, Monoid o) => EventG t o -> EventG t o
monoidE = scanlE mappend mempty



-- | Decompose an event into its first occurrence value and a remainder
-- event.  See also 'firstE' and 'restE'.
firstRestE :: (Ord t, Bounded t) => EventG t a -> (a, EventG t a)
firstRestE = futVal . eventOcc

-- | Extract the first occurrence value of an event.  See also
-- 'firstRestE' and 'restE'.
firstE :: (Ord t, Bounded t) => EventG t a -> a
firstE = fst . firstRestE

-- | Extract the remainder an event, after its first occurrence.  See also
-- 'firstRestE' and 'firstE'.
restE :: (Ord t, Bounded t) => EventG t a -> EventG t a
restE = snd . firstRestE



-- | Remaining part of an event.  See also 'withRestE'.
remainderR :: (Ord t, Bounded t) => EventG t a -> ReactiveG t (EventG t a)
remainderR e = e `stepper` (snd <$> withRestE e)


-- | Tack remainders a second event onto values of a first event.  Occurs
-- when the first event occurs.
snapRemainderE :: (Ord t, Bounded t) =>
                  EventG t b -> EventG t a -> EventG t (a, EventG t b)
snapRemainderE = snapshot . remainderR

-- snapRemainderE eb = snapshot (remainderR eb)

-- eb `snapRemainderE` ea = remainderR eb `snapshot` ea

-- withTailE ea eb = error "withTailE: undefined" ea eb


-- | Convert an event into a single-occurrence event, whose occurrence
-- contains the remainder.
onceRestE :: (Ord t, Bounded t) => EventG t a -> EventG t (a, EventG t a)
onceRestE = once . withRestE



-- | Pair each event value with the previous one.  The second result is
-- the old one.  Nothing will come out for the first occurrence of @e@,
-- but if you have an initial value @a@, you can do @withPrevE (pure a
-- `mappend` e)@.
withPrevE :: (Ord t, Bounded t) => EventG t a -> EventG t (a,a)
withPrevE e = (joinMaybes . fmap combineMaybes) $
              (Nothing,Nothing) `accumE` fmap (shift.Just) e
 where
   -- Shift newer value into (new,old) pair if present.
   shift :: u -> (u,u) -> (u,u)
   shift newer (new,_) = (newer,new)
   combineMaybes :: (Maybe u, Maybe v) -> Maybe (u,v)
   combineMaybes = uncurry (liftA2 (,))


-- | Same as 'withPrevE', but allow a function to combine the values.
-- Provided for convenience.
withPrevEWith :: (Ord t, Bounded t) => (a -> a -> b) -> EventG t a -> EventG t b
withPrevEWith f e =  fmap (uncurry f) (withPrevE e)


-- | Pair each event value with the next one one.  The second result is
-- the next one.
withNextE :: (Ord t, Bounded t) => EventG t a -> EventG t (a,a)
withNextE = (result.fmap.second) firstE withRestE
-- Alt. def.
-- withNextE = fmap (second firstE) . withRestE

-- | Same as 'withNextE', but allow a function to combine the values.
-- Provided for convenience.
withNextEWith :: (Ord t, Bounded t) => (a -> a -> b) -> EventG t a -> EventG t b
withNextEWith f e =  fmap (uncurry f) (withNextE e)


-- | Mealy-style state machine, given initial value and transition
-- function.  Carries along event data.  See also 'mealy_'.
mealy :: (Ord t, Bounded t) => s -> (s -> s) -> EventG t b -> EventG t (b,s)
mealy s0 f = scanlE h (b0,s0)
 where
   b0        = error "mealy: no initial value"
   h (_,s) b = (b, f s)

-- | Mealy-style state machine, given initial value and transition
-- function.  Forgetful version of 'mealy'.
mealy_ :: (Ord t, Bounded t) => s -> (s -> s) -> EventG t b -> EventG t s
mealy_ = (result.result.result.fmap) snd mealy

-- mealy_ s0 f e = snd <$> mealy s0 f e


-- | Count occurrences of an event, remembering the occurrence values.
-- See also 'countE_'.
countE :: (Ord t, Bounded t, Num n) => EventG t b -> EventG t (b,n)
countE = mealy 0 (+1)

-- | Count occurrences of an event, forgetting the occurrence values.  See
-- also 'countE'.
countE_ :: (Ord t, Bounded t, Num n) => EventG t b -> EventG t n
countE_ = (result.fmap) snd countE

-- countE_ e = snd <$> countE e

-- | Difference of successive event occurrences.  See 'withPrevE' for a
-- trick to supply an initial previous value.
diffE :: (Ord t, Bounded t, AffineSpace a) =>
         EventG t a -> EventG t (Diff a)
diffE = withPrevEWith (.-.)

-- -- | Returns an event whose occurrence's value corresponds with the input
-- --   event's previous occurence's value.
-- delayE :: Event a -> Event a
-- delayE = withPrevEWith (flip const)

-- I suspect that delayE will only be used to hide implementation
-- problems, so I removed it.  - Conal

{--------------------------------------------------------------------
    Reactive extras (defined via primitives)
--------------------------------------------------------------------}

-- | Reactive values, specialized to improving doubles for time
type Reactive = ReactiveG ITime

-- -- | Compatibility synonym (for ease of transition from DataDriven)
-- type Source = Reactive


-- | Snapshot a reactive value whenever an event occurs.
snapshot :: (Ord t, Bounded t) => ReactiveG t b -> EventG t a -> EventG t (a,b)
snapshot = snapshotWith (,)

-- | Like 'snapshot' but discarding event data (often @a@ is '()').
snapshot_ :: (Ord t, Bounded t) => ReactiveG t b -> EventG t a -> EventG t b
snapshot_ = snapshotWith (flip const)

-- Alternative implementations
-- e `snapshot_` src = snd <$> (e `snapshot` src)
-- snapshot_ = (result.result.fmap) snd snapshot

-- | Filter an event according to whether a reactive boolean is true.
whenE :: (Ord t, Bounded t) => EventG t a -> ReactiveG t Bool -> EventG t a
whenE e = joinMaybes . fmap h . flip snapshot e
 where
   h (a,True)  = Just a
   h (_,False) = Nothing

-- | Like 'scanl' for reactive values.  See also 'scanlE'.
scanlR :: (Ord t, Bounded t) => (a -> b -> a) -> a -> EventG t b -> ReactiveG t a
scanlR f a e = a `stepper` scanlE f a e

-- | Accumulate values from a monoid-valued event.  Specialization of
-- 'scanlE', using 'mappend' and 'mempty'.  See also 'monoidE'.
monoidR :: (Ord t, Bounded t, Monoid a) => EventG t a -> ReactiveG t a
monoidR = scanlR mappend mempty

-- Equivalently,
--   monoidR = stepper mempty . monoidE

-- | Combine two events into one.
eitherE :: (Ord t, Bounded t) => EventG t a -> EventG t b -> EventG t (Either a b)
eitherE ea eb = ((Left <$> ea) `mappend` (Right <$> eb))

-- | Start out blank ('Nothing'), latching onto each new @a@, and blanking
-- on each @b@.  If you just want to latch and not blank, then use
-- 'mempty' for @lose@.
maybeR :: (Ord t, Bounded t) => EventG t a -> EventG t b -> ReactiveG t (Maybe a)
maybeR get lose =
  Nothing `stepper` ((Just <$> get) `mappend` (Nothing <$ lose))

-- | Flip-flopping reactive value.  Turns true when @ea@ occurs and false
-- when @eb@ occurs.
flipFlop :: (Ord t, Bounded t) => EventG t a -> EventG t b -> ReactiveG t Bool
flipFlop ea eb =
  False `stepper` ((True <$ ea) `mappend` (False <$ eb))

-- TODO: redefine maybeR and flipFlop in terms of eitherE.

-- | Count occurrences of an event.  See also 'countE'.
countR :: (Ord t, Bounded t, Num n) => EventG t a -> ReactiveG t n
countR e = 0 `stepper` countE_ e

-- | Partition an event into segments.
splitE :: (Ord t, Bounded t) => EventG t b -> EventG t a -> EventG t (a, EventG t b)
eb `splitE` ea = h <$> (eb `snapRemainderE` withRestE ea)
 where
   h ((a,ea'),eb') = (a, eb' `untilE` ea')

-- | Switch from one event to another, as they occur.  (Doesn't merge, as
-- 'join' does.)
switchE :: (Ord t, Bounded t) => EventG t (EventG t a) -> EventG t a
switchE = join . fmap (uncurry untilE) . withRestE


-- | Euler integral.
integral :: forall v t. (VectorSpace v, AffineSpace t, Scalar v ~ Diff t) =>
            t -> Event t -> Reactive v -> Reactive v
integral t0 newT r = sumR (snapshotWith (*^) r deltaT)
  where
    deltaT :: Event (Diff t)
    deltaT = diffE (pure t0 `mappend` newT)

-- TODO: find out whether this integral works recursively.  If not, then
-- fix the implementation, rather than changing the semantics.  (No
-- "delayed integral".)

sumR :: (Ord t, Bounded t) => AdditiveGroup v => EventG t v -> ReactiveG t v
sumR = scanlR (^+^) zeroV


{----------------------------------------------------------
    Tests
----------------------------------------------------------}

batch :: TestBatch
batch = ( "FRP.Reactive.Reactive"
        , concatMap unbatch
            [ 
            -- Write some tests!
            ]
        )
