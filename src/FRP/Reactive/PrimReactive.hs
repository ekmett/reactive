{-# LANGUAGE TypeOperators, ScopedTypeVariables
           , FlexibleInstances, MultiParamTypeClasses
           , GeneralizedNewtypeDeriving
 #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- For ghc-6.6 compatibility
-- {-# OPTIONS_GHC -fglasgow-exts -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.PrimReactive
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Functional /events/ and /reactive values/.  Semantically, an 'Event' is
-- stream of future values in time order.  A 'Reactive' value is a
-- discretly time-varying value.
-- 
-- Many of the operations on events and reactive values are packaged as
-- instances of the standard type classes 'Monoid', 'Functor',
-- 'Applicative', and 'Monad'.
-- 
-- This module focuses on representation and primitives defined in terms
-- of the representation.  See also "FRP.Reactive.Reactive", which
-- re-exports this module, plus extras that do not exploit the
-- representation.  My intention for this separation is to ease
-- experimentation with alternative representations.
-- 
-- Although the basic 'Reactive' type describes /discretely/-changing
-- values, /continuously/-changing values can be modeled simply as
-- reactive functions.  See "FRP.Reactive.Behavior" for a convenient type
-- composition of 'Reactive' and a constant-optimized representation of
-- functions of time.  The exact packaging of discrete vs continuous will
-- probably change with more experience.
----------------------------------------------------------------------

module FRP.Reactive.PrimReactive
  ( -- * Events and reactive values
    EventG, ReactiveG
    -- * Operations on events and reactive values
  , stepper, switcher, withTimeGE, withTimeGR
  , futuresE, futureStreamE, listEG, atTimesG, atTimeG
  , snapshotWith, accumE, accumR, once
  , withRestE, untilE
  , justE, filterE
  -- , traceE, traceR
  -- , mkEvent, mkEventTrace, mkEventShow
  , eventOcc
    -- * To be moved elsewhere
  , joinMaybes, filterMP, result
  -- * To be removed when it gets used somewhere
  , isMonotoneR
  -- * Testing
  , batch, infE, monoid_E
  -- * Temporary exports, while debugging
  -- , snap, merge
  ) where

import Prelude hiding (zip,zipWith)

import Data.Monoid
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.Function (on)
-- import Debug.Trace (trace)

-- TODO: eliminate the needs for this stuff.
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import System.IO.Unsafe

import Data.Stream (Stream(..))

import Control.Comonad

import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
-- import Data.List

-- TypeCompose
import Control.Compose ((:.)(..), inO2, Monoid_f(..))
import Data.Zip
import Control.Instances () -- Monoid (IO ())


import Data.Unamb (unamb, assuming)
import Data.Unamb (race)  -- eliminate

-- import Data.Max
-- import Data.AddBounds
import FRP.Reactive.Future hiding (batch)
import FRP.Reactive.Internal.Reactive

{--------------------------------------------------------------------
    Events and reactive values
--------------------------------------------------------------------}

-- Bogus EqProp instance.  TODO: replace with a random equality test, such
-- that the collection of all generated tests covers equality.

instance (Bounded t, Eq t, Eq a, EqProp t, EqProp a) => EqProp (EventG t a) where
  a =-= b = foldr (.&.) (property True) $ zipWith (=-=) (f a) (f b)
    where
      f = take 20 . eFutures

-- TODO: work less and reach further per (=-=).

arbitraryE :: (Num t, Ord t, Bounded t, Arbitrary t, Arbitrary u) => Gen (EventG t u)
arbitraryE = frequency 
  [ -- (1, liftA2 ((liftA. liftA) futuresE addStart) arbitrary futureList)
    (4, liftA futuresE futureList)
  ]
  where
    -- earliestFuture = Future . (,) (Max MinBound)
    -- addStart = (:).earliestFuture
    futureList = futureListFinite
                 -- frequency [(10, futureListFinite), (1,futureListInf)]
    futureListFinite = liftA2 (zipWith future) nondecreasing arbitrary
--     futureListInf =
--       liftA2 (zipWith future) (resize 10 nondecreasingInf)
--                               (infiniteList arbitrary)

instance (Arbitrary t, Ord t, Bounded t, Num t, Arbitrary a) => Arbitrary (EventG t a) where
  arbitrary   = arbitraryE

instance (CoArbitrary t, CoArbitrary a) => CoArbitrary (EventG t a) where
  coarbitrary = coarbitrary . eFuture

----

-- Arbitrary works just like pairs:

-- instance (Arbitrary t, Arbitrary a, Num t, Ord t, Bounded t) => Arbitrary (ReactiveG t a) where
--   arbitrary = liftA2 Stepper arbitrary arbitrary
--   coarbitrary (a `Stepper` e) = coarbitrary e . coarbitrary a

instance (Arbitrary t, Arbitrary a, Num t, Ord t, Bounded t) => Arbitrary (ReactiveG t a) where
  arbitrary = liftA2 Stepper arbitrary arbitrary

instance (CoArbitrary t, CoArbitrary a) => CoArbitrary (ReactiveG t a) where
  coarbitrary (a `Stepper` e) = coarbitrary e . coarbitrary a

instance (Ord t, Bounded t) => Model (ReactiveG t a) (t -> a) where
  model = rat

instance (Ord t, Bounded t, Arbitrary t, Show t, EqProp a) => EqProp (ReactiveG t a)
 where
   (=-=) = (=-=) `on` model

-- Initial value of a 'Reactive'
rInit :: ReactiveG t a -> a
rInit (a `Stepper` _) = a


{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

instance (Ord t, Bounded t) => Monoid (EventG t a) where
  mempty  = Event mempty
  mappend = inEvent2 merge

-- Standard instance for Applicative of Monoid
instance (Ord t, Bounded t, Monoid a) => Monoid (ReactiveG t a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

-- | Merge two 'Future' reactives into one.
merge :: (Ord t, Bounded t) => Binop (FutureG t (ReactiveG t a))

-- The following two lines seem to be too strict and are causing
-- reactive to lock up.  I.e. the time argument of one of these
-- must have been _|_, so when we pattern match against it, we 
-- block.
-- 
-- On the other hand, they patch a massive space leak in filterE.  Perhaps
-- there's an unamb solution.

u `merge` v =
  assuming (isNeverF u) v `unamb`
  assuming (isNeverF v) u `unamb`
  (inFutR (`merge` v) <$> u) `mappend` (inFutR (u `merge`) <$> v)

-- TODO: redefine via parIdentity from Data.Unamb

-- u `merge` v | isNever u = v
--             | isNever v = u

-- Future (Max MaxBound,_) `merge` v = v
-- u `merge` Future (Max MaxBound,_) = u

-- u `merge` v = 
--   (inFutR (`merge` v) <$> u) `mappend` (inFutR (u `merge`) <$> v)

-- What's going on in this 'merge' definition?  Try two different
-- future paths.  If u arrives before v (or simultaneously), then
-- begin as u begins and then merge v with the rest of u.  Otherwise,
-- begin as v begins and then merge u with the rest of v.  Because of
-- the left-bias, make sure u fragments are always the first argument
-- to merge and v fragments are always the second.


-- Define functor instances in terms of each other.
instance Functor (EventG t) where
  fmap = inEvent.fmap.fmap

instance Functor (ReactiveG t) where
  fmap f ~(a `Stepper` e) = f a `stepper` fmap f e

-- standard instance
instance (Ord t, Bounded t) => Applicative (EventG t) where
  pure = return
  (<*>) = ap
--   _ <*> (Event (Future (Max MaxBound,_))) = mempty
--   x <*> y = x `ap` y

-- standard instance
instance (Ord t, Bounded t) => Alternative (EventG t) where
  { empty = mempty; (<|>) = mappend }

instance (Ord t, Bounded t) => Zip (ReactiveG t) where
  -- zip :: ReactiveG t a -> ReactiveG t b -> ReactiveG t (a,b)
  (c `Stepper` ce) `zip` (d `Stepper` de) =
    (c,d) `accumR` pairEdit (ce,de)

instance (Ord t, Bounded t) => Applicative (ReactiveG t) where
  pure a = a `stepper` mempty
  -- Standard definition.  See 'Zip'.
  rf <*> rx = zipWith ($) rf rx

-- A wonderful thing about the <*> definition for ReactiveG is that it
-- automatically caches the previous value of the function or argument
-- when the argument or function changes.


instance (Ord t, Bounded t) => Monad (EventG t) where
  return a = Event (pure (pure a))
  e >>= f  = joinE (fmap f e)


-- From Jules Bean (quicksilver):

-- joinE :: (Ord t) => EventG t (EventG t a) -> EventG t a
-- joinE (Event u) =
--   Event . join $
--   fmap (\ (e `Stepper` ee) ->
--          let (Event uu) = (e `mappend` joinE ee) in uu)
--   u

-- plus some fiddling:

joinE :: (Ord t, Bounded t) => EventG t (EventG t a) -> EventG t a

joinE (Event u) = Event (u >>= eFuture . g)
 where 
   g (e `Stepper` ee) = e `mappend` joinE ee

-- joinE = inEvent (>>= eFuture . g)
--  where 
--    g (e `Stepper` ee) = e `mappend` joinE ee


-- | Experimental specialization of 'joinMaybes'.
justE :: (Ord t, Bounded t) => EventG t (Maybe a) -> EventG t a
justE ~(Event (Future (t, mb `Stepper` e'))) =
  assuming (t == maxBound) mempty `unamb`
  (inEvent.inFuture.first) (max t) $
    case mb of
      Nothing -> justE e'
      Just a  -> Event (Future (t, a `Stepper` justE e'))


-- This definition is much more efficient than the following.

-- justE = (>>= maybe mzero return)

-- On the other hand, this simpler definition inserts the necessary max
-- applications so that we needn't find a Just in order to have a lower bound.

-- TODO: find and fix the inefficiency.





-- | Experimental specialization of 'filterMP'.
filterE :: (Ord t, Bounded t) => (a -> Bool) -> EventG t a -> EventG t a
filterE p m = justE (liftM f m)
 where
   f a | p a        = Just a
       | otherwise  = Nothing


{-

-- happy a t b. Same as (a `mappend` b) except takes advantage of knowledge
-- that t is a lower bound for the occurences of b. This allows for extra
-- laziness.
happy :: (Ord t) => EventG t a ->
                    Time t ->
                    EventG t a ->
                    EventG t a
happy a t b =
  assuming (isNeverE a) b `unamb`
  assuming (isNeverF b) a `unamb`
  happy' a t b ...


happy a (Max MaxBound) _ = a
happy (Event (Future (Max MaxBound, _))) _ b = b
happy a@(Event (Future (t0, e `Stepper` ee'))) t b 
  | t0 <= t = (Event (Future (t0, e `Stepper` (happy ee' t b))))
  | otherwise = a `mappend` b

-- Note, joinE should not be called with an infinite list of events that all
-- occur at the same time.  It can't decide which occurs first.
joinE :: (Ord t) => EventG t (EventG t a) -> EventG t a
joinE (Event (Future (Max MaxBound, _))) = mempty
joinE (Event (Future (t0h, e `Stepper` ((Event (Future (Max MaxBound, _)))))))
  = adjustE t0h e
joinE (Event (Future (t0h, e `Stepper` ee'@((Event (Future (t1h, _)))))))
  = happy (adjustE t0h e) t1h (adjustTopE t0h (joinE ee'))
-}

{-
-- Note, joinE should not be called with an infinite list of events that all
-- occur at the same time.  It can't decide which occurs first.
joinE :: (Ord t) => EventG t (EventG t a) -> EventG t a
joinE (Event (Future (t0h, e `Stepper` ee'))) =
  assuming (t0h == maxBound) mempty $
  adjustE t0h (e `mappend` joinE ee')

-- TODO: revisit this def.


-- Original Version:
-- joinE (Event (Future (t0h, e `Stepper` ee'))) =
--   adjustE t0h e `mappend` adjustTopE t0h (joinE ee')

adjustTopE :: (Ord t, Bounded t) => Time t -> EventG t t1 -> EventG t t1

-- adjustTopE t0h = (inEvent.inFuture.first) (max t0h)

adjustTopE t0h ~(Event (Future (tah, r))) =
  Event (Future (t0h `max` tah,r))

adjustE :: (Ord t, Bounded t) => Time t -> EventG t t1 -> EventG t t1

adjustE _ e@(Event (Future (Max MaxBound, _))) = e

adjustE t0h (Event (Future (tah, a `Stepper` e))) =
  Event (Future (t1h,a `Stepper` adjustE t1h e))
   where
     t1h = t0h `max` tah

-}

-- The two-caseness of adjustE prevents the any info from coming out until
-- tah is known to be Max or non-Max.  Problem?

-- Is the MaxBound case really necessary?

-- TODO: add adjustE explanation.  What's going on and why t1 in the
-- recursive call?  David's comment:
-- If we have an event [t1, t2] we know t2 >= t1 so (max t t2) == (max (max t t1) t2).
-- See http://hpaste.org/11518 for a def that doesn't change the lower bound.
-- 
-- What I remember is that this function is quite subtle w.r.t laziness.
-- There are some notes in the paper.  If i find instead that a simpler
-- definition is possible, so much the better.

-- Here's an alternative to joinE that is less strict, and doesn't cause
-- reactive to lock up.  Need to verify correctness.  (Does lock up with
-- the mappend optimization that eliminates a space/time leak.)
{-
joinE :: (Ord t, Bounded t) => EventG t (EventG t a) -> EventG t a
joinE (Event (Future (t0h, ~(e `Stepper` ee')))) =
   adjustE t0h (e `mappend` joinE ee')

adjustE t0h (Event (Future (tah, ~(a `Stepper` e)))) =
  Event (Future (t1h,a `Stepper` adjustE t1h e))
  where
    t1h = t0h `max` tah
-}


-- These two joinE defs both lock up in my tests.


instance (Ord t, Bounded t) => MonadPlus (EventG t) where
  { mzero = mempty; mplus = mappend }

-- Standard instance for Applicative w/ join
instance (Ord t, Bounded t) => Monad (ReactiveG t) where
  return  = pure
  r >>= f = joinR (f <$> r)


-- -- Temporary
-- justE :: (Ord t, Bounded t) => EventG t (Maybe a) -> EventG t a
-- justE = joinMaybes

-- filterE :: (Ord t, Bounded t, Show a) => (a -> Bool) -> EventG t a -> EventG t a
-- filterE = filterMP

{-

-- | Pass through the 'Just' occurrences, stripped.  Experimental
-- specialization of 'joinMaybes'.
justE :: (Ord t, Bounded t) => EventG t (Maybe a) -> EventG t a
justE (Event (Future (ta, Just a `Stepper` e'))) =
  Event (Future (ta, a `Stepper` justE e'))
justE (Event (Future (ta, Nothing `Stepper` e'))) =
  adjustE ta (justE e')

-- The adjustE lets consumers know that the resulting event occurs no
-- earlier than ta.

-- | Pass through values satisfying a given predicate.  Experimental
-- specialization of 'filterMP'.
filterE :: (Ord t, Show a) => (a -> Bool) -> EventG t a -> EventG t a

-- filterE p e = joinMaybes (f <$> e)
--  where
--    f a | p a        = Just a
--        | otherwise  = Nothing

filterE _ e@(Event (Future (Max MaxBound, _))) = e

filterE p (Event (Future (ta, a `Stepper` e'))) =
  adjustTopE ta $
    if p a then
      Event (Future (ta, a `Stepper` filterE p e'))
    else filterE p e'
-}

-- The adjustTopE ta guarantees a lower bound even before we've looked at a.

-- filterE p (Event (Future (ta, a `Stepper` e')))
--   | p a       = Event (Future (ta, a `Stepper` filterE p e'))
--   | otherwise = adjustTopE ta (filterE p e')

-- filterE p (Event (Future (ta, a `Stepper` e'))) = h (filterE p e')
--  where  
--    h | p a = -- trace ("pass " ++ show a) $
--             \ e'' -> Event (Future (ta, a `Stepper` e''))
--      | otherwise = -- trace ("skip " ++ show a) $
--                    adjustTopE ta

-- Or maybe move the adjustTopE to the second filterE

-- adjustTopE t0h = (inEvent.inFuture.first) (max t0h)


-- Laziness problem: no information at all can come out of filterE's
-- result until @p a@ is known.

-- filterE p ~(Event (Future (ta, a `Stepper` e'))) =
--   Event (Future (ta', r'))
--  where
--    ta' 
-- 
--   if p a then
--     Event (Future (ta, a `Stepper` filterE p e'))
--   else
--     adjustE ta (filterE p e')


{--------------------------------------------------------------------
    Operations on events and reactive values
--------------------------------------------------------------------}

-- | Reactive value from an initial value and a new-value event.
stepper :: a -> EventG t a -> ReactiveG t a
stepper = Stepper

-- -- | Turn a reactive value into an event, with the initial value
-- -- occurring at -Infinity.
-- --
-- -- Oops: breaks the semantic abstraction of 'Reactive' as a step
-- function.
-- rToE :: (Ord t, Bounded t) => ReactiveG t a -> EventG t a
-- rToE (a `Stepper` e) = pure a `mappend` e

-- | Switch between reactive values.
switcher :: (Ord t, Bounded t) => ReactiveG t a -> EventG t (ReactiveG t a) -> ReactiveG t a
r `switcher` e = join (r `stepper` e)

-- | Reactive 'join' (equivalent to 'join' but slightly more efficient, I think)
joinR :: (Ord t, Bounded t) => ReactiveG t (ReactiveG t a) -> ReactiveG t a

joinR ((a `Stepper` Event ur) `Stepper` e'@(Event urr)) = a `stepper` Event u
 where
   u = ((`switcher` e') <$> ur) `mappend` (join <$> urr)

-- The following simpler definition is wrong.  It keeps listening to @e@
-- even after @er@ has occurred.
-- joinR ((a `Stepper` e) `Stepper` er) = 
--   a `stepper` (e `mappend` join (rToE <$> er))

-- e  :: EventG t a
-- er :: EventG t (ReactiveG t a)
-- 
-- rToE <$> er ::: EventG t (EventG t a)
-- join (rToE <$> er) ::: EventG t a


-- | Access occurrence times in an event.  See also 'withTimeGR'.
withTimeGE :: EventG t a -> EventG t (a, Time t)
withTimeGE = inEvent $ inFuture $ \ (t,r) -> (t, withTimeGR t r)

-- | Access occurrence times in a reactive value.  See also 'withTimeGE'.
withTimeGR :: Time t -> ReactiveG t a -> ReactiveG t (a, Time t)
withTimeGR t (a `Stepper` e) = (a,t) `Stepper` withTimeGE e

-- | Convert a temporally monotonic list of futures to an event.  See also
-- the specialization 'listE'
listEG :: (Ord t, Bounded t) => [(t,a)] -> EventG t a
listEG = futuresE . map (uncurry future)

-- | Convert a temporally monotonic list of futures to an event
futuresE :: (Ord t, Bounded t) => [FutureG t a] -> EventG t a
futuresE [] = mempty
futuresE (Future (t,a) : futs) =
  -- trace ("l2E: "++show t) $
  Event (Future (t, a `stepper` futuresE futs))

-- TODO: redefine 'futuresE' as a fold
-- futuresE = foldr (\ fut e -> Event ((`stepper` e) <$> fut)) mempty

-- TODO: hide futuresE.  currently exported for use in TVal.  If I move to
-- Internal/Reactive, I have to move the monoid instance there, which
-- requires moving others as well.

-- | Convert a temporally monotonic stream of futures to an event.  Like
-- 'futuresE' but it can be lazier, because there's not empty case.
futureStreamE :: (Ord t, Bounded t) => Stream (FutureG t a) -> EventG t a
futureStreamE (~(Cons (Future (t,a)) futs)) =
  Event (Future (t, a `stepper` futureStreamE futs))

-- | Event at given times.  See also 'atTimeG'.
atTimesG :: (Ord t, Bounded t) => [t] -> EventG t ()
atTimesG = listEG . fmap (flip (,) ())

-- | Single-occurrence event at given time.
atTimeG :: (Ord t, Bounded t) => t -> EventG t ()
atTimeG = atTimesG . pure

-- | Snapshot a reactive value whenever an event occurs and apply a
-- combining function to the event and reactive's values.
snapshotWith :: (Ord t, Bounded t) =>
                (a -> b -> c) -> ReactiveG t b -> EventG t a -> EventG t c

-- snapshotWith f e r = joinMaybes $ fmap h (e `snap` r)
--  where
--    h (Nothing,_) = Nothing
--    h (Just a ,b) = Just (f a b)

-- -- This variant of 'snapshot' has 'Nothing's where @b@ changed and @a@
-- -- didn't.
-- snap :: forall a b t. (Ord t, Bounded t) =>
--         ReactiveG t b -> EventG t a -> EventG t (Maybe a, b)
-- (b0 `Stepper` eb) `snap` ea =
--   assuming (isNeverE ea) mempty $
--   (Nothing, b0) `accumE` (fmap fa ea `mappend` fmap fb eb)
--  where
--    fa :: a -> Unop (Maybe a, b)
--    fb :: b -> Unop (Maybe a, b)
--    fa a (_,b) = (Just a , b)
--    fb b _     = (Nothing, b)

-- This next version from Chuan-kai Lin, so that snapshot is lazy enough
-- for recursive cases.  It leaks when the reactive changes faster than
-- the event occurs.

snapshotWith f r e =
    fmap snap $ accumE seed $ fmap advance $ withTimeGE e
        where snap (a, sr)           = f a (rInit sr)
              seed                   = (error "snapshotWith seed", r)
              advance (a, t) (_, sr) = (a, skipRT sr t)

-- | Skip reactive values until the given time.
skipRT :: (Ord t, Bounded t) => ReactiveG t a -> Time t -> ReactiveG t a
r@(_ `Stepper` Event (Future (t, r1))) `skipRT` start =
    if t < start then r1 `skipRT` start else r

-- From Beelsebob:

-- snapshotWith f r e@(Event (Future (t,_ `Stepper` ne))) =
--   Event (Future (t, v' `stepper` snapshotWith f r ne))
--   where
--     Event (Future (_,v' `Stepper` _)) = snapshotWith' f r e
--     snapshotWith' f' r' e' = joinMaybes $ fmap h (r' `snap` e')
--       where
--         h (Nothing,_) = Nothing
--         h (Just a ,b) = Just (f' a b)



-- | Accumulating event, starting from an initial value and a
-- update-function event.  See also 'accumR'.
accumE :: a -> EventG t (a -> a) -> EventG t a
accumE a = inEvent $ fmap $ \ (f `Stepper` e') -> f a `accumR` e'

-- | Reactive value from an initial value and an updater event.  See also
-- 'accumE'.
accumR :: a -> EventG t (a -> a) -> ReactiveG t a
a `accumR` e = a `stepper` (a `accumE` e)

-- | Just the first occurrence of an event.
once :: (Ord t, Bounded t) => EventG t a -> EventG t a
once = (inEvent.fmap) (pure . rInit)

-- | Extract a future representing the first occurrence of the event together
-- with the event of all occurrences after that one.
eventOcc :: (Ord t) => EventG t a -> FutureG t (a, EventG t a)
eventOcc (Event fut)  = (\ (Stepper a e) -> (a,e)) <$> fut


-- | Access the remainder with each event occurrence.
withRestE :: EventG t a -> EventG t (a, EventG t a)
withRestE = (inEvent.fmap) $
	    \ (a `Stepper` e') -> (a,e') `stepper` withRestE e'


-- | Truncate first event at first occurrence of second event.
untilE :: (Ord t, Bounded t) => EventG t a -> EventG t b -> EventG t a
ea `untilE` Event (Future ~(tb,_)) = ea `untilET` tb

-- | Truncate first event at the given time.
untilET :: (Ord t, Bounded t) => EventG t a -> Time t -> EventG t a


-- Event (Future (ta, ~(a `Stepper` e'))) `untilET` t = 
--   if ta < t then
--     Event (Future (ta, a `Stepper` (e' `untilET` t)))
--   else
--     mempty

-- Hm.  I doubt that the definition above gives sufficient temporal
-- laziness.  No information can come out of the result until the value of
-- @ta < t@ is determined, which is usually at about time @ta `min` t@.

-- So, try the following definition instead.  It immediately provides
-- lower bounds of both @ta@ and @t@ as lower bounds of the constructed
-- event occurrences.

Event (Future ~(ta, a `Stepper` e')) `untilET` t = 
  Event (Future (ta', a `Stepper` (e' `untilET` t)))
 where
   ta' = (ta `min` t) `max` (if ta < t then ta else maxBound)

-- I'm not sure about @<@ vs @<=@ above.


-- | Sample a reactive value at a sequence of monotonically non-decreasing
-- times.  Deprecated, because it does not reveal when value is known to
-- be repeated in the output.  Those values won't be recomputed, but they
-- may be re-displayed.
rats :: (Ord t, Bounded t) => ReactiveG t a -> [t] -> [a] -- increasing times

_ `rats` [] = []

r@(a `Stepper` Event (Future (tr',r'))) `rats` ts@(t:ts')
  | ftime t <= tr' = a : r `rats` ts'
  | otherwise      = r' `rats` ts

-- Just for testing
rat :: (Ord t, Bounded t) => ReactiveG t a -> t -> a
rat r = head . rats r . (:[])


{--------------------------------------------------------------------
    Other instances
--------------------------------------------------------------------}

-- Standard instances
instance (Monoid_f f, Ord t, Bounded t) => Monoid_f (ReactiveG t :. f) where
    { mempty_f = O (pure mempty_f); mappend_f = inO2 (liftA2 mappend_f) }
instance (Ord t, Bounded t, Zip f) => Zip (ReactiveG t :. f) where zip = apZip

instance Unzip (ReactiveG t) where {fsts = fmap fst; snds = fmap snd}

-- Standard instances
instance (Ord t, Bounded t) => Monoid_f (EventG t) where
  { mempty_f = mempty ; mappend_f = mappend }
instance (Ord t, Bounded t) => Monoid ((EventG t :. f) a) where
  { mempty = O mempty; mappend = inO2 mappend }
instance (Ord t, Bounded t) => Monoid_f (EventG t :. f) where
  { mempty_f = mempty ; mappend_f = mappend }
instance (Ord t, Bounded t, Cozip f) => Zip (EventG t :. f) where
  zip = cozip

-- Standard instance for functors
instance Unzip (EventG t) where {fsts = fmap fst; snds = fmap snd}


{--------------------------------------------------------------------
    Comonadic stuff
--------------------------------------------------------------------}

instance Copointed (EventG t) where
  -- E a -> F (R a) -> R a -> a
  extract = extract . extract . eFuture

-- Here's the plan for 'duplicate':
-- 
--   E a -> F (R a) -> F (R (R a)) -> F (F (R (R a)))
--       -> F (R (F (R a))) -> E (F (R a)) -> E (E a)


instance Monoid t => Comonad (EventG t) where
  duplicate =
    fmap Event . Event . fmap frTOrf . duplicate . fmap duplicate . eFuture

-- This frTOrf definition type-checks.  Is it what we want?
frTOrf :: FutureG t (ReactiveG t a) -> ReactiveG t (FutureG t a)
frTOrf ~(Future (ta,e)) = (Future . (,) ta) <$> e

-- TODO: Reconsider E = F :. R .  Didn't work with absolute time.  What
-- about relative time?

instance (Ord t, Bounded t) => Pointed (ReactiveG t) where
  point = (`stepper` mempty)

-- TODO: I think we can bypass mempty and so eliminate the Ord
-- constraint.  If so, remove Ord tr from 'time' in Behavior.

instance Copointed (ReactiveG t) where
  -- extract = extract . rat
  -- Semantically: extract == extract . rat == (`rat` mempty) But mempty
  -- is the earliest time (since I'm using the Max monoid *), so here's a
  -- cheap alternative that also doesn't require Ord t:
  extract (a `Stepper` _) = a

-- extract r == extract (rat r) == rat r mempty

-- * Moreover, mempty is the earliest time in the Sum monoid on
-- non-negative values, for relative-time behaviors.

instance Monoid t => Comonad (ReactiveG t) where
  duplicate r@(_ `Stepper` Event u) =
    r `Stepper` Event (duplicate <$> u)

-- TODO: Prove the morphism law:
-- 
--   fmap rat . rat . dup == dup . rat

-- Reactive is like the stream comonad
-- TODO: try again letting events and reactives be streams of futures.


{--------------------------------------------------------------------
    To be moved elsewhere
--------------------------------------------------------------------}

-- | Pass through @Just@ occurrences.
joinMaybes :: MonadPlus m => m (Maybe a) -> m a
joinMaybes = (>>= maybe mzero return)

-- | Pass through values satisfying @p@.
filterMP :: MonadPlus m => (a -> Bool) -> m a -> m a
filterMP p m = joinMaybes (liftM f m)
 where
   f a | p a        = Just a
       | otherwise  = Nothing

-- Alternatively:
-- filterMP p m = m >>= guarded p
--  where
--    guarded p x = guard (p x) >> return x


-- | Apply a given function inside the results of other functions.
-- Equivalent to '(.)', but has a nicer reading when composed
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)


{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

-- TODO: Define more types like ApTy, use in batch below.  Move to checkers.
type ApTy f a b = f (a -> b) -> f a -> f b

batch :: TestBatch
batch = ( "Reactive.PrimReactive"
        , concatMap unbatch
          [ 
          -- monad associativity fails
          -- , monad  (undefined :: EventG NumT (NumT,T,NumT))
            monoid (undefined :: EventG NumT T)
          , monoid (undefined :: ReactiveG NumT [T])
          , monad  (undefined :: ReactiveG NumT (NumT,T,NumT))
--           , ("occurence count",
--              [("joinE", joinEOccuranceCount)]
--             )
          , ("monotonicity",
              [ monotonicity2 "<*>"           
                 ((<*>) :: ApTy (EventG NumT) T T)
{-
              , monotonicity2 "adjustE"       (adjustE
                ::    Time NumT
                   -> EventG NumT NumT
                   -> EventG NumT NumT)
-}
              , monotonicity  "join"          (join
                ::    EventG NumT (EventG NumT T)
                   -> EventG NumT T)
              , monotonicity  "withTimeGE"    (withTimeGE
                ::    EventG NumT T
                   -> EventG NumT (T, Time NumT))
              , monotonicity  "once"          (once
                ::    EventG NumT T
                   -> EventG NumT T)
              , monotonicity2 "accumE"        (accumE
                ::    T
                   -> EventG NumT (T -> T)
                   -> EventG NumT T)
              , monotonicity2 "mappend"       (mappend
                ::    EventG NumT T
                   -> EventG NumT T
                   -> EventG NumT T)
              , monotonicity2 "mplus"         (mplus
                ::    EventG NumT T
                   -> EventG NumT T
                   -> EventG NumT T)
              , monotonicity2 "<|>"           ((<|>)
                ::    EventG NumT T
                   -> EventG NumT T
                   -> EventG NumT T)
              , monotonicity2 "fmap"          (fmap
                ::    (T -> T)
                   -> EventG NumT T
                   -> EventG NumT T)
--              ,monotonicity2 "flip (>>=)"    (flip (>>=))
--              ,monotonicity2 (flip snapshot) "flip snapshot"
              ])
          , ("order preservation",
              [ simulEventOrder  "once"       (once
                ::    EventG NumT NumT
                   -> EventG NumT NumT)
              ])
          ]
        )

monoid_E :: TestBatch
monoid_E = monoid (undefined :: EventG NumT T)


-- joinEOccuranceCount :: Property
-- joinEOccuranceCount =
--   forAll (finiteEvent $ finiteEvent arbitrary
--            :: Gen (EventG NumT (EventG NumT T)))
--          ((==) <$> (sum . map (length . toListE_) . toListE_)
--                <*> (length . toListE_ . joinE))

{-
toListE :: EventG t a -> [FutureG t a]
toListE (Event (Future (Max MaxBound, _             ))) = []
toListE (Event (Future (t0          , v `Stepper` e'))) = Future (t0,v) : toListE e'

toListE_ :: EventG t a -> [a]
toListE_ = map futVal . toListE
-}

monotonicity :: (Show a, Arbitrary a, Arbitrary t
                ,Num t, Ord t, Bounded t, Ord t', Bounded t')
             => String -> (EventG t a -> EventG t' a')
             -> (String,Property)
monotonicity n f = (n, property $ monotoneTest f)

monotonicity2 :: (Show a, Show b, Arbitrary a, Arbitrary b, Arbitrary t
                 ,Num t, Ord t, Bounded t, Ord t', Bounded t')
              => String -> (b -> EventG t a -> EventG t' a')
              -> (String,Property)
monotonicity2 n f = (n, property $ monotoneTest2 f)

monotoneTest :: (Ord t', Bounded t') =>
                (EventG t a -> EventG t' a')
             -> EventG t a
             -> Bool
monotoneTest f e = unsafePerformIO (       (evaluate (isMonotoneE . f $ e))
                                    `race` slowTrue)

monotoneTest2 :: (Show a, Show b, Arbitrary a, Arbitrary b, Arbitrary t
                 ,Num t, Ord t, Bounded t, Ord t', Bounded t')
              => (b -> EventG t a -> EventG t' a')
              -> (b ,  EventG t a) -> Bool
monotoneTest2 f (x,e) =
  unsafePerformIO (       (evaluate (isMonotoneE (x `f` e)))
                   `race` slowTrue)

slowTrue :: IO Bool
slowTrue = do threadDelay 10
              return True

-- TODO: Replace this stuff with a use of delay from Data.Later in checkers.


isMonotoneE :: (Ord t, Bounded t) => EventG t a -> Bool
isMonotoneE = liftA2 (||) isNeverE
                          ((uncurry isMonotoneR') . unFuture . eFuture)

isMonotoneE' :: (Ord t, Bounded t) => (Time t) -> EventG t a -> Bool
isMonotoneE' t =
  liftA2 (||) isNeverE
              ((\(t',r) -> t <= t' && isMonotoneR' t' r) . unFuture . eFuture)

isMonotoneR :: (Ord t, Bounded t) => ReactiveG t a -> Bool
isMonotoneR (_ `Stepper` e) = isMonotoneE e

isMonotoneR' :: (Ord t, Bounded t) => Time t -> ReactiveG t a -> Bool
isMonotoneR' t (_ `Stepper` e) = isMonotoneE' t e

simulEventOrder :: ( Arbitrary t, Num t, Ord t, Bounded t
                   , Arbitrary t', Num t', Ord t', Bounded t'
                   , Num t'', Ord t'', Bounded t''
                   , Num t''', Ord t''', Bounded t''')
                => String -> (EventG t t' -> EventG t'' t''')
                -> (String, Property)
simulEventOrder n f =
  (n,forAll genEvent (isStillOrderedE . f))
  where
    genEvent :: ( Arbitrary t1, Num t1, Ord t1, Bounded t1
                , Arbitrary t2, Num t2, Ord t2, Bounded t2)
             => Gen (EventG t1 t2)
    genEvent = liftA futuresE (liftA2 (zipWith future) nondecreasing
                                                          increasing)
    isStillOrderedE :: ( Num t1, Ord t1, Bounded t1
                       , Num t2, Ord t2, Bounded t2) => EventG t1 t2 -> Bool
    isStillOrderedE =
      liftA2 (||) isNeverE
                  (isStillOrderedR . futVal . eFuture)
    
    isStillOrderedR (a `Stepper` e) =
      isStillOrderedE' a e
    
    isStillOrderedE' a =
      liftA2 (||) isNeverE
                  (isStillOrderedR' a . futVal . eFuture)
    
    isStillOrderedR' a (b `Stepper` e) =
      a < b && isStillOrderedE' b e

-- An infinite event.  handy for testing.
infE :: EventG NumT NumT
infE = futuresE (zipWith future [1..] [1..]) 

