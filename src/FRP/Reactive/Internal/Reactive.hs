{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Reactive
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Representation for 'Reactive' and 'Event' types.  Combined here,
-- because they're mutually recursive.
-- 
-- The representation used in this module is based on a close connection
-- between these two types.  A reactive value is defined by an initial
-- value and an event that yields future values; while an event is given
-- as a future reactive value.
----------------------------------------------------------------------

module FRP.Reactive.Internal.Reactive
  (
    EventG(..), isNeverE, inEvent, inEvent2, eFutures
  , ReactiveG(..), inREvent, inFutR
  , runE, runR, forkE, forkR
  ) where

-- import Data.List (intersperse)

import Control.Concurrent (forkIO,ThreadId)

import FRP.Reactive.Internal.Misc
import FRP.Reactive.Internal.Future
import Data.Max
-- import Data.AddBounds

-- | Events.  Semantically: time-ordered list of future values.
-- Instances: 
-- 
-- * 'Monoid': 'mempty' is the event that never occurs, and @e `mappend`
--   e'@ is the event that combines occurrences from @e@ and @e'@.
-- 
-- * 'Functor': @fmap f e@ is the event that occurs whenever @e@ occurs,
--   and whose occurrence values come from applying @f@ to the values from
--   @e@.
-- 
-- * 'Applicative': @pure a@ is an event with a single occurrence at time
--   -Infinity.  @ef \<*\> ex@ is an event whose occurrences are made from
--   the /product/ of the occurrences of @ef@ and @ex@.  For every occurrence
--   @f@ at time @tf@ of @ef@ and occurrence @x@ at time @tx@ of @ex@, @ef
--   \<*\> ex@ has an occurrence @f x@ at time @tf `max` tx@.  N.B.: I
--   don't expect this instance to be very useful.  If @ef@ has @nf@
--   instances and @ex@ has @nx@ instances, then @ef \<*\> ex@ has @nf*nx@
--   instances.  However, there are only @nf+nx@ possibilities for @tf
--   `max` tx@, so many of the occurrences are simultaneous.  If you think
--   you want to use this instance, consider using 'Reactive' instead.
-- 
-- * 'Monad': @return a@ is the same as @pure a@ (as usual).  In @e >>= f@,
--   each occurrence of @e@ leads, through @f@, to a new event.  Similarly
--   for @join ee@, which is somehow simpler for me to think about.  The
--   occurrences of @e >>= f@ (or @join ee@) correspond to the union of the
--   occurrences (temporal interleaving) of all such events.  For example,
--   suppose we're playing Asteroids and tracking collisions.  Each collision
--   can break an asteroid into more of them, each of which has to be tracked
--   for more collisions.  Another example: A chat room has an /enter/ event,
--   whose occurrences contain new events like /speak/.  An especially useful
--   monad-based function is 'joinMaybes', which filters a Maybe-valued
--   event.

newtype EventG t a = Event { eFuture :: FutureG t (ReactiveG t a) }

-- The event representation requires temporal monotonicity but does not
-- enforce it, which invites bugs.  Every operation therefore must be
-- tested for preserving monotonicity.  (Better yet, find an efficient
-- representation that either enforces or doesn't require monotonicity.)

-- Why the newtype for 'EventG?'  Because the 'Monoid' instance of 'Future'
-- does not do what I want for 'EventG'.  It will pick just the
-- earlier-occurring event, while I want an interleaving of occurrences
-- from each.  Similarly for other classes.


-- TODO: Alternative and MonadPlus instances for EventG

-- | Reactive value: a discretely changing value.  Reactive values can be
-- understood in terms of (a) a simple denotational semantics of reactive
-- values as functions of time, and (b) the corresponding instances for
-- functions.  The semantics is given by the function @at :: ReactiveG t a ->
-- (t -> a)@.  A reactive value may also be thought of (and in this module
-- is implemented as) a current value and an event (stream of future values).
-- 
-- The semantics of 'ReactiveG' instances are given by corresponding
-- instances for the semantic model (functions):
-- 
-- * 'Functor': @at (fmap f r) == fmap f (at r)@, i.e., @fmap f r `at`
--   t == f (r `at` t)@.
-- 
-- * 'Applicative': @at (pure a) == pure a@, and @at (s \<*\> r) == at s
--   \<*\> at t@.  That is, @pure a `at` t == a@, and @(s \<*\> r) `at` t
--   == (s `at` t) (r `at` t)@.
-- 
-- * 'Monad': @at (return a) == return a@, and @at (join rr) == join (at
--   . at rr)@.  That is, @return a `at` t == a@, and @join rr `at` t ==
--   (rr `at` t) `at` t@.  As always, @(r >>= f) == join (fmap f r)@.
--   @at (r >>= f) == at r >>= at . f@.
-- 
-- * 'Monoid': a typical lifted monoid.  If @o@ is a monoid, then
--   @Reactive o@ is a monoid, with @mempty == pure mempty@, and @mappend
--   == liftA2 mappend@.  That is, @mempty `at` t == mempty@, and @(r
--   `mappend` s) `at` t == (r `at` t) `mappend` (s `at` t).@

data ReactiveG t a = a `Stepper` EventG t a


{--------------------------------------------------------------------
    Applying functions inside of representations
--------------------------------------------------------------------}

-- | Apply a unary function inside an 'EventG' representation.
inEvent :: (FutureG s (ReactiveG s a) -> FutureG t (ReactiveG t b))
        -> (EventG s a -> EventG t b)
inEvent f = Event . f . eFuture

-- | Apply a binary function inside an 'EventG' representation.
inEvent2 :: (FutureG t (ReactiveG t a) -> FutureG t (ReactiveG t b)
                                       -> FutureG t (ReactiveG t c))
         -> (EventG t a -> EventG t b -> EventG t c)
inEvent2 f = inEvent . f . eFuture

-- | Apply a unary function inside the 'rEvent' part of a 'Reactive'
-- representation.
inREvent :: (EventG    s a -> EventG    t a)
         -> (ReactiveG s a -> ReactiveG t a)
inREvent f ~(a `Stepper` e) = a `Stepper` f e

-- | Apply a unary function inside the future reactive inside a 'Reactive'
-- representation.
inFutR :: (FutureG s (ReactiveG s b) -> FutureG t (ReactiveG t b))
       -> (ReactiveG s b -> ReactiveG t b)
inFutR = inREvent . inEvent


{--------------------------------------------------------------------
    Showing values (exposing rep)
--------------------------------------------------------------------}

isNeverE :: (Bounded t, Eq t) => EventG t a -> Bool
isNeverE = isNeverF . eFuture

-- | Make the event into a list of futures
eFutures :: (Bounded t, Eq t) => EventG t a -> [FutureG t a]
eFutures e | isNeverE e = []
eFutures (Event (Future (t,a `Stepper` e))) = Future (t,a) : eFutures e

-- TODO: redefine 'eFutures' as an unfold

-- TODO: does this isNeverE interfere with laziness?  Does it need an unamb?

-- Show a future
sFuture :: (Show t, Show a) => FutureG t a -> String
sFuture = show . unFuture

-- sFuture (Future (Max MinBound,a)) = "(-infty," ++ show a ++ ")"
-- sFuture (Future (Max MaxBound,_)) = "(infty,_)"
-- sFuture (Future (Max (NoBound t),a)) = "(" ++ show t ++ "," ++ show a ++ ")"

-- TODO: Better re-use in sFuture.

-- Truncated show
sFutures :: (Show t, Show a) => [FutureG t a] -> String

-- sFutures = show

-- This next implementation blocks all output until far future occurrences
-- are detected, which causes problems for debugging.  I like the "...",
-- so look for another implementation.

-- sFutures fs =
--   let maxleng = 20
--       a   = (intersperse "->" . map sFuture) fs
--       inf = length (take maxleng a) == maxleng
--   in
--     if not inf then concat a
--                else concat (take maxleng a) ++ "..."

-- This version uses a lazier intersperse
-- sFutures = take 100 . concat . intersperse' "->" . map sFuture

-- The following version adds "..." in case of truncation.

sFutures fs = leading early ++ trailing late
 where
  (early,late) = splitAt 20 fs
  leading  = concat . intersperse' "->" . map sFuture
  trailing [] = ""
  trailing _  = "-> ..."
   

-- TODO: clean up sFutures def: use intercalate, concat before trimming,
-- and define&use a general function for truncating and adding "...".
-- Test.

instance (Eq t, Bounded t, Show t, Show a) => Show (EventG t a) where
  show = ("Event: " ++) . sFutures . eFutures

instance (Eq t, Bounded t, Show t, Show a) => Show (ReactiveG t a) where
  show (x `Stepper` e) = show x ++ " `Stepper` " ++ show e


{--------------------------------------------------------------------
    Execution
--------------------------------------------------------------------}

-- | Run an event in the current thread.  Use the given time sink to sync
-- time, i.e., to wait for an output time before performing the action.
runE :: forall t. (Ord t, Bounded t) => Sink t -> Sink (EventG t Action)
runE sync ~(Event (Future (Max t,r)))
  | t == maxBound = return () -- finished!
  | otherwise     = sync t >> runR sync r

-- In most cases, the value of t won't be known ahead of time, so just
-- evaluating t will do the necessary waiting.


-- | Run an event in a new thread, using the given time sink to sync time.
forkE :: (Ord t, Bounded t) => Sink t -> EventG t Action -> IO ThreadId
forkE = (fmap.fmap) forkIO runE

-- TODO: Revisit this tsync definition.  For instance, maybe the MaxBound
-- case ought to simply return.

-- | Run a reactive value in the current thread, using the given time sink
-- to sync time.
runR :: (Bounded t, Ord t) => Sink t -> Sink (ReactiveG t Action)
runR sync (act `Stepper` e) = act >> runE sync e
                      
-- | Run a reactive value in a new thread, using the given time sink to
-- sync time.  The initial action happens in the current thread.
forkR :: (Ord t, Bounded t) => Sink t -> ReactiveG t Action -> IO ThreadId
forkR = (fmap.fmap) forkIO runR

-----

-- intersperse             :: a -> [a] -> [a]
-- intersperse _   []      = []
-- intersperse _   [x]     = [x]
-- intersperse sep (x:xs)  = x : sep : intersperse sep xs

-- Lazier intersperse

intersperse'             :: a -> [a] -> [a]
intersperse' _   []      = []
intersperse' sep (x:xs)  = x : continue xs
 where
   continue [] = []
   continue xs' = sep : intersperse' sep xs'

