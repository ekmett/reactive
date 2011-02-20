{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Future
-- Copyright   :  (c) Conal Elliott 2007-2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- A simple formulation of functional /futures/, roughly as
-- described at <http://en.wikipedia.org/wiki/Futures_and_promises>.
-- 
-- A /future/ is a value with an associated time of /arrival/.  Typically,
-- neither the time nor the value can be known until the arrival time.
-- 
-- Primitive futures can be things like /the value of the next key you
-- press/, or /the value of LambdaPix stock at noon next Monday/.
-- 
-- Composition is via standard type classes: 'Functor', 'Applicative',
-- 'Monad', and 'Monoid'.  Some comments on the 'Future' instances of
-- these classes:
-- 
-- * Monoid: 'mempty' is a future that never arrives (infinite time and
--   undefined value), and @a `mappend` b@ is the earlier of @a@ and @b@,
--   preferring @a@ when simultaneous.
-- 
-- * 'Functor': apply a function to a future argument.  The (future)
-- result arrives simultaneously with the argument.
-- 
-- * 'Applicative': 'pure' gives value arriving negative infinity.
-- '(\<*\>)' applies a future function to a future argument, yielding a
-- future result that arrives once /both/ function and argument have
-- arrived (coinciding with the later of the two times).
-- 
-- * 'Monad': 'return' is the same as 'pure' (as usual).  @(>>=)@ cascades
-- futures.  'join' resolves a future future value into a future value.
-- 
-- Futures are parametric over /time/ as well as /value/ types.  The time
-- parameter can be any ordered type and is particularly useful with time
-- types that have rich partial information structure, such as /improving
-- values/.
----------------------------------------------------------------------

module FRP.Reactive.Future
  (
    -- * Time & futures
    Time, ftime
  , FutureG(..), isNeverF, inFuture, inFuture2, futTime, futVal, future
  , withTimeF
  -- * Tests
  , batch
  ) where

import Data.Monoid (Monoid(..))

import Data.Max
-- import Data.AddBounds
import FRP.Reactive.Internal.Future

-- Testing
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{----------------------------------------------------------
    Time and futures
----------------------------------------------------------}

-- | Make a finite time
ftime :: t -> Time t
ftime = Max

-- FutureG representation in Internal.Future

instance (Bounded t, Eq t, EqProp t, EqProp a) => EqProp (FutureG t a) where
  u =-= v | isNeverF u && isNeverF v = property True
  Future a =-= Future b = a =-= b

-- I'd rather say:
-- 
-- instance (Bounded t, EqProp t, EqProp a) => EqProp (FutureG t a) where
--   Future a =-= Future b =
--     (fst a =-= maxBound && fst b =-= maxBound) .|. a =-= b
-- 
-- However, I don't know how to define disjunction on QuickCheck properties.

-- | A future's time
futTime :: FutureG t a -> Time t
futTime = fst . unFuture

-- | A future's value
futVal :: FutureG t a -> a
futVal = snd . unFuture

-- | A future value with given time & value
future :: t -> a -> FutureG t a
future t a = Future (ftime t, a)

-- | Access time of future
withTimeF :: FutureG t a -> FutureG t (Time t, a)
withTimeF = inFuture $ \ (t,a) -> (t,(t,a))

-- withTimeF = inFuture duplicate (with Comonad)

-- TODO: Eliminate this Monoid instance.  Derive Monoid along with all the
-- other classes.  And don't use mempty and mappend for the operations
-- below.  For one thing, the current instance makes Future a monoid but
-- unFuture not be a monoid morphism.

instance (Ord t, Bounded t) => Monoid (FutureG t a) where
  mempty = Future (maxBound, error "Future mempty: it'll never happen, buddy")
  -- Pick the earlier future.
  Future (s,a) `mappend` Future (t,b) =
    Future (s `min` t, if s <= t then a else b)

-- Consider the following simpler definition:
-- 
--   fa@(Future (s,_)) `mappend` fb@(Future (t,_)) =
--     if s <= t then fa else fb
-- 
-- Nothing can be known about the resulting future until @s <= t@ is
-- determined.  In particular, we cannot know lower bounds for the time.
-- In contrast, the actual 'mappend' definition can potentially yield
-- useful partial information, such as lower bounds, about the future
-- time, if the type parameter @t@ has rich partial information structure
-- (non-flat).

-- For some choices of @t@, there may be an efficient combination of 'min'
-- and '(<=)', so the 'mappend' definition is sub-optimal.  In particular,
-- 'Improving' has 'minI'.


-- -- A future known never to happen (by construction), i.e., infinite time.
-- isNever :: FutureG t a -> Bool
-- isNever = isMaxBound . futTime
--  where
--    isMaxBound (Max MaxBound) = True
--    isMaxBound _              = False
-- 
-- This function is an abstraction leak.  Don't export it to library
-- users.



{----------------------------------------------------------
    Tests
----------------------------------------------------------}

-- Represents times at a given instant.
newtype TimeInfo t = TimeInfo (Maybe t)
  deriving EqProp

instance Bounded t => Bounded (TimeInfo t) where
  minBound = TimeInfo (Just minBound)
  maxBound = TimeInfo Nothing


-- A time at a given instant can be some unknown time in the future
unknownTimeInFuture :: TimeInfo a
unknownTimeInFuture = TimeInfo Nothing

-- or, a known time in the past. We're ignoring known future times for now.
knownTimeInPast :: a -> TimeInfo a
knownTimeInPast = TimeInfo . Just

instance Eq a => Eq (TimeInfo a) where
  TimeInfo Nothing == TimeInfo Nothing = error "Cannot tell if two unknown times in the future are equal"
  TimeInfo (Just _) == TimeInfo Nothing = False
  TimeInfo Nothing == TimeInfo (Just _) = False
  TimeInfo (Just a) == TimeInfo (Just b) = a == b

instance Ord a => Ord (TimeInfo a) where
  -- The minimum of two unknown times in the future is an unkown time in the
  -- future.
  TimeInfo Nothing `min` TimeInfo Nothing = unknownTimeInFuture
  TimeInfo Nothing `min` b = b
  a `min` TimeInfo Nothing = a
  TimeInfo (Just a) `min` TimeInfo (Just b) = (TimeInfo . Just) (a `min` b)
  
  TimeInfo Nothing <= TimeInfo Nothing = error "Cannot tell if one unknown time in the future is less than another."
  TimeInfo Nothing <= TimeInfo (Just _) = False
  TimeInfo (Just _) <= TimeInfo Nothing = True
  TimeInfo (Just a) <= TimeInfo (Just b) = a <= b

batch :: TestBatch
batch = ( "FRP.Reactive.Future"
        , concatMap unbatch
            [ monoid (undefined :: FutureG NumT T)
            , functorMonoid (undefined :: FutureG NumT
                                                  (T,NumT))
            -- Checking the semantics here isn't necessary because
            -- the implementation is identical to them.
            --
            -- Also, Functor, Applicative, and Monad don't require checking
            -- since they are automatically derived.
            --
            -- , semanticMonoid' (undefined :: FutureG NumT T)
            -- , functor (undefined :: FutureG NumT (T,NumT,T))
            -- , semanticFunctor (undefined :: FutureG NumT ())
            -- , applicative (undefined :: FutureG NumT (NumT,T,NumT))
            -- , semanticApplicative (undefined :: FutureG NumT ())
            -- , monad (undefined :: FutureG NumT (NumT,T,NumT))
            -- , semanticMonad (undefined :: FutureG NumT ())

            , ("specifics",
                [ ("laziness", property laziness )
                       ])
            ]
        )
 where
   laziness :: BoundedT -> T -> Property
   laziness t a = (uf `mappend` uf) `mappend` kf  =-= kf
      where
        uf = unknownFuture
        kf = knownFuture
        knownFuture = future (knownTimeInPast t) a
        unknownFuture = future unknownTimeInFuture (error "cannot retrieve value at unknown time at the future")


-- Move to checkers
type BoundedT = Int
