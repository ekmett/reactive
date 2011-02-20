{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Future
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Representation of future values
----------------------------------------------------------------------

module FRP.Reactive.Internal.Future
  (
    -- * Time & futures
    Time
  , FutureG(..), isNeverF, inFuture, inFuture2
  , runF
  ) where


import Control.Applicative (Applicative(..))

import Control.Comonad (Copointed,Comonad)

import Test.QuickCheck

import FRP.Reactive.Internal.Misc (Sink)
import Data.Max
import Data.PairMonad ()


-- | Time used in futures.  The parameter @t@ can be any @Ord@ and
-- @Bounded@ type.  Pure values have time 'minBound', while
-- never-occurring futures have time 'maxBound.'
-- type Time t = Max (AddBounds t)

type Time = Max


-- | A future value of type @a@ with time type @t@.  Simply a
-- time\/value pair.  Particularly useful with time types that have
-- non-flat structure.
newtype FutureG t a = Future { unFuture :: (Time t, a) }
  deriving (Functor, Applicative, Monad, Copointed, Comonad {-, Show-}
           , Arbitrary, CoArbitrary)

isNeverF :: (Bounded t, Eq t) => FutureG t t1 -> Bool
isNeverF (Future (t,_)) = t == maxBound

instance (Eq t, Eq a, Bounded t) => Eq (FutureG t a) where
  Future a == Future b =
    (fst a == maxBound && fst b == maxBound) || a == b

-- When I drop @AddBounds@, I use @maxBound@ as infinity/never.  I'm
-- uncomfortable with this choice, however.  Consider a small type like
-- @Bool@ for @t@.


instance (Show t, Show a, Eq t, Bounded t) => Show (FutureG t a) where
--   show (Future (Max t, a)) | t == maxBound = "<never>"
--                            | otherwise     = "<" ++ show t ++ "," ++ show a ++ ">"
  show u | isNeverF u = "<never>"
  show (Future (Max t, a)) = "<" ++ show t ++ "," ++ show a ++ ">"

--  The 'Applicative' and 'Monad' instances rely on the 'Monoid' instance
-- of 'Max'.


-- | Apply a unary function within the 'FutureG' representation.
inFuture :: ((Time t, a) -> (Time t', b))
         -> FutureG t a  -> FutureG t' b
inFuture f = Future . f . unFuture

-- | Apply a binary function within the 'FutureG' representation.
inFuture2 :: ((Time t, a) -> (Time t', b) -> (Time t', c))
          ->  FutureG t a -> FutureG t' b -> FutureG t' c
inFuture2 f =  inFuture . f . unFuture


-- | Run a future in the current thread.  Use the given time sink to sync
-- time, i.e., to wait for an output time before performing the action.
runF :: Ord t => Sink t -> FutureG t (IO a) -> IO a
runF sync (Future (Max t,io)) = sync t >> io
