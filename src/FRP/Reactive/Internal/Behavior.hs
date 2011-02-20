{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving
           , FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Behavior
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Representation of reactive behaviors
----------------------------------------------------------------------

module FRP.Reactive.Internal.Behavior (BehaviorG(..), beh, unb) where

import Prelude hiding (zip,unzip)

import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(pure),liftA2)

-- TypeCompose
import Control.Compose ((:.)(..),unO)
import Data.Zip (Zip(..),Unzip(..))

import qualified FRP.Reactive.Reactive as R
-- import FRP.Reactive.Reactive (TimeT)
import FRP.Reactive.Fun


-- Reactive behaviors.  Simply a reactive 'Fun'ction value.  Wrapped in
-- a type composition to get 'Functor' and 'Applicative' for free.

-- | Reactive behaviors.  They can be understood in terms of a simple
-- model (denotational semantics) as functions of time, namely @at ::
-- BehaviorG t a -> (t -> a)@.
-- 
-- The semantics of 'BehaviorG' instances are given by corresponding
-- instances for the semantic model (functions).  See
-- <http://conal.net/blog/posts/simplifying-semantics-with-type-class-morphisms/>.
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
newtype BehaviorG tr tf a = Beh { unBeh :: (R.ReactiveG tr :. Fun tf) a }
   deriving (Monoid,Functor,Applicative)

-- Standard Monoid instance for Applicative applied to Monoid.  Used by
-- @deriving Monoid@ above.
instance (Applicative (R.ReactiveG tr :. Fun tf), Monoid a)
      => Monoid ((R.ReactiveG tr :. Fun tf) a) where
  { mempty = pure mempty; mappend = liftA2 mappend }

-- Standard 'Zip' for an 'Applicative'
instance (Ord tr, Bounded tr) => Zip (BehaviorG tr tf) where zip = liftA2 (,)

-- Standard 'Unzip' for a 'Functor'
instance Unzip (BehaviorG tr tf) where {fsts = fmap fst; snds = fmap snd}

-- | Wrap a reactive time fun as a behavior.
beh :: R.ReactiveG tr (Fun tf a) -> BehaviorG tr tf a
beh = Beh . O

-- | Unwrap a behavior.
unb :: BehaviorG tr tf a -> R.ReactiveG tr (Fun tf a)
unb = unO . unBeh
