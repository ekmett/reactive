{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- A library for programming with functional reactive behaviors.
----------------------------------------------------------------------

module FRP.Reactive
  (
    -- * Events
    TimeT, ITime
  , EventG, Event
  , accumE
  , withTimeE, withTimeE_
  , zipE, scanlE, monoidE
  , mealy, mealy_, countE, countE_, diffE
  , withPrevE, withPrevEWith
  , eitherE
  , justE, filterE
    -- ** More esoteric
  , listE, atTimes, atTime, once
  , firstRestE, firstE, restE, snapRemainderE
  , withRestE, untilE
  , splitE, switchE
    -- ** Useful with events.
  , joinMaybes, filterMP
    -- * Behaviors
  , BehaviorG, Behavior, Behaviour
  , time
  , stepper, switcher --, select
  , snapshotWith, snapshot, snapshot_, whenE
  , accumB
  , scanlB, monoidB, maybeB, flipFlop, countB
  , sumB, integral
  ) where

-- Reactive.Reactive exports reactive values as well.  Filter them out.

import FRP.Reactive.Reactive hiding
  (stepper,switcher,snapshotWith,snapshot,snapshot_,whenE,flipFlop,integral)
import FRP.Reactive.Behavior
import FRP.Reactive.VectorSpace ()
import FRP.Reactive.Num ()
