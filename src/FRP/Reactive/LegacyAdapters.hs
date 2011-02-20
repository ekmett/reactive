{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.LegacyAdapters
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Tools for making Reactive adapters for imperative (\"legacy\")
-- libraries.
----------------------------------------------------------------------

module FRP.Reactive.LegacyAdapters
  ( Sink, Action
  , Clock, makeClock, cGetTime
  , adaptE, mkUpdater
  , module FRP.Reactive.Internal.TVal
  ) where

import FRP.Reactive.Internal.Misc     (Sink,Action)
import FRP.Reactive.Internal.Clock    (Clock,makeClock,cGetTime)
import FRP.Reactive.Internal.TVal
import FRP.Reactive.Internal.Timing   (adaptE,mkUpdater)

