{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Clock
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Serializing clocks
-- 
-- Thanks to Luke Palmer for help with this module.
----------------------------------------------------------------------

module FRP.Reactive.Internal.Clock
  (Clock(..), makeClock) where

import Control.Applicative (liftA2)
import System.Time

import FRP.Reactive.Reactive (TimeT)
-- import FRP.Reactive.Internal.Misc (Sink)
import FRP.Reactive.Internal.Serial


-- | Waits a specified duration and then execute an action
-- type Delay t = t -> forall a. IO a -> IO a

-- | Waits until just after a specified time and then execute an action,
-- passing in the actual time.
-- type Schedule t = t -> Sink (Sink t)

-- | A serializing clock.  Can (a) produce a time and (b) serialize an
-- action.
data Clock t = Clock { cGetTime   :: IO t
                     , cSerialize :: Serial
                     }

-- | Make a clock
makeClock :: IO (Clock TimeT)
makeClock = liftA2 clock getClockTime makeSerial
 where
   clock :: ClockTime -> Serial -> Clock TimeT
   clock refTime serial =
     Clock (currRelTime refTime) serial


-- TODO: How can I know that actions are carried out monotonically?

-- | Get the current time in seconds, relative to a start 'ClockTime'.
currRelTime :: ClockTime -> IO TimeT
currRelTime (TOD sec0 pico0) = fmap delta getClockTime
 where
   delta (TOD sec pico) =
     fromIntegral (sec-sec0) + 1.0e-12 * fromIntegral (pico-pico0)
