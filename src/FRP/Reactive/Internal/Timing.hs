{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Timing
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 
----------------------------------------------------------------------

module FRP.Reactive.Internal.Timing
  (adaptE,mkUpdater,sleepPast)
  where

import Data.Monoid (mempty)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Concurrent.SampleVar

-- For IO monoid
import Control.Instances ()

import Data.AddBounds

import FRP.Reactive.Reactive (exactNB,TimeT,Event)
import FRP.Reactive.Improving (Improving,exact)
import FRP.Reactive.Behavior (Behavior)

import FRP.Reactive.Internal.Misc (Action,Sink)
import FRP.Reactive.Internal.Reactive (forkR,runE)
import FRP.Reactive.Internal.Behavior (unb)
import FRP.Reactive.Internal.Fun
import FRP.Reactive.Internal.Clock (makeClock,cGetTime)



-- | Execute an action-valued event.
adaptE :: Sink (Event Action)
adaptE e = do clock <- makeClock
              runE (sleepPast (cGetTime clock) . exactNB) e


-- | If a sample variable is full, act on the contents, leaving it empty.
drainS :: SampleVar a -> Sink (Sink a)
drainS sv snk = do emptySVar <- isEmptySampleVar sv
                   unless emptySVar (readSampleVar sv >>= snk)

-- TODO: Generalize from TimeT below, using BehaviorG.

noSink :: Sink t
noSink = mempty -- const (putStrLn "noSink")

-- | Make an action to be executed regularly, given a time-source and a
-- action-behavior.  The generated action is optimized to do almost no
-- work during known-constant phases of the given behavior.
mkUpdater :: IO TimeT -> Behavior Action -> IO Action
mkUpdater getT acts =
  -- The plan: Stash new phases (time functions) in a sample variable as
  -- they arise.  Every minPeriod, check the sample var for a new value.
  do actSVar <- newEmptySampleVar
     _       <- forkR (sleepPast' getT . exact)
                      (writeSampleVar' actSVar <$> unb acts) 
     tfunRef <- newIORef (noSink :: Sink TimeT)
     return $
       do -- When there's a new time fun, execute it once if
          -- constant, or remember for repeated execution if
          -- non-constant.
          now <- getT
          -- putStrLn ("scheduler: time == " ++ show now)
          drainS actSVar $ \ actF ->
            case actF of 
              K   c -> do -- putStrLn "K"
                          writeIORef tfunRef noSink >> c
              Fun f -> do -- putStrLn "Fun"
                          writeIORef tfunRef f
          readIORef tfunRef >>= ($ now)
          -- yield  -- experiment
 where
   writeSampleVar' v x = do -- putStrLn "writeSampleVar"
                            writeSampleVar v x

-- | Pause a thread for the given duration in seconds
sleep :: Sink TimeT
sleep = threadDelay . ceiling . (1.0e6 *)

-- sleep = threadDelay . ceiling . (1.0e6 *)

-- | Sleep past a given time
sleepPast :: IO TimeT -> Sink TimeT
sleepPast getT !target = 
   -- Snooze until strictly after the target.
   do -- The strict evaluation of target is essential here.
      -- (See bang pattern.)  Otherwise, the next line will grab a
      -- time before a possibly long block, and then sleep much
      -- longer than necessary.
      now <- getT
--       putStrLn $ "sleepPast: now == " ++ show now
--                  ++ ", target == " ++ show target
      unless (now > target) $
         sleep (target-now) -- >> loop

-- | Variant of 'sleepPast', taking a possibly-infinite time
sleepPast' :: IO TimeT -> Sink (AddBounds TimeT)
sleepPast' _     MinBound        = return ()
sleepPast' getT (NoBound target) = sleepPast getT target
sleepPast' _ MaxBound            = error "sleepPast MaxBound.  Expected??"
