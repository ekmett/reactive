{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Test.TestReactive
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gather up QuickCheck tests for Reactive
----------------------------------------------------------------------

module Test.Reactive (batches,main) where

-- import Test.QuickCheck

import Test.QuickCheck.Checkers

-- import qualified Data.Unamb

import qualified FRP.Reactive.Future
import qualified FRP.Reactive.PrimReactive
import qualified FRP.Reactive.Reactive
import qualified FRP.Reactive.Fun

batches :: [TestBatch]
batches = [ FRP.Reactive.Future.batch
          , FRP.Reactive.PrimReactive.batch
          , FRP.Reactive.Reactive.batch
          , FRP.Reactive.Fun.batch
          ]

main :: IO ()
main = mapM_ quickBatch batches
