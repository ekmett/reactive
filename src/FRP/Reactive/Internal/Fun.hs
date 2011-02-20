{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.Fun
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Constant-optimized representation of functions.
----------------------------------------------------------------------

module FRP.Reactive.Internal.Fun (Fun(..)) where

-- | Constant-optimized functions
data Fun t a = K a                      -- ^ constant function
             | Fun (t -> a)             -- ^ non-constant function
