{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.PairMonad
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Writer monad as a pair.  Until it's in Control.Monad.Instances.
-- 
-- Use @import Data.PairMonad ()@
----------------------------------------------------------------------

module Data.PairMonad () where

import Data.Monoid
import Control.Applicative


-- Orphan instance:

-- Equivalent to the Monad Writer instance.
instance Monoid o => Monad ((,) o) where
  return      = pure
  (o,a) >>= f = (o `mappend` o', a') where (o',a') = f a

-- Alternatively,
--   m >>= f = join (fmap f m)
--    where
--      join ((o, (o',a))) = (o `mappend` o', a)
-- Or even,
--   (o,a) >>= f = (o,id) <*> f a
-- 
-- I prefer the join version, because it's the standard (>>=)-via-join,
-- plus a very simple definition for join.  Too bad join isn't a method of
-- Monad, with (>>=) and join defined in terms of each other.  Why isn't
-- it?  Probably because Monad isn't derived from Functor.  Was that an
-- oversight?
