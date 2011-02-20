{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.Sorted
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Sorted lists: experimental (unused)
----------------------------------------------------------------------

module Reactive.Sorted where

import Data.Monoid
import Data.List (sort)
import Control.Applicative
import Control.Monad

newtype Sorted a = Sort { unSort :: [a] } -- non-decreasing values

-- | Apply a unary function within the event representation.
inSort :: ([a] -> [b]) -> (Sorted a -> Sorted b)
inSort f = Sort . f . unSort

-- | Apply a binary function within the event representation.
inSort2 :: ([a] -> [b] -> [c]) -> (Sorted a -> Sorted b -> Sorted c)
inSort2 f = inSort . f . unSort


instance Ord a => Monoid (Sorted a) where
  mempty  = Sort []
  mappend = inSort2 merge

-- | Merge two ordered lists into an ordered list.
merge :: Ord a => [a] -> [a] -> [a]
[]         `merge` vs         = vs
us         `merge` []         = us
us@(u:us') `merge` vs@(v:vs') =
  (u `min` v) : if u <= v then us' `merge` vs else us `merge` vs'

-- Alternatively,
-- 
--   us@(u:us') `merge` vs@(v:vs') =
--     if u <= v then
--       u : (us' `merge` vs )
--     else
--       v : (us  `merge` vs')
-- 
-- The definition used instead is more productive.  It produces a cons
-- cell immediately and can even produce partial information about @u
-- `min` v@ before it's known which is smaller.

class FunctorOrd h where
  fmapO :: (Ord a, Ord b) => (a -> b) -> h a -> h b

class FunctorOrd h => ApplicativeOrd h where
  pureO  :: Ord a => a -> h a
  (<*?>) :: (Ord a, Ord b) => h (a -> b) -> h a -> h b

class MonadOrd h where
  returnO :: Ord a => a -> h a
  -- does joinO need Ord (h a) ?
  joinO :: Ord a => h (h a) -> h a

instance FunctorOrd Sorted where
  fmapO f = inSort (sort . fmap f)

instance ApplicativeOrd Sorted where
  pureO a = Sort (pure a)
  (<*?>)  = inSort2 $ (fmap.fmap) sort (<*>)

instance MonadOrd Sorted where
  returnO = pureO
  joinO = inSort $ sort . join . fmap unSort 
