{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.SImproving
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- \"Improving values\" from Warren Burton's \"Encapsulating Nondeterminacy
-- in an Abstract Data Type with Deterministic Semantics\".
-- 
-- This implementation is simple but not efficient, as it accumulates lots
-- of lower bounds.
----------------------------------------------------------------------

module Reactive.SImproving
  (
    Improving(..), exactly, exact, improveMbs
  -- * Misc speculation tools
  , spec, specNY, specYY, start
  ) where

import Data.Function (on)
-- import Debug.Trace

import Control.Parallel (par)

-- | Progressive information about a value (e.g., a time).  Represented as
-- a non-empty list of monotonically non-decreasing values.  The last one
-- is the actual value.  (The operations here ensure that the values are
-- strictly increasing, but they only rely on non-decreasing.)
newtype Improving a = Imp { unImp :: [a] } deriving Show

-- | Apply a unary function inside an 'Improving' representation.
inImp :: ([a] -> [b]) -> (Improving a -> Improving b)
inImp f = Imp . f . unImp

-- | Apply a unary function inside an 'Improving' representation.
inImp2 :: ([a] -> [b] -> [c]) -> (Improving a -> Improving b -> Improving c)
inImp2 f = inImp . f . unImp

-- | A known improving value (which doesn't really improve)
exactly :: Ord a => a -> Improving a
exactly = Imp . (:[])

-- | Extract an exact value from an improving value
exact :: Improving a -> a
exact = last . unImp

instance Eq a => Eq (Improving a) where
  (==) = (==) `on` exact

instance Ord a => Ord (Improving a) where
  Imp xs `compare` Imp ys = -- trace "Improving: compare" $
                            xs `compares` ys
  -- experimental.  probably eliminate.
  Imp xs <= Imp ys = xs `leq` ys
  min = inImp2 shortMerge
  max = inImp2 (specNY monotonicAppend)

-- This one wasn't in the Improving Values papers.  Here so that
-- 'compare', '(<=)', etc are defined on Improving.
compares :: Ord a => [a] -> [a] -> Ordering
compares [] _    = error "compares: emptied first argument"
compares _ []    = error "compares: emptied second argument"
compares [x] (y:_) | x < y = LT
compares (x:_) [y] | x > y = GT
compares [x] [y] = compare x y
-- we know x >= y and length ys >= 2
compares xs@[_] (_:ys') = compares xs ys'
-- we know x <= y and length xs >= 2
compares (_:xs') ys@[_] = compares xs' ys
-- neither list is down to last element.  progress where less is known.
compares xs@(x:xs') ys@(y:ys') | x == y    = compares xs' ys'
                               | x  < y    = compares xs' ys
                               | otherwise = compares xs  ys'

-- Hm!  The test I really want is (<=), which can get an answer based on
-- slightly less information than compares.

leq :: Ord a => [a] -> [a] -> Bool
leq [] _    = error "leq: emptied first argument"
leq _ []    = error "leq: emptied second argument"
leq [x] (y:_) | x <= y = True
leq (x:_) [y] | x >  y = False
leq [x] [y] = x <= y
-- we know x > y and length ys >= 2
leq xs@[_] (_:ys') = leq xs ys'
-- we know x <= y and length xs >= 2
leq (_:xs') ys@[_] = leq xs' ys
-- neither list is down to last element.  progress where less is known.
leq xs@(x:xs') ys@(y:ys') | x == y    = leq xs' ys'
                          | x  < y    = leq xs' ys
                          | otherwise = leq xs  ys'

-- leq didn't fix the bug I'm finding in phooey (src/Examples/Monad, t5)
-- when using SReactive instead of PrimReactive in Data/Reactive.
-- Probably remove leq later.


shortMerge :: Ord a => [a] -> [a] -> [a]
shortMerge [] _ = []
shortMerge _ [] = []
shortMerge xs@(x:xs') ys@(y:ys')
  | x == y    = x : shortMerge xs' ys'
  | x <  y    = x : shortMerge xs' ys
  | otherwise = y : shortMerge xs  ys'

monotonicAppend :: Ord a => [a] -> [a] -> [a]
-- monotonicAppend [x]     ys = x : dropWhile (<= x) ys
-- monotonicAppend (x:xs') ys = x : monotonicAppend xs' ys
-- monotonicAppend []      _  = error "monotonicAppend: empty list"

-- From "Encapsulating nondeterminacy in an abstract data type with
-- deterministic semantics"
monotonicAppend xs ys = xs ++ dropWhile (<= last xs) ys


-- TODO: consider trimming ys as we go, rather than later.  However, I
-- have a fuzzy understanding of why spec_max and not just max in the
-- papers.

-- | Interpret 'Nothing' values as lower bounds
improveMbs :: [(t, Maybe a)] -> [(Improving t, a)]
improveMbs = foldr f []
 where
   f (t,Just a ) qs = (Imp [t],a) : qs
   f (t,Nothing) ~((Imp ts', a) : qs') = (Imp (t:ts'), a) : qs'
   -- f (_,Nothing) [] = error "improveMbs: input ends in a Nothing"

-- The lazy pattern (~) above is essential for laziness.  It also
-- complicates giving an error message if the input ends in a Nothing.

-- improveMbs [] = []
-- improveMbs ((t,Just a ) : ps') = (Imp [{-tr True-} t],a) : improveMbs ps'
-- improveMbs ((t,Nothing) : ps') = (Imp ({-tr False-} t:ts'), a) : qs'
--  where
--     (Imp ts', a) : qs' = improveMbs ps'

-- tr :: (Show x, Show t) => x -> t -> t
-- tr x t = t
--          -- trace (show (t, x)) t

-- improveMbs = foldr f []
--  where
--    f (t,Just a ) qs = (Imp [t],a) : qs
--    f (t,Nothing) qs =
--      case qs of ((Imp ts', a) : qs') -> (Imp (t:ts'), a) : qs'
--                 [] -> error "improveMbs: input ends in a Nothing"

-- TODO: re-think the case of input ending in a Nothing.


---- Misc

spec :: (a -> b) -> (a -> b)
spec f a = a `par` f a

specNY :: (a -> b -> c) -> (a -> b -> c)
specNY f a = spec (f a)

specYY :: (a -> b -> c) -> (a -> b -> c)
specYY f a = spec (spec f a)

start :: [a] -> [a]
start [] = []
start (x:xs) = specYY (:) x (start xs)

-- Hm. Does this specNY really do anything?  How far does 'par' evaluate?
-- Probably to WHNF, which wouldn't help much, would it?  And I don't
-- understand the point yet.  Read further in the paper.
