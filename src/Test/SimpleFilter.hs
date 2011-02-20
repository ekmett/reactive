-- Tracking down a problem with event merging

import Data.Monoid
import Control.Applicative (pure,(<$>))
import Control.Monad (join)

import Data.Unamb

import Data.Max
import Data.AddBounds
import FRP.Reactive.Improving
import FRP.Reactive.Future
import FRP.Reactive.PrimReactive -- hiding (filterE)
import FRP.Reactive.Reactive     -- hiding (filterE)
import FRP.Reactive.Internal.Future
import FRP.Reactive.Internal.Reactive

-- For neverE
import FRP.Reactive.Internal.Clock
import FRP.Reactive.Internal.TVal
import System.IO.Unsafe


negateOdds :: Event Int -> Event Int
negateOdds e =
  (negate <$> filterE odd e) `mappend` (filterE even e)

en :: TimeT -> Improving (AddBounds TimeT)
en = exactly . NoBound

an :: TimeT -> Improving (AddBounds TimeT)
an = after   . NoBound

t :: (Bounded t, Eq t) => Int -> EventG t a -> [FutureG t a]
t n = take n . eFutures

e7 :: Event Int
e7 = listEG [(en 1,1),(en 2,2),(en 3,3),(an 4,17)]
t7 = t 3 e7

e8 = filterE odd e7
t8 = t 2 e8

e9 = negate <$> e8
t9 = t 2 e9

e10 = filterE even e7
t10 = t 1 e10

e11 = e9 `mappend` e10
t11 = t 3 e11

e12 = filterE (const True) e7
t12 = t 3 e12

e13 = filterE (const True) e7 `mappend` mempty
t13 = t 3 e13

e14 = filterE (const True) e7 `mappend` listEG [(an 5, error "five")]
t14 = t 3 e14

-- One occurrence out per second 
e15 = filterE (const True) e7 `mappend` neverE
t15 = t 3 e15

-- This one finishes fine.
e16 = filterE (const True) e7 `mappend` listEG [(maxBound, error "maxed out")]
t16 = t 3 e16

e17 = e7 `mappend` neverE
t17 = t 3 e17


-- Semantically equivalent to mappend
neverE :: Event a
neverE = unsafePerformIO $
         do c <- makeClock 
            (_,never) <- makeEvent c
            return never

-- as expected: [<Imp NoBound   C-c C-c
tN = t 1 neverE

-- Imp NoBound   C-c C-c
tinf :: ITime
tinf = getMax (futTime (head tN))

-- True
p1 = en 0 <= tinf

-- GT
p2 = compareI tinf (NoBound 0)
