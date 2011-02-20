-- Tracking down a problem with event merging

import Data.Monoid (mappend)
import Control.Applicative ((<$>))

import FRP.Reactive.Improving
import FRP.Reactive.Future
import FRP.Reactive.PrimReactive
import FRP.Reactive.Reactive
import FRP.Reactive.Internal.Future
import FRP.Reactive.Internal.Reactive


-- (Imp 1.0,1)->(Imp 2.0,2)->(Imp 3.0,3)->(Imp *** Exception: Prelude.undefined
e1 = listEG [(exactly 1,1),(exactly 2,2),(exactly 3,3),(after 4,17)]

-- (Imp 1.5,100)->(Imp 2.5,200)
e2 = listEG [(exactly 1.5, 100), (exactly 2.5, 200)]

-- (Imp *** Exception: Prelude.undefined
e3 = listEG [(after 2.5, 200)]

-- (Imp 1.5,100)->(Imp 2.3,200)->(Imp *** Exception: Prelude.undefined
e3' = listEG [(exactly 1.5, 100), (exactly 2.3, 200), (after 2.5, 300)]

-- (Imp 1.0,1)->(Imp 1.5,100)->(Imp 2.0,2)->(Imp 2.5,200)->(Imp 3.0,3)->(Imp *** Exception: Prelude.undefined
e4 = e1 `mappend` e2

-- (Imp 1.0,1)->(Imp 2.0,2)<interactive>: after: comparing after
e5 = e1 `mappend` e3

-- (Imp 1.0,1)->(Imp 1.5,100)->(Imp 2.0,2)->(Imp 2.3,200)<interactive>: after: comparing after
e5' = e1 `mappend` e3'

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)->(Imp 3.0,3)->(Imp *** Exception: Prelude.undefined
f1 = eFuture e1

-- <NoBound Imp 1.5,100 `Stepper` (Imp 2.5,200)>
f2 = eFuture e2

-- <NoBound Imp *** Exception: Prelude.undefined
f3 = eFuture e3

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)->(Imp 3.0,3)->(Imp *** Exception: Prelude.undefined
f4 = f1 `mappend` f3

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)<interactive>: after: comparing after
f5 = f1 `merge` f3

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)<interactive>: after: comparing after
f5' = eFuture e5



-- 

type Binop a = a -> a -> a

mergeLR, mergeL, mergeR :: (Ord s) => Binop (FutureG s (ReactiveG s b))

-- Same as 'merge'
u `mergeLR` v = 
  (inFutR (`merge` v) <$> u) `mappend` (inFutR (u `merge`) <$> v)

u `mergeL` v = inFutR (`merge` v) <$> u

u `mergeR` v = inFutR (u `merge`) <$> v

-- inFutR :: (FutureG s (ReactiveG s b) -> FutureG t (ReactiveG t b))
--        -> (ReactiveG s b -> ReactiveG t b)


-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)<interactive>: after: comparing after
f6 = f1 `mergeLR` f3

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)<interactive>: after: comparing after
f7 :: Future (Reactive Integer)
f7 = f1 `mergeL` f3

-- <NoBound Imp *** Exception: Prelude.undefined
f8 = f1 `mergeR` f3


f7' :: Future (Reactive Integer)

-- <NoBound Imp 1.0,1 `Stepper` (Imp 2.0,2)<interactive>: after: comparing after
f7' = q <$> f1
 where
   q (a `Stepper` Event u') = a `Stepper` Event (u' `merge` f3)
