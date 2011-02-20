-- From Beelsebob's: http://hpaste.org/13096

-- *FRP.Reactive.Behavior FRP.Reactive.Reactive FRP.Reactive.Improving FRP.Reactive.Fun FRP.Reactive.Internal.Fun> paddlePosR
-- 0.0 `Stepper` (1.0,5.0e-2)->(2.0,0.0)->(3.0,5.0e-2)->(*** Exception: Prelude.undefined
-- *FRP.Reactive.Behavior FRP.Reactive.Reactive FRP.Reactive.Improving FRP.Reactive.Fun FRP.Reactive.Internal.Fun> paddlePosR `FRP.Reactive.Reactive.snapshot_` (listEG [(exactly (2.5 :: TimeT), ()),(exactly 3.5, ())]) 
-- (2.5,0.0)->(3.5,0.0)

-- I was unable to reproduce the error:

import FRP.Reactive.Improving
import FRP.Reactive.PrimReactive
import FRP.Reactive.Reactive

r :: Reactive Int
r = 0 `stepper` listEG [(exactly 1,1),(exactly 2,2),(exactly 3,3),(after 4,17)]

e :: Event ()
e = listEG [(exactly 2.5, ()),(exactly 3.5, ())] 

e1 :: Event Int
e1 = r `snapshot_` e

-- (Imp 2.5,2)->(Imp 3.5,3)

e2 :: EventG ITime (Maybe (), Int)
e2 = r `snap` e

-- (Imp 1.0,(Nothing,1))->(Imp 2.0,(Nothing,2))->(Imp 2.5,(Just (),2))->(Imp 3.0,(Nothing,3))->(Imp 3.5,(Just (),3))->(Imp *** Exception: Prelude.undefined
