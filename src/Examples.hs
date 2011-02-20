{-# LANGUAGE TypeOperators, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

----------------------------------------------------------------------
-- |
-- Module      :  Examples
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Simple test for Reactive
----------------------------------------------------------------------

-- module Main where

-- base
import Data.Monoid
import Data.IORef
import Control.Monad
import Control.Applicative
import Control.Arrow (first,second)
import Control.Concurrent (yield, forkIO, killThread, threadDelay, ThreadId)

-- wxHaskell
import Graphics.UI.WX hiding (Event,Reactive)
import qualified Graphics.UI.WX as WX
-- TypeCompose
import Control.Compose ((:.)(..), inO,inO2)
import Data.Title

-- Reactive
import Reactive.Reactive


{--------------------------------------------------------------------
    Mini-Phooey
--------------------------------------------------------------------}

type Win = Panel ()

type Wio = ((->) Win) :. IO :. (,) Layout

type Wio' a = Win -> IO (Layout,a)


wio :: Wio' a -> Wio a
wio = O . O

unWio :: Wio a -> Wio' a
unWio = unO . unO

inWio :: (Wio' a -> Wio' b) -> (Wio a -> Wio b)
inWio f = wio . f . unWio

inWio2 :: (Wio' a -> Wio' b -> Wio' c) -> (Wio a -> Wio b -> Wio c)
inWio2 f = inWio . f . unWio

instance Title_f Wio where
  title_f str = inWio ((fmap.fmap.first) (boxed str))

-- Bake in vertical layout.  See phooey for flexible layout.
instance Monoid Layout where
  mempty  = WX.empty
  mappend = above

instance Monoid a => Monoid (Wio a) where
  mempty  = wio    mempty
  mappend = inWio2 mappend

type WioE a = Wio (Event    a)
type WioR a = Wio (Reactive a)

buttonE :: String -> WioE ()
buttonE str = wio $ \ win ->
  do (e, snk) <- mkEvent
     b <- button win [ text := str, on command := snk () ]
     return (hwidget b, e)

buttonE' :: String -> a -> WioE a
buttonE' str a = (a `replace`) <$> buttonE str

sliderE :: (Int,Int) -> Int -> WioE Int
sliderE (lo,hi) initial = wio $ \ win ->
  do (e, snk) <- mkEvent
     s <- hslider win True lo hi
            [ selection := initial ]
     set s [ on command := getAttr selection s >>= snk ]
     return (hwidget s, e)

sliderR :: (Int,Int) -> Int -> WioR Int
sliderR lh initial = stepper initial <$> sliderE lh initial

stringO :: Wio (Sink String)
stringO = attrO (flip textEntry []) text

-- Make an output.  The returned sink collects updates.  On idle, the
-- latest update gets stored in the given attribute.
attrO :: Widget w => (Win -> IO w) -> Attr w a -> Wio (Sink a)
attrO mk attr = wio $ \ win ->
  do ctl <- mk win
     ref <- newIORef Nothing
     setAttr (on idle) win $
       do readIORef ref >>= maybe mempty (setAttr attr ctl)
          writeIORef ref Nothing
          return True
     return (hwidget ctl , writeIORef ref . Just)

-- -- The following alternative ought to be more efficient.  Oddly, the timer
-- -- doesn't get restarted, although enabled gets set to True.

-- stringO = wio $ \ win ->
--   do ctl <- textEntry win []
--      ref <- newIORef (error "stringO: no initial value")
--      tim <- timer win [ interval := 10, enabled := False ]
--      let enable b = do putStrLn $ "enable: " ++ show b
--                        setAttr enabled tim b
--      set tim [ on command := do putStrLn "timer"
--                                 readIORef ref >>= setAttr text ctl
--                                 enable False
--              ]
--      return ( hwidget ctl
--             , \ str -> writeIORef ref str >> enable True )

showO :: Show a => Wio (Sink a)
showO = (. show) <$> stringO

showR :: Show a => WioR (Sink a)
showR = pure <$> showO


-- | Horizontally-filled widget layout
hwidget :: Widget w => w -> Layout
hwidget = hfill . widget

-- | Binary layout combinator
above, leftOf :: Layout -> Layout -> Layout
la `above`   lb = fill (column  0 [la,lb])
la `leftOf`  lb = fill (row     0 [la,lb])

-- |  Get attribute.  Just a flipped 'get'.  Handy for partial application.
getAttr :: Attr w a -> w -> IO a
getAttr = flip get

-- | Set a single attribute.  Handy for partial application.
setAttr :: Attr w a -> w -> Sink a
setAttr attr ctl x = set ctl [ attr := x ]


{--------------------------------------------------------------------
    Running
--------------------------------------------------------------------}

-- | Fork a 'Wio': handle frame & widget creation, and apply layout.
forkWio :: (o -> IO ThreadId) -> String -> Wio o -> IO ()
forkWio forker name w = start $
  do  f     <- frame [ visible := False, text := name ]
      pan   <- panel f []
      (l,o) <- unWio w pan
      set pan [ layout := l ]
      forker o
      -- Yield regularly, to allow other threads to continue.  Unnecessary
      -- when apps are compiled with -threaded.
      -- timer pan [interval := 10, on command := yield]
      set f   [ layout  := fill (widget pan)
              , visible := True
              ]

-- | Fork a 'WioE'
forkWioE :: String -> WioE Action -> IO ()
forkWioE = forkWio forkE

-- | Fork a 'WioR'
forkWioR :: String -> WioR Action -> IO ()
forkWioR = forkWio forkR


{--------------------------------------------------------------------
    Examples
--------------------------------------------------------------------}

alarm :: Double -> Int -> IO (Event Int)
alarm secs reps =
  do (e,snk) <- mkEvent
     forkIO $ forM_ [1 .. reps] $ \ i ->
               do threadDelay micros
                  snk i
     return e
 where
   micros = round (1.0e6 * secs)
                          

t0 = alarm 0.5 10 >>= \ e -> runE $ print <$> {-traceE (const "boo!")-} e

mkAB :: WioE String
mkAB = buttonE' "a" "a" `mappend` buttonE' "b" "b"


t1 = forkWioE "t1" $ liftA2 (<$>) stringO mkAB

acc :: WioE String
acc = g <$> mkAB
 where
   g :: Event String -> Event String
   g e = "" `accumE` (flip (++) <$> e)

t2 = forkWioE "t2" $ liftA2 (<$>) stringO acc

total :: Show a => WioR (Sink a)
total = title "total" showR

sl :: Int -> WioR Int
sl = sliderR (0,100)

apples, bananas, fruit :: WioR Int
apples  = title "apples"  $ sl 3
bananas = title "bananas" $ sl 7
fruit   = title "fruit"   $ (liftA2.liftA2) (+) apples bananas

t3 = forkWioR "t3" $ liftA2 (<**>) fruit total 

t4 = forkWioR "t4" $ liftA2 (<*>) showR (sl 0)

t5 = forkWioR "t5" $ liftA2 (<$>) showO (sl 0)

-- This example shows what happens with expensive computations.  There's a
-- lag between slider movement and shown result.  Can even get more than
-- one computation behind.
t6 = forkWioR "t6" $ liftA2 (<$>) showO (fmap (ack 2) <$> sliderR (0,1000) 0)

ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

-- Test switchers.  Ivan Tomac's example.
sw1 = do (e, snk) <- mkEvent
         forkR $ print <$> pure "init" `switcher` ((\_ -> pure "next") <$> e)
         snk ()
         snk ()

-- TODO: replace sw1 with a declarative GUI example, say switching between
-- two different previous GUI examples.

main = t6


updPair :: Either c d -> (c,d) -> (c,d)
updPair = (first.const) `either` (second.const)

-- updPair (Left  c') (_,d) = (c',d)
-- updPair (Right d') (c,_) = (c,d')

-- mixEither :: (Event c, Event d) -> Event (Either c d)
-- mixEither :: (Functor f, Monoid (f (Either a b))) =>
--               (f a, f b) -> f (Either a b)
mixEither :: MonadPlus m => (m a, m b) -> m (Either a b)
mixEither (ec,ed) = liftM Left ec `mplus` liftM Right ed

-- unmixEither :: Event (Either c d) -> (Event c, Event d)
unmixEither :: MonadPlus m => m (Either c d) -> (m c, m d)
unmixEither ecd = (filt left, filt right)
 where
   filt f = joinMaybes (liftM f ecd)

left :: Either c d -> Maybe c
left (Left  c) = Just c
left _         = Nothing

right :: Either c d -> Maybe d
right (Right d) = Just d
right _         = Nothing


-- pairEditE :: (Event c, Event d) -> Event ((c,d) -> (c,d))

-- pairEditE :: (Functor f, Monoid (f ((d, a) -> (d, a)))) =>
--              (f d, f a) -> f ((d, a) -> (d, a))
-- pairEditE (ce,de) =
--   ((first.const) <$> ce) `mappend` ((second.const) <$> de)

-- pairEditE :: (Functor m, MonadPlus m) => (m d, m a) -> m ((d, a) -> (d, a))
-- pairEditE (ce,de) =
--   ((first.const) <$> ce) `mplus` ((second.const) <$> de)

pairEditE :: MonadPlus m => (m c,m d) -> m ((c,d) -> (c,d))
pairEditE = liftM updPair . mixEither

-- pairEditE cde = liftM updPair (mixEither cde)

-- or, skipping sums

-- pairEditE (ce,de) =
--   liftM (first.const) ce `mplus` liftM (second.const) de

pairE :: (c,d) -> (Event c, Event d) -> Event (c,d)
pairE cd cde = cd `accumE` pairEditE cde

pairR :: Reactive c -> Reactive d -> Reactive (c,d)

-- (c `Stepper` ce) `pairR` (d `Stepper` de) =
--   (c,d) `stepper` pairE (c,d) (ce,de)

-- More directly:

(c `Stepper` ce) `pairR` (d `Stepper` de) =
  (c,d) `accumR` pairEditE (ce,de)

-- pairR' :: Reactive c -> Reactive d -> Reactive (c,d)
-- (c `Stepper` ce) `pairR'` (d `Stepper` de) =
--   (c,d) `accumR` pairEditE (ce,de)

