{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.TVal
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Timed values.  A primitive interface for futures.
----------------------------------------------------------------------

module FRP.Reactive.Internal.TVal
  ((:-->), (:+->), makeEvent) where

import Control.Applicative ((<$>)) -- ,liftA2
-- import Control.Monad (forever)
import Control.Concurrent (forkIO,yield) -- , ThreadId

-- import Control.Concurrent.Chan hiding (getChanContents)
import FRP.Reactive.Internal.Chan

--import System.Mem.Weak (mkWeakPtr,deRefWeak)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Data.Stream (Stream(..)) -- ,streamToList

import Data.Unamb (unamb,assuming)

import Data.AddBounds
import FRP.Reactive.Improving (Improving(..))
import FRP.Reactive.Future (FutureG,future)
import FRP.Reactive.Reactive (Event,TimeT,ITime)
import FRP.Reactive.PrimReactive (futureStreamE)

import FRP.Reactive.Internal.Misc (Sink)
import FRP.Reactive.Internal.Clock
import FRP.Reactive.Internal.Timing (sleepPast)
import FRP.Reactive.Internal.IVar
-- import FRP.Reactive.Internal.Reactive (isNeverE)

-- | An @a@ that's fed by a @b@
type b :--> a = (Sink b, a)

-- | Make a '(:-->)'.
type b :+-> a = IO (b :--> a)

-- | A value that becomes defined at some time.  'timeVal' may block if
-- forced before the time & value are knowable.  'definedAt' says whether
-- the value is defined at (and after) a given time and likely blocks
-- until the earlier of the query time and the value's actual time.
data TVal t a = TVal { timeVal :: (t,a), definedAt :: t -> Bool }

makeTVal :: Clock TimeT -> a :+-> TVal TimeT a
makeTVal (Clock getT _) = do -- putStrLn "makeTVal"
                             f <$> newIVar
  where
    f v = (sink, TVal (readIVar v) (unsafePerformIO . undefAt))
     where   
      undefAt t =
        -- Read v after time t.  If it's undefined, then it wasn't defined
        -- at t.  If it is defined, then see whether it was defined before t.
        do -- putStrLn $ "undefAt " ++ show t
           -- ser $ putStrLn $ "sleepPast " ++ show t
           sleepPast getT t
--            maybe False ((< t) . fst) <$> tryReadIVar v
           
           value <- tryReadIVar v
           case value of
             -- We're past t, if it's not defined now, it wasn't at t.
             Nothing     -> return False
             -- If it became defined before t, then it's defined now.
             Just (t',_) -> return (t' < t)

      sink a = do -- putStrLn "sink"
                  t <- getT
                  writeIVar v (t,a)

  --  sink a = getT >>= writeIVar v . flip (,) a

-- TODO: oops - the definedAt in makeTVal always waits until the given
-- time.  It could also grab the time and compare with t.  Currently that
-- comparison is done in tValImp.  How can we avoid the redundant test?
-- We don't really have to avoid it, since makeTVal isn't exported.

-- | 'TVal' as 'Future'
tValFuture :: Ord t => TVal t a -> FutureG (Improving (AddBounds t)) a
tValFuture v = future (tValImp v) (snd (timeVal v))

-- | 'TVal' as 'Improving'
tValImp :: Ord t => TVal t a -> Improving (AddBounds t)
tValImp v = Imp ta (\ t' -> assuming (not (definedAt' v t')) GT
                             `unamb` (ta `compare` t'))
 where
   ta = NoBound (fst (timeVal v))

definedAt' :: TVal t a -> AddBounds t -> Bool
definedAt' _     MinBound   = False
definedAt' tval (NoBound t) = definedAt tval t
definedAt' _     MaxBound   = True

-- definedAt' _ _ = error "definedAt': non-NoBound"


-- -- | Make a new event and a sink that writes to it.  Uses the given
-- -- clock to serialize and time-stamp.
-- makeEvent :: Clock TimeT -> a :+-> Event a
-- makeEvent clock =
--   do chanA <- newChan
--      chanF <- newChan
--      spin $ do
--          -- Get the skeleton tval written out immediately.  Details will
--          -- be added
--          (tval,snka) <- makeTVal clock
--          writeChan chanF (tValFuture tval)
--          readChan  chanA >>= snka
--      futs <- getChanContents chanF
--      return (futuresE futs, writeChanY chanA)

-- makeTVal :: Clock TimeT -> a :+-> TVal TimeT a


-- | Make a connected sink/future pair.  The sink may only be written to once.
makeFuture :: Clock TimeT -> (a :+-> FutureG ITime a)
makeFuture = (fmap.fmap.fmap) tValFuture makeTVal

-- | Make a new event and a sink that writes to it.  Uses the given
-- clock to serialize and time-stamp.
makeEvent :: Clock TimeT -> forall a. Show a => (a :+-> Event a)
makeEvent clock = (fmap.fmap) futureStreamE (listSink (makeFuture clock))

-- makeEvent clock =
--   do (snk,s) <- listSink (makeFuture clock)
--      let e = futureStreamE s
--      putStrLn $ "isNeverE e == " ++ show (isNeverE e)
--      -- putStrLn $ "makeEvent: e == " ++ show e
--      return (snk, e)
          

-- Turn a single-feedable into a multi-feedable

-- listSink :: (b :+-> a) -> (b :+-> [a])
-- listSink mk = do chanA <- newChan
--                  chanB <- newChan
--                  spin $ do
--                      (snk,a) <- mk
--                      -- putStrLn "writing input"
--                      writeChan chanA a
--                      readChan  chanB >>= snk
--                  as <- getChanContents chanA
--                  return (writeChanY chanB, as)

listSink :: Show a => (b :+-> a) -> (b :+-> Stream a)

-- listSink mk = do chanA <- newChan
--                  chanB <- newChan
--                  spin $ do
--                      (snk,a) <- mk
--                      -- putStrLn "writing input"
--                      writeChan chanA a
--                      readChan  chanB >>= snk
--                  as <- getChanStream chanA
--                  return (writeChanY chanB, as)
-- spin :: IO a -> IO ThreadId
-- spin = forkIO . forever


-- Yield control after channel write.  Helps responsiveness
-- tremendously.
writeChanY :: Chan a -> Sink a
writeChanY ch x = writeChan ch x >> yield
-- Equivalently:
-- writeChanY = (fmap.fmap) (>> yield) writeChan




-- I want to quit gathing input when no one is listening, to eliminate a
-- space leak.  Here's my first attempt:

-- listSink mk = do chanA <- newChan
--                  chanB <- newChan
--                  wchanA <- mkWeakPtr chanA Nothing
--                  let loop =
--                        do mbch <- deRefWeak wchanA
--                           case mbch of
--                             Nothing ->
--                               do -- putStrLn "qutting"
--                                  return ()
--                             Just ch ->
--                               do -- putStrLn "add value"
--                                  (a,snk) <- mk
--                                  writeChan ch a
--                                  readChan chanB >>= snk
--                                  loop
--                  forkIO loop
--                  as  <- getChanContents chanA
--                  return (writeChanY chanB, as)

-- This attempt fails.  The weak reference gets lost almost immediately.
-- My hunch: ghc optimizes away the Chan representation when compiling
-- getChanContents, and just holds onto the read and write ends (mvars),
-- via a technique described at ICFP 07.  I don't know how to get a
-- reliable weak reference, without altering Control.Concurrent.Chan.
-- 
-- Apparently this problem has popped up before.  See
-- http://haskell.org/ghc/docs/latest/html/libraries/base/System-Mem-Weak.html#v%3AaddFinalizer


listSink mk = do -- putStrLn "listSink"
                 chanA   <- newChan
                 chanB   <- newChan

--                  let loop = do (snk,a) <- mk
--                                -- putStrLn "sank"
--                                writeChanY chanA a
--                                readChan chanB >>= snk
--                                loop

--                  wwriteA <- weakChanWriter chanA
--                  let loop = do (snk,a) <- mk
--                                mbw <- wwriteA
--                                case mbw of
--                                  Nothing     -> putStrLn "bailing"
--                                  Just writeA -> do writeA a >> yield
--                                                    readChan chanB >>= snk
--                                                    loop

                 wwriteA <- weakChanWriter chanA
                 let loop = do mbw <- wwriteA
                               case mbw of
                                 Nothing     ->
                                   do -- putStrLn "bailing"
                                      return ()
                                 Just writeA ->
                                   do -- putStrLn "writing to weak channel"
                                      (snk,a) <- mk
                                      writeA a
                                      -- putStrLn "wrote"
                                      yield
                                      readChan chanB >>= snk
                                      loop

                 _ <- forkIO loop
                 as  <- getChanStream chanA

                 -- debugging.  defeats freeing.
                 -- forkIO $ print $ streamToList as

                 return (writeChanY chanB, as)


-- I hadn't been yielding after writing to chanA.  What implications?


-- | Variation on 'getChanContents', returning a stream instead of a
-- list.  Note that 'getChanContents' only makes infinite lists.  I'm
-- hoping to get some extra laziness by using irrefutable 'Cons' pattern
-- when consuming the stream.
getChanStream :: Chan a -> IO (Stream a)

-- getChanStream ch = unsafeInterleaveIO $
--                     liftA2 Cons (readChan ch) (getChanStream ch)

getChanStream ch
  = unsafeInterleaveIO (do
        x  <- readChan ch
        xs <- getChanStream ch
        return (Cons x xs)
    )


{-
-}
