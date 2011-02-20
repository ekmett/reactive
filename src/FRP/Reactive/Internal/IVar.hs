{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-state-hack #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Internal.IVar
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Write-once variables.
----------------------------------------------------------------------

module FRP.Reactive.Internal.IVar 
    ( IVar, newIVar, readIVar, tryReadIVar, writeIVar
    ) where


import Control.Concurrent.MVar
import Control.Applicative ((<$>))
import System.IO.Unsafe (unsafePerformIO)

newtype IVar a = IVar (MVar a)

newIVar :: IO (IVar a)
newIVar = IVar <$> newEmptyMVar

-- | Returns the value in the IVar.  The *value* will block
-- until the variable becomes filled.
readIVar :: IVar a -> a
readIVar (IVar v) = unsafePerformIO $ do -- putStrLn "readIVar"
                                         readMVar v

-- | Returns Nothing if the IVar has no value yet, otherwise
-- returns the value.
tryReadIVar :: IVar a -> IO (Maybe a)
tryReadIVar (IVar v) = do
    empty <- isEmptyMVar v
    if empty
       then return Nothing
       else Just <$> readMVar v

-- | Puts the value of the IVar.  If it already has a value,
-- block forever.
writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar v) x = putMVar v x

{-

-- From: Bertram Felgenhauer <int-e@gmx.de>
-- to: conal@conal.net
-- date: Mon, Nov 10, 2008 at 1:02 PM
-- subject: About IVars

-- Interestingly, the code triggers a bug in ghc; you have to compile
-- it with -fno-state-hack if you enable optimization. (Though Simon
-- Marlow says that it's not the state hack's fault. See
-- http://hackage.haskell.org/trac/ghc/ticket/2756)

-- Hm: ghc balks at {-# OPTIONS_GHC -fno-state-hack #-}


-- with a few tweaks by conal

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

-- an IVar consists of
-- a) A lock for the writers. (This avoids the bug explained above.)
-- b) An MVar to put the value into
-- c) The value of the IVar. This is the main difference between
--    our implementations.
data IVar a = IVar (MVar ()) (MVar a) a

-- Creating an IVar creates two MVars and sets up a suspended
-- takeMVar for reading the value.
-- It relies on unsafePerformIO to execute its body at most once;
-- As far as I know this is true since ghc 6.6.1 -- see
-- http://hackage.haskell.org/trac/ghc/ticket/986
newIVar :: IO (IVar a)
newIVar = do
   lock <- newMVar ()
   trans <- newEmptyMVar
   let {-# NOINLINE value #-}
       value = unsafePerformIO $ takeMVar trans
   return (IVar lock trans value)

-- Reading an IVar just returns its value.
readIVar :: IVar a -> a
readIVar (IVar _ _ value) = value

-- Writing an IVar takes the writer's lock and writes the value.
-- (To match your interface, use  takeMVar  instead of  tryTakeMVar)

writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar lock trans _) value = do
   a <- tryTakeMVar lock
   case a of
       Just () -> putMVar trans value
       Nothing -> error "writeIVar: already written"

-- writeIVar :: IVar a -> a -> IO Bool
-- writeIVar (IVar lock trans _) value = do
--    a <- tryTakeMVar lock
--    case a of
--        Just _  -> putMVar trans value >> return True
--        Nothing -> return False

-- I didn't originally support tryReadIVar, but it's easily implemented,
-- too.
tryReadIVar :: IVar a -> IO (Maybe a)
tryReadIVar (IVar lock _ value) = fmap f (isEmptyMVar lock)
 where
   f True  = Just value
   f False = Nothing

-- tryReadIVar (IVar lock _ value) = do
--    empty <- isEmptyMVar lock
--    if empty then return (Just value) else return Nothing

-}
