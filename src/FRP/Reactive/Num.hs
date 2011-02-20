{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Num
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Numeric class instances for behaviors
----------------------------------------------------------------------

module FRP.Reactive.Num () where

import Prelude hiding (zip,unzip)

import FRP.Reactive.Behavior
import Control.Applicative

import Data.Zip

noOv :: String -> String -> a
noOv ty meth = error $ meth ++ ": No overloading for " ++ ty

noFun :: String -> a
noFun = noOv "behavior"

-- Eq & Show are prerequisites for Num, so they need to be faked here
instance Eq (Behavior b) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"

instance Ord b => Ord (Behavior b) where
  min = liftA2 min
  max = liftA2 max

instance Enum a => Enum (Behavior a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = pure . toEnum
  fromEnum       = noFun "fromEnum"
  enumFrom       = noFun "enumFrom"
  enumFromThen   = noFun "enumFromThen"
  enumFromTo     = noFun "enumFromTo"
  enumFromThenTo = noFun "enumFromThenTo"

instance Show (Behavior b) where
  show      = noFun "show"
  showsPrec = noFun "showsPrec"
  showList  = noFun "showList"

instance Num b => Num (Behavior b) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Num a, Ord a) => Real (Behavior a) where
  toRational = noFun "toRational"

instance Integral a => Integral (Behavior a) where
  quot      = liftA2 quot
  rem       = liftA2 rem
  div       = liftA2 div
  mod       = liftA2 mod
  quotRem   = (fmap.fmap) unzip (liftA2 quotRem)
  divMod    = (fmap.fmap) unzip (liftA2 divMod)
  toInteger = noFun "toInteger"

instance Fractional b => Fractional (Behavior b) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (Behavior b) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance RealFrac a => RealFrac (Behavior a) where
  properFraction = noFun "properFraction"
  truncate       = noFun "truncate"
  round          = noFun "round"
  ceiling        = noFun "ceiling"
  floor          = noFun "floor"

instance RealFloat a => RealFloat (Behavior a) where
  floatRadix     = noFun "floatRadix"
  floatDigits    = noFun "floatDigits"
  floatRange     = noFun "floatRange"
  decodeFloat    = noFun "decodeFloat"
  encodeFloat    = (fmap.fmap) pure encodeFloat
  exponent       = noFun "exponent"
  significand    = noFun "significand"
  scaleFloat n   = fmap (scaleFloat n)
  isNaN          = noFun "isNaN"
  isInfinite     = noFun "isInfinite"
  isDenormalized = noFun "isDenormalized"
  isNegativeZero = noFun "isNegativeZero"
  isIEEE         = noFun "isIEEE"
  atan2          = liftA2 atan2
