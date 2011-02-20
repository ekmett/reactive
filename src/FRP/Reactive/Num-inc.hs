----------------------------------------------------------------------
-- Meta-Module :  Num-inc
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Instances of Num classes for applicative functors.  To be #include'd
-- after defining APPLICATIVE as the applicative functor name.
-- 
-- You'll also have to import 'pure' and 'liftA2' from
-- "Control.Applicative".
----------------------------------------------------------------------

-- This module still needs some think work.  It now assumes that Eq, Ord,
-- Enum, and Show are undefined, which is not a good assumption.  For
-- instance, Maybe.


noOv :: String -> String -> a
noOv ty meth = error $ meth ++ ": No overloading for " ++ ty

noFun :: String -> a
noFun = noOv "behavior"

-- Eq & Show are prerequisites for Num, so they need to be faked here
instance Eq (APPLICATIVE b) where
  (==) = noFun "(==)"
  (/=) = noFun "(/=)"

instance Ord b => Ord (APPLICATIVE b) where
  min = liftA2 min
  max = liftA2 max

instance Enum b => Enum (APPLICATIVE b) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = pure . toEnum
  fromEnum       = noFun "fromEnum"
  enumFrom       = noFun "enumFrom"
  enumFromThen   = noFun "enumFromThen"
  enumFromTo     = noFun "enumFromTo"
  enumFromThenTo = noFun "enumFromThenTo"

instance Show (APPLICATIVE b) where
  show      = noFun "show"
  showsPrec = noFun "showsPrec"
  showList  = noFun "showList"

instance Num b => Num (APPLICATIVE b) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Num b, Ord b) => Real (APPLICATIVE b) where
  toRational = noFun "toRational"

instance Integral b => Integral (APPLICATIVE b) where
  quot      = liftA2 quot
  rem       = liftA2 rem
  div       = liftA2 div
  mod       = liftA2 mod
  quotRem   = (fmap.fmap) unzip (liftA2 quotRem)
  divMod    = (fmap.fmap) unzip (liftA2 divMod)
  toInteger = noFun "toInteger"

instance Fractional b => Fractional (APPLICATIVE b) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Floating b => Floating (APPLICATIVE b) where
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

instance RealFrac b => RealFrac (APPLICATIVE b) where
  properFraction = noFun "properFraction"
  truncate       = noFun "truncate"
  round          = noFun "round"
  ceiling        = noFun "ceiling"
  floor          = noFun "floor"

instance RealFloat b => RealFloat (APPLICATIVE b) where
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
