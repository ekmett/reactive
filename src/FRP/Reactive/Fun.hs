{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  FRP.Reactive.Fun
-- Copyright   :  (c) Conal Elliott 2007
-- License     :  GNU AGPLv3 (see COPYING)
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Functions, with constant functions optimized, with instances for many
-- standard classes.
----------------------------------------------------------------------

module FRP.Reactive.Fun (Fun, fun, apply, batch) where

import Prelude hiding
  ( zip, zipWith
#if __GLASGOW_HASKELL__ >= 609
                , (.), id
#endif
  )
#if __GLASGOW_HASKELL__ >= 609
import Control.Category
#endif


import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..),liftA)
import Control.Arrow 
#if __GLASGOW_HASKELL__ < 610
                     hiding (pure)
#endif
import Text.Show.Functions ()

import Control.Comonad

import Data.Zip (Zip(..))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import FRP.Reactive.Internal.Fun


-- TODO: write RULE for fun . const = K
fun :: (t -> a) -> Fun t a
fun = Fun

instance (CoArbitrary a,Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = oneof [liftA K arbitrary, liftA Fun arbitrary]

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Fun a b) where
  coarbitrary (K a)   = variant (0 :: Int) . coarbitrary a
  coarbitrary (Fun x) = variant (1 :: Int) . coarbitrary x

instance Show b => Show (Fun a b) where
  show (K x)   = "K " ++ show x
  show (Fun f) = "Fun " ++ show f

instance (Show a, Arbitrary a, EqProp a, EqProp b) => EqProp (Fun a b) where
  (=-=) = eqModels

instance Model (Fun a b) (a -> b) where
  model = apply

instance Model1 (Fun a) ((->) a) where
  model1 = apply

-- | 'Fun' as a function
apply :: Fun t a -> (t -> a)
apply (K   a) = const a
apply (Fun f) = f

instance Monoid a => Monoid (Fun t a) where
  mempty = K mempty
  K a  `mappend` K a' = K (a `mappend` a')
  funa `mappend` funb = Fun (apply funa `mappend` apply funb)

instance Functor (Fun t) where
  fmap f (K   a) = K   (f a)
  fmap f (Fun g) = Fun (f.g)  -- == Fun (fmap f g)

instance Zip (Fun t) where
  K x `zip` K y = K   (x,y)
  cf  `zip`  cx = Fun (apply cf `zip` apply cx)

instance Applicative (Fun t) where
  pure        = K
  K f <*> K x = K   (f x)
  cf  <*> cx  = Fun (apply cf <*> apply cx)

instance Monad (Fun t) where
  return = pure
  K   a >>= h = h a
  Fun f >>= h = Fun (f >>= apply . h)

#if __GLASGOW_HASKELL__ >= 609
instance Category Fun where
  id = Fun id
  K   b . _     = K   b
  Fun g . K   a = K   (g a)
  Fun f . Fun g = Fun (f . g)
#endif

instance Arrow Fun where
  arr             = Fun
#if __GLASGOW_HASKELL__ < 609
  _     >>> K b   = K   b
  K a   >>> Fun g = K   (g a)
  Fun g >>> Fun f = Fun (g >>> f)
#endif
  first           = Fun . first  . apply
  second          = Fun . second . apply
  K a'  *** K b'  = K (a',b')
  f     *** g     = first f >>> second g

instance Pointed (Fun t) where
  point = K

instance Monoid t => Copointed (Fun t) where
  extract = extract . apply

instance Monoid t => Comonad (Fun t) where
  duplicate (K   a) = K   (K a)
  duplicate (Fun f) = Fun (Fun . duplicate f)



----------------------------------

batch :: TestBatch
batch = ( "FRP.Reactive.Fun"
        , concatMap unbatch
            [ monoid              (undefined :: Fun NumT [T])
            , semanticMonoid      (undefined :: Fun NumT [T])
            , functor             (undefined :: Fun NumT (NumT,T,NumT))
            , semanticFunctor     (undefined :: Fun NumT ())
            , applicative         (undefined :: Fun NumT (NumT,T,NumT))
            , semanticApplicative (undefined :: Fun NumT ())
            , monad               (undefined :: Fun NumT (NumT,T,NumT))
            , semanticMonad       (undefined :: Fun NumT ())
            , arrow               (undefined :: Fun NumT (NumT,T,NumT))
            , ("specifics",
                [("Constants are"
                 ,property (\x -> (K (x :: NumT)) =-=
                                  ((fun . const $ x) :: Fun T NumT)))])
            ]
        )
