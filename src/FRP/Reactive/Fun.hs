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

module FRP.Reactive.Fun 
  ( Fun
  , fun
  , apply
#ifdef TEST
  , batch
#endif
  ) where

import Prelude hiding
  ( zip, zipWith
#if __GLASGOW_HASKELL__ >= 609
  , (.), id
#endif
  )
#if __GLASGOW_HASKELL__ >= 609
import Control.Category
#endif


import Data.Pointed
import Data.Copointed
import Data.Default
import Data.Semigroup
import Data.Semigroupoid
import Data.Monoid (Monoid(..))
import Data.Functor.Bind
import Control.Comonad

import Control.Applicative 
  ( Applicative(..)
#ifdef TEST
  , liftA
#endif
  )
import Control.Arrow 
#if __GLASGOW_HASKELL__ < 610
  hiding (pure)
#endif
import Text.Show.Functions ()

import Data.Zip (Zip(..))

#ifdef TEST
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
#endif

import FRP.Reactive.Internal.Fun


-- TODO: write RULE for fun . const = K
fun :: (t -> a) -> Fun t a
fun = Fun

#ifdef TEST
instance (CoArbitrary a,Arbitrary b) => Arbitrary (Fun a b) where
  arbitrary = oneof [liftA K arbitrary, liftA Fun arbitrary]

instance (Arbitrary a, CoArbitrary b) => CoArbitrary (Fun a b) where
  coarbitrary (K a)   = variant (0 :: Int) . coarbitrary a
  coarbitrary (Fun x) = variant (1 :: Int) . coarbitrary x

instance (Show a, Arbitrary a, EqProp a, EqProp b) => EqProp (Fun a b) where
  (=-=) = eqModels

instance Model (Fun a b) (a -> b) where
  model = apply

instance Model1 (Fun a) ((->) a) where
  model1 = apply
#endif

instance Show b => Show (Fun a b) where
  show (K x)   = "K " ++ show x
  show (Fun f) = "Fun " ++ show f

-- | 'Fun' as a function
apply :: Fun t a -> (t -> a)
apply (K   a) = const a
apply (Fun f) = f

instance Semigroup a => Semigroup (Fun t a) where
  K a  <> K a' = K (a <> a')
  funa <> funb = Fun (apply funa <> apply funb)
  
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

instance Apply (Fun t) where
  K f <.> K x = K   (f x)
  cf  <.> cx  = Fun (apply cf <.> apply cx)

instance Applicative (Fun t) where
  pure        = K
  K f <*> K x = K   (f x)
  cf  <*> cx  = Fun (apply cf <*> apply cx)

instance Bind (Fun t) where
  K   a >>- h = h a
  Fun f >>- h = Fun (f >>- apply . h)

instance Monad (Fun t) where
  return = pure
  K   a >>= h = h a
  Fun f >>= h = Fun (f >>= apply . h)

instance Semigroupoid Fun where
  K   b `o` _     = K   b
  Fun g `o` K   a = K   (g a)
  Fun f `o` Fun g = Fun (f `o` g)

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

instance Default t => Copointed (Fun t) where
  copoint = copoint . apply

instance Semigroup t => Extend (Fun t) where
  duplicate (K   a) = K   (K a)
  duplicate (Fun f) = Fun (Fun . duplicate f)

instance (Semigroup t, Monoid t) => Comonad (Fun t) where
  extract = extract . apply

#ifdef TEST
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

#endif
