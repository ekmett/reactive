{-# LANGUAGE TypeSynonymInstances, FlexibleInstances
           , TypeFamilies
  #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module FRP.Reactive.VectorSpace( ) where

import FRP.Reactive.Behavior
import Control.Applicative

import Data.VectorSpace

instance AdditiveGroup v => AdditiveGroup (Behavior v) where
  zeroV   = pure   zeroV
  (^+^)   = liftA2 (^+^)
  negateV = liftA   negateV

instance VectorSpace v => VectorSpace (Behavior v) where
  type Scalar (Behavior v) = Scalar v
  (*^) s = fmap (s *^)
