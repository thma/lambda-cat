{-# OPTIONS_GHC -fno-warn-orphans #-}

{-- This module contains instance definitions of (->) for all required type-classes.
    This is required to allow interpretation of FreeCat terms as standard Haskell functions.

    See for instance eval :: FreeCat a b -> (a -> b) in the Interpreter module.
--}

module Hask where

import           Cat
import qualified GHC.Base

instance Category (->) where
  id = GHC.Base.id
  (.) = (GHC.Base..)

instance Monoidal (->) where
  parC f g (x, y) = (f x, g y) -- this could also be implemented as `bimap f g` (imported from Data.Bifunctor)

instance Cartesian (->) where
  fstC (x, _y) = x
  sndC (_x, y) = y
  dupC x = (x, x)

instance Closed (->) where
  applyC (f, x) = f x
  curryC = curry
  uncurryC = uncurry

instance NumCat (->) where
  mulC = uncurry (*)
  negC = negate
  addC = uncurry (+)
  subC = uncurry (-)
  absC = abs

--  leqC = uncurry (<=)
--  geqC = uncurry (>=)
--  lesC = uncurry (<)
--  greC = uncurry (>)

instance BoolCat (->) where
  andC = uncurry (Cat.&&)
  orC = uncurry (Cat.||)
  notC = Cat.not
  ifTE (test, (f, g)) x = if test then f x else g x

instance EqCat (->) where
  eqlC = uncurry (Cat.==)
