{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Hask where

import           Data.Bifunctor   (bimap)
import           Cat

{--
 This module contains instance definitions of (->) for all required typeclasses.
 This is required to allow interpretation of FreeCat terms as standard Haskell functions.

 See for instance eval :: FreeCat a b -> (a -> b) in the Interpreter module.
--}

instance Monoidal (->) where
  parC f g = bimap f g

instance Cartesian (->) where
  fstC (x, _y) = x
  sndC (_x, y) = y
  dupC x       = (x, x)

instance Closed (->) where
    applyC (f,x) = f x
    curryC       = curry
    uncurryC     = uncurry

instance NumCat (->) where
  mulC = uncurry (*)
  negateC = negate
  addC = uncurry (+)
  subC = uncurry (-)
  absC = abs

--  eqlC = uncurry (==)
--  leqC = uncurry (<=)
--  geqC = uncurry (>=)
--  lesC = uncurry (<)
--  greC = uncurry (>)

instance BoolCat (->) where
  andC = uncurry (Cat.&&)
  orC  = uncurry (Cat.||)
  notC = Cat.not
  ifTE = uncurry Cat.ite

instance EqCat (->) where
  eqlC = uncurry (Cat.==)
