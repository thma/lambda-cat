{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-- This module exposes the GADT data type FreeCat which instantiates the type classes
    Closed, Cartesian and Category (among others).
    This makes this data type the ideal compilation target for toCCC.

    > toCCC @FreeCat (\(x, y) -> x)
    Comp Fst Id
--}

module FreeCat where

import           Cat     (BoolCat (..), BoolLike (..), Cartesian (..),
                          Category (..), Closed (..), EqCat (..), EqLike (..),
                          Monoidal (..), NumCat (..), fanC)
import           Prelude hiding (id, (.))
import           Text.Show.Functions ()

data FreeCat a b where
  Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
  Id :: FreeCat a a
  IntConst :: Integer -> FreeCat a Integer
  FromInt :: (Num b) => FreeCat Integer b
  Fst :: FreeCat (a, b) a
  Snd :: FreeCat (a, b) b
  Dup :: FreeCat a (a, a)
  Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
  Add :: (Num a) => FreeCat (a, a) a
  Sub :: (Num a) => FreeCat (a, a) a
  Mul :: (Num a) => FreeCat (a, a) a
  Abs :: (Num a) => FreeCat a a
  Neg :: (Num a) => FreeCat a a
  Apply :: FreeCat (FreeCat a b, a) b
  Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
  Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c
  Lift :: (a -> b) -> FreeCat a b
  Eql :: (EqLike a b, BoolLike b) => FreeCat (a, a) b
  Leq :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Geq :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Les :: (Ord a, BoolLike b) => FreeCat (a, a) b
  Gre :: (Ord a, BoolLike b) => FreeCat (a, a) b
  -- Boolean
  And :: (BoolLike a) => FreeCat (a, a) a
  Or :: (BoolLike a) => FreeCat (a, a) a
  Not :: (BoolLike a) => FreeCat a a
  T :: (BoolLike a) => FreeCat b a
  F :: (BoolLike a) => FreeCat b a
  -- Conditional branching: selects between two morphisms based on a boolean
  IfThenElse :: FreeCat (Bool, (FreeCat b c, FreeCat b c)) (FreeCat b c)
  -- Value-level conditional: selects between two values based on a boolean
  IfVal :: FreeCat (Bool, (a, a)) a
  -- Fixpoint combinator for recursive definitions
  -- Takes a step function (rec, input) -> output and produces the fixed point
  Fix :: FreeCat (FreeCat a b, a) b -> FreeCat a b

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

deriving instance Show (FreeCat a b)

instance Category FreeCat where
  (.) = Comp
  id = Id

instance Monoidal FreeCat where
  parC = Par

instance Cartesian FreeCat where
  fstC = Fst
  sndC = Snd
  dupC = Dup

instance NumCat FreeCat where
  mulC = Mul
  negC = Neg
  addC = Add
  subC = Sub
  absC = Abs

  leqC = Leq
  geqC = Geq
  lesC = Les
  greC = Gre

instance (Num a) => Num (FreeCat z a) where
  f + g = Add . fanC f g
  f * g = Mul . fanC f g
  negate f = Neg . f
  f - g = Sub . fanC f g
  abs f = Abs . f
  -- Signum has no dedicated constructor; Lift wraps Prelude.signum over the underlying type.
  signum f = Lift Prelude.signum . f
  fromInteger i = FromInt . IntConst i

instance BoolCat FreeCat where
  andC = And
  orC = Or
  notC = Not

  ifTE = IfThenElse

instance (BoolLike b) => BoolLike (FreeCat a b) where
  f && g = And . fanC f g
  f || g = Or . fanC f g
  not f = Not . f
  true = T
  false = F

instance EqCat FreeCat where
  eqlC = Eql

instance (BoolLike b, EqLike a b) => EqLike (FreeCat a a) (FreeCat a b) where
  f == g = Eql . fanC f g

instance EqLike Integer (FreeCat Integer Bool) where
  (==) = error "NYI EqLike Integer (FreeCat Integer Bool)" --(Cat.==)

instance
  EqLike
    (FreeCat (Integer, Integer) Integer)
    (FreeCat (Integer, Integer) Bool)
  where
  f == g = Eql . fanC f g

instance EqLike (FreeCat Integer Integer) Bool where
  _ == _ = error "NYI EqLike (FreeCat Integer Integer) Bool" --Eql . fanC f g

-- Structural equality on FreeCat is not derivable due to GADT existential constraints.
-- Returns False conservatively; the only caller (ruleParDupEq in Rewrite) is commented out.
instance Eq (FreeCat a b) where
  _ == _ = False
