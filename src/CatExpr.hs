{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-- This module exposes the GADT data type CatExpr which instantiates the type classes
    Closed, Cartesian and Category (among others).
    This makes this data type the ideal compilation target for toCCC.

    > toCCC @CatExpr (\(x, y) -> x)
    Comp Fst Id
--}

module CatExpr where

import           Cat     (BoolCat (..), BoolLike (..), Cartesian (..),
                          Category (..), Closed (..), EqCat (..), EqLike (..),
                          Monoidal (..), NumCat (..), fanC)
import           Prelude hiding (id, (.))
import           Text.Show.Functions ()

data CatExpr a b where
  Comp :: CatExpr b c -> CatExpr a b -> CatExpr a c
  Id :: CatExpr a a
  IntConst :: Integer -> CatExpr a Integer
  FromInt :: (Num b) => CatExpr Integer b
  Fst :: CatExpr (a, b) a
  Snd :: CatExpr (a, b) b
  Dup :: CatExpr a (a, a)
  Par :: CatExpr a b -> CatExpr c d -> CatExpr (a, c) (b, d)
  Add :: (Num a) => CatExpr (a, a) a
  Sub :: (Num a) => CatExpr (a, a) a
  Mul :: (Num a) => CatExpr (a, a) a
  Abs :: (Num a) => CatExpr a a
  Neg :: (Num a) => CatExpr a a
  Apply :: CatExpr (CatExpr a b, a) b
  Curry :: CatExpr (a, b) c -> CatExpr a (CatExpr b c)
  Uncurry :: CatExpr a (CatExpr b c) -> CatExpr (a, b) c
  Lift :: (a -> b) -> CatExpr a b
  Eql :: (EqLike a b, BoolLike b) => CatExpr (a, a) b
  Leq :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Geq :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Les :: (Ord a, BoolLike b) => CatExpr (a, a) b
  Gre :: (Ord a, BoolLike b) => CatExpr (a, a) b
  -- Boolean
  And :: (BoolLike a) => CatExpr (a, a) a
  Or :: (BoolLike a) => CatExpr (a, a) a
  Not :: (BoolLike a) => CatExpr a a
  T :: (BoolLike a) => CatExpr b a
  F :: (BoolLike a) => CatExpr b a
  -- Conditional branching: selects between two morphisms based on a boolean
  IfThenElse :: CatExpr (Bool, (CatExpr b c, CatExpr b c)) (CatExpr b c)
  -- Value-level conditional: selects between two values based on a boolean
  IfVal :: CatExpr (Bool, (a, a)) a
  -- Fixpoint combinator for recursive definitions
  -- Takes a step function (rec, input) -> output and produces the fixed point
  Fix :: CatExpr (CatExpr a b, a) b -> CatExpr a b

instance Closed CatExpr where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

deriving instance Show (CatExpr a b)

instance Category CatExpr where
  (.) = Comp
  id = Id

instance Monoidal CatExpr where
  parC = Par

instance Cartesian CatExpr where
  fstC = Fst
  sndC = Snd
  dupC = Dup

instance NumCat CatExpr where
  mulC = Mul
  negC = Neg
  addC = Add
  subC = Sub
  absC = Abs

  leqC = Leq
  geqC = Geq
  lesC = Les
  greC = Gre

instance (Num a) => Num (CatExpr z a) where
  f + g = Add . fanC f g
  f * g = Mul . fanC f g
  negate f = Neg . f
  f - g = Sub . fanC f g
  abs f = Abs . f
  -- Signum has no dedicated constructor; Lift wraps Prelude.signum over the underlying type.
  signum f = Lift Prelude.signum . f
  fromInteger i = FromInt . IntConst i

instance BoolCat CatExpr where
  andC = And
  orC = Or
  notC = Not

  ifTE = IfThenElse

instance (BoolLike b) => BoolLike (CatExpr a b) where
  f && g = And . fanC f g
  f || g = Or . fanC f g
  not f = Not . f
  true = T
  false = F

instance EqCat CatExpr where
  eqlC = Eql

instance (BoolLike b, EqLike a b) => EqLike (CatExpr a a) (CatExpr a b) where
  f == g = Eql . fanC f g

instance EqLike Integer (CatExpr Integer Bool) where
  (==) = error "NYI EqLike Integer (CatExpr Integer Bool)" --(Cat.==)

instance
  EqLike
    (CatExpr (Integer, Integer) Integer)
    (CatExpr (Integer, Integer) Bool)
  where
  f == g = Eql . fanC f g

instance EqLike (CatExpr Integer Integer) Bool where
  _ == _ = error "NYI EqLike (CatExpr Integer Integer) Bool" --Eql . fanC f g

-- Structural equality on CatExpr is not derivable due to GADT existential constraints.
-- Returns False conservatively; the only caller (ruleParDupEq in Rewrite) is commented out.
instance Eq (CatExpr a b) where
  _ == _ = False
