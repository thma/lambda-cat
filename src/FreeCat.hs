{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}

module FreeCat where

import           Cat
import           Control.Category
import           Prelude          hiding (id, (.))

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
  AddCurry :: (Num a) => FreeCat a (FreeCat a a)
  Mul :: (Num a) => FreeCat (a, a) a
  Abs :: (Num a) => FreeCat a a
  Neg :: (Num a) => FreeCat a a
  Apply :: FreeCat (FreeCat a b, a) b
  Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
  Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c
  Wrap :: (a -> b) -> FreeCat a b
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
  IfThenElse :: (BoolLike test) => FreeCat (test, (FreeCat b c, FreeCat b c)) (FreeCat b c)

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

-- this little hack is needed to allow auto deriving Show for FreeCat
instance Show (a -> b) where
  showsPrec _ _ = showString "<function>"

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
  signum = error "TODO sig"
  fromInteger i = FromInt . IntConst i --error "TODO fromInteger"

instance BoolCat FreeCat where
  andC = And
  orC  = Or
  notC = Not
  --ifTE :: (BoolLike a) => FreeCat (a, (FreeCat b d, FreeCat b d)) (FreeCat b d)
  --ifTE :: (BoolLike a) => FreeCat (FreeCat (a, (FreeCat b d, FreeCat b d)) b) d
  --ifTE :: FreeCat (a, (FreeCat b d, FreeCat b d)) (FreeCat b d)
  ifTE = IfThenElse


instance (BoolLike b) => BoolLike (FreeCat a b) where
  f && g = And . fanC f g
  f || g = Or . fanC f g
  not f = Not . f
  true = T
  false = F
  --ite :: FreeCat a b -> (FreeCat c d, FreeCat c d) -> FreeCat c d
  --ite test (f,g) = undefined --_IfThenElse . test . fanC f g

instance EqCat FreeCat where
  eqlC = Eql

instance (BoolLike b, EqLike a b) => EqLike (FreeCat a a) (FreeCat a b) where
  f == g = Eql . fanC f g

instance EqLike Integer (FreeCat Integer Bool) where
  (==) = error "NYI EqLike Integer (FreeCat Integer Bool)" --(Cat.==)

instance EqLike (FreeCat (Integer, Integer) Integer)
                (FreeCat (Integer, Integer) Bool) where
  f == g = Eql . fanC f g

instance EqLike (FreeCat Integer Integer) Bool where
  f == g = error "NYI EqLike (FreeCat Integer Integer) Bool" --Eql . fanC f g

instance Eq (FreeCat a b) where
  f == g = f Prelude.== g