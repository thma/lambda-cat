{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

{-- This module contains definition of categories that are required for
    modelling Closed Cartesian Categories.
--}  

module Cat where
  
import           Prelude          hiding (id, (.))

class Category cat where
  id  :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

class Category k => Monoidal k where
  parC :: k a c -> k b d -> k (a, b) (c, d)

class Monoidal k => Cartesian k where
  fstC :: k (a, b) a
  sndC :: k (a, b) b
  dupC :: k a (a, a)

class Cartesian k => Closed k where
  applyC   :: k (k a b, a) b
  curryC   :: k (a, b) c -> k a (k b c)
  uncurryC :: k a (k b c) -> k (a, b) c

fanC :: (Cartesian cat) => cat b c -> cat b d -> cat b (c, d)
fanC f g = parC f g . dupC

idC :: (Category k) => k a a
idC = id

class Cartesian k => NumCat k where
  mulC :: Num a => k (a, a) a
  negC :: Num a => k a a
  addC :: Num a => k (a, a) a
  subC :: Num a => k (a, a) a
  absC :: Num a => k a a

--  eqlC :: (Eq a, BoolLike b)  => k (a,a) b
  leqC :: (Ord a, BoolLike b) => k (a,a) b
  geqC :: (Ord a, BoolLike b) => k (a,a) b
  lesC :: (Ord a, BoolLike b) => k (a,a) b
  greC :: (Ord a, BoolLike b) => k (a,a) b

class Cartesian k => BoolCat k where
  andC :: BoolLike a => k (a, a) a
  orC  :: BoolLike a => k (a, a) a
  notC :: BoolLike a => k a a
  ifTE :: k (Bool, (k b d, k b d)) (k b d)

class BoolLike a where
  (&&)  :: a -> a -> a
  (||)  :: a -> a -> a
  not   :: a -> a
  true  :: a
  false :: a

instance BoolLike Bool where
  (&&)  = (Prelude.&&)
  (||)  = (Prelude.||)
  not   = Prelude.not
  true  = True
  false = False

class (BoolLike b) => EqLike a b where
  (==) ::  a -> a -> b

instance EqLike Integer Bool where
  (==) = (Prelude.==)

instance EqLike Bool Bool where
  (==) = (Prelude.==)

class Cartesian k => EqCat k where
  eqlC :: (EqLike a b, BoolLike b)  => k (a,a) b