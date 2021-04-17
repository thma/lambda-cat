{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Demo where

import           Control.Category
import           Prelude ((&&), (||), not, Bool(..), IO, Show, fst, snd, const, uncurry, print, ($), error, undefined)


class Category k => Cartesian k where
  parC :: k a c -> k b d -> k (a, b) (c, d)
  fstC :: k (a, b) a
  sndC :: k (a, b) b
  dupC :: k a (a, a)

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

class Cartesian k => BoolCat k where
  andC :: BoolLike a => k (a, a) a
  orC  :: BoolLike a => k (a, a) a
  notC :: BoolLike a => k a a
  --ifTE :: BoolLike a => k (a, (k b c, k b c)) c
  --ifTE :: (BoolLike test) =>(FreeCat a1 test, (FreeCat b1 c, FreeCat b1 c)) -> FreeCat b1 c
  --ifTE :: (Cartesian cat, BoolLike a) => k (k x a, (cat b c, cat b c)) (cat b c)

instance Cartesian (->) where
  parC f g (x,y) = (f x, g y)
  fstC (x, _y)   = x
  sndC (_x, y)   = y
  dupC x         = (x, x)

instance BoolCat (->) where
  andC = uncurry (Demo.&&)
  orC  = uncurry (Demo.||)
  notC = Demo.not
  --ifTE = \(test, (f,g)) -> if test then f else g --if test then f else g
--  ifTE (test, (f,g)) x = f x --if test then f x else g x
  
data FreeCat a b where
  Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
  Id   :: FreeCat a a
  Fst  :: FreeCat (a, b) a
  Snd  :: FreeCat (a, b) b
  Dup  :: FreeCat a (a, a)
  Par  :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
  And  :: (BoolLike a) => FreeCat (a, a) a
  Or   :: (BoolLike a) => FreeCat (a, a) a
  Not  :: (BoolLike a) => FreeCat a a
  T    :: (BoolLike a) => FreeCat b a
  F    :: (BoolLike a) => FreeCat b a
  --IfThenElse :: (BoolLike test) => FreeCat a test -> FreeCat b c -> FreeCat b c -> FreeCat b c --
  IfTE :: (BoolLike test) => FreeCat a test ->  (FreeCat b c, FreeCat b c) -> FreeCat (FreeCat a test, (FreeCat b c, FreeCat b c)) (FreeCat b c)

deriving instance Show (FreeCat a b)


eval :: FreeCat a b -> (a -> b)
eval (Comp f g)   = eval f . eval g
eval (Par f g)    = parC (eval f) (eval g)
eval Id           = id
eval Fst          = fst
eval Snd          = snd
eval Dup          = dupC
eval And          = andC
eval Or           = orC
eval Not          = notC
eval T            = const true
eval F            = const false
eval (IfTE t (f,g)) = undefined --ifTE (t, (f,g))





test :: IO ()
test = do
  print $ eval And (True,True)
  print $ eval Not False
  print $ eval Or (True,False)
 -- print $ eval $ IfThenElse T Id Id