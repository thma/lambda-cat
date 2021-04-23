{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           CCC
import           Cat
import           Control.Category
import           Data.Data
import           Data.Generics.Aliases
import           FreeCat
import           Interpreter
import           Prelude               hiding (id, pred, succ, (&&), (.), (==))
import           Rewrite

mapTuple :: (Data a, Typeable b) => (b -> b) -> a -> a
mapTuple f = gmapT (mkT f)

--
main :: IO ()
main = do
  print example1
  print example2
  print example3
  print example4
  print example5
  print example6
  print example7
  print example8
  print example9
  print example10
  print example11
  print example12
  print example13
  print example14
  print example15
  print example16
  print example17
  print example18
  print example19
  print example21
  print example22
  print example23
  print example24
  print example25
  print example26

  --print fac

  let x = eval fac 10
  print 10

  mains

--}

example2 :: FreeCat (a, b) (b, a)
example2 = simplify $ toCCC (\(x, y) -> (y, x))

-- You need to give the type signature unfortunately. k is too ambiguous otherwise
-- example3 :: Cartesian k => k _ _
example3 :: FreeCat (b'1, b'2) (b'1, b'1)
example3 = simplify $ toCCC (\(z, y) -> (z, z))

example4 = simplify $ toCCC (\((x, y), z) -> x)

example5 = simplify $ toCCC (\(x, y) -> x + y)

example6 = simplify $ toCCC (\(x, y) -> (y + (x * y), x * y))

-- example7 :: Cartesian k => k _ _
example7 = simplify $ toCCC (\(x, (y, z)) -> (x, (y, z)))

myconst = \x -> \y -> x

example8 = simplify $ toCCC myconst -- const -- (\x -> \y -> x)

example9 = let f = (\x y -> x) in toCCC @FreeCat f

example10 = simplify $ toCCC (\x -> x)

example11 = simplify $ toCCC f where f = (\x y -> y)

example11b = simplify $ toCCC const

--example12 :: Cartesian k => k (k a b) b
example12 = simplify $ toCCC ((\x y -> y) :: a -> b -> b)

-- the following incorrectly fails. Early picking of incoherent instance seems to send it into case 3 of CCC rather than curry case 2.
-- This isn't producing incorrect code, but it does suck.
--doesnotwork =  simplify $ toCCC (\x y -> y)

-- Even this is fine
--example16 =  toCCC ((\x y -> y)) -- :: _ -> _ -> _)
example12' = simplify $ toCCC (\x -> (x, x))

example13 = simplify $ toCCC (\x y -> (x, y))

example14 = simplify $ toCCC f where f = (\x y z -> z)

example15 = simplify $ toCCC f where f = (\x y z -> x)

example13' = simplify $ toCCC (\(x, y) -> y)

example1 = simplify $ toCCC id

example16 = simplify $ toCCC (+)

example17 = simplify $ toCCC (*)

-- fails. appears to be another inocherent hiccup. ($) is weird anyway
-- example18 = simplify $ toCCC ($)

example18 = simplify $ toCCC f where f = \g x -> g x

example19 = simplify $ toCCC (\(g, x) -> g x)

-- fails confusingly. This might mean something is fundmanetally wrong somehwere.
-- This may be failing because it tries to toCCC const as an itermediate step, which we've already
-- seens is tempermental
-- example20 = simplify $ toCCC f  where f = (\x -> (x, \y -> x))
helper = (\x -> (x, snd)) -- where f = \y -> y

example20' = simplify $ toCCC helper -- This one came out Comp (Par Id Id) Dup = Dup
-- That isn't right. It should be Curry (Id) = Curry (Par Id Id)
-- What is this? k a (a, k b b) = Id
-- fanC Id (Curry Snd) = (Par Id (Curry Snd)) .  Dup

-- This was a weird bug due to using id. Which was the catgoerical id
-- No the problme is when I fan, I seperate the two type variables, but they are still connected
-- And I try to unify them into different extractor morphism types.
--- oh dear
-- Fixed with bizarre EitherTreem work around

-- can't tell if this one is correct. It is too big. revisit when I have optimizations
example21 = simplify $ toCCC f where f = \h g x -> h g x

-- you can throw catagorocial literals in there if you want
-- Edit: not anymore :( ...
-- Wait, why is this working?
-- And the Num stuff still works.
-- It's because we have no fans.
example22 = simplify $ toCCC (\x -> Id . x)

-- example22' = simplify $ toCCC (\x -> (Id . x, Id . x)) -- This doesn't work
example23 = simplify $ toCCC (\(x, y) -> Dup . x)

-- could define helper functions with preapplied (.). dup = (.) Dup
-- then (\x -> dup x) looks more nautral
example24 = simplify $ toCCC (\(x, y) -> dup x) where dup = (.) Dup

example25 = simplify $ toCCC (\(x, y) -> (x, y))

example26 = simplify $ toCCC (\(x, (y, z)) -> (y, z))

-- example27 = simplify $ toCCC (
-- or perhaps  f $$ x = applyC . (fanC f x). This makes sense in that f and x are extractors.
-- And then.
-- \x -> mysquare x.

example28 = simplify $ toCCC (+)

fac = simplify $ toCCC f where f = \n -> if n == 0 then 1 else n * f (n -1)

example29 = simplify . toCCC $ \x -> if x < 10 then 0 else 1

test :: (Eq a, Num a) => a -> a
test 4 = 1
test _ = 0

example30 = simplify . toCCC test


--------------------

--fact = fix (\f n -> (isZero n) 1 (* n (f (n - 1))))

iff :: (BoolLike b, EqLike b Bool) => (b,p,p) -> p
iff (test,r,f) = if test == true then r else f

--cIf :: BoolLike a => FreeCat a (FreeCat b (FreeCat b b))
--cIf :: FreeCat a' (FreeCat b' (FreeCat b' b'))
--cIf = simplify $ toCCC iff

isZero :: (EqLike a b, Num a, BoolLike  b) => p -> a -> b
isZero x = (== 0)

cIsZero :: (EqLike (FreeCat (a', b') b') (FreeCat (a', b') c'), Num b') => FreeCat a' (FreeCat b' c')
cIsZero = simplify $ toCCC isZero

isTrue :: BoolLike a => a -> a
isTrue x = true && x

cIsTrue :: BoolLike a => FreeCat a a
cIsTrue = simplify $ toCCC isTrue

cFix :: FreeCat (FreeCat a' a') a'
cFix = simplify $ toCCC fix

cAnd :: BoolLike b' => FreeCat (b', b') b'
cAnd = simplify $ toCCC (uncurry (&&))

--fact :: (EqlLike p p, Num p) => p -> p
--fact :: (BoolLike b, EqLike b Bool, EqLike p b, Num p) => p -> p
--fact = fix (\rec n -> iff (n == 0) 1 (n * rec (n -1)))

--check (x,y) = iff (x == 3) x y

--cFact :: (Ord (FreeCat a' a'), Num a') => FreeCat a' a'
--cFact = simplify $ toCCC fact


eql :: EqLike a b => (a, a) -> b
eql (x,y) = x == y

--cEqual :: (BoolLike a, EqLike (b, b) (FreeCat (b, b) a)) => FreeCat (b, b) a
--cEqual :: EqLike (FreeCat (b', b') b') (FreeCat (b', b') b) => FreeCat (b', b') b
cEqual :: EqLike (FreeCat (b', b') b') (FreeCat (b', b') b) => FreeCat (b', b') b
cEqual = simplify $ toCCC eql

is0 :: (BoolLike b, Num a, EqLike a b) => a -> b
is0 x = x == 0

cIs0 :: (BoolLike b, Num a, EqLike a (FreeCat a b), EqLike a b) => FreeCat a b
cIs0 = simplify $ toCCC is0

pair :: (Integer, Integer)
pair = (23, 23)

simple :: (Num a, EqLike a Bool, Eq a) => a -> a
simple 1 = 1
simple _ = 23

cSimple :: (Num a, EqLike (FreeCat a a) Bool) => FreeCat a a
cSimple = simplify $ toCCC simple

mains :: IO ()
mains = do
  print (is0 (78 :: Integer) :: Bool)
  print (eval (cIs0 :: FreeCat Integer Bool) 8)

  print (eval cIs0 (0::Integer) :: Bool)

  print (eval cEqual pair :: Bool)

  print (cIsTrue :: FreeCat Bool Bool)

  print (eval cSimple 1 :: Integer)


area :: Fractional a => (a, a) -> a
area (x,y) = (x-2)/2 + y

i :: a -> a
i x = x

k :: a -> b -> a
k y _ = y

s :: (a -> b -> c) -> (a -> b) -> a -> c
s p q x = p x (q x)  

cId :: a -> a
cId = s k k

--y=s s k (s(k(s s(s(s s k))))k)