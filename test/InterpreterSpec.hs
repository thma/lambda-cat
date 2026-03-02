{-# LANGUAGE NoImplicitPrelude #-}

module InterpreterSpec where

import           CCC
import           Cat
import           FreeCat
import           Interpreter
import           Prelude         (Bool (..), Double, Float, Int, Num, Integer, abs,
                                  negate, uncurry, ($), (&&), (*), (+), (-),
                                  (==), show)
import           Rewrite
import           Test.Hspec
import           Test.QuickCheck

idCCC :: FreeCat Int Int
idCCC = simplify . toCCC $ id

addCCC :: FreeCat (Integer, Integer) Integer
addCCC = simplify . toCCC $ uncurry (+)

mulCCC :: FreeCat (Double, Double) Double
mulCCC = simplify . toCCC $ uncurry (*)

subCCC :: FreeCat (Integer, Integer) Integer
subCCC = simplify . toCCC $ uncurry (-)

negCCC :: FreeCat Int Int
negCCC = simplify . toCCC $ negate

absCCC :: FreeCat Float Float
absCCC = simplify . toCCC $ abs

example6 :: FreeCat (Integer, Integer) (Integer, Integer)
example6 = simplify $ toCCC (\(x, y) -> (y + (x * y), x * y))

add2CCC :: FreeCat Integer Integer
add2CCC = simplify $ toCCC (2 +)

isTrueCCC :: FreeCat Bool Bool
isTrueCCC = simplify $ toCCC (true Cat.&&)


is0CCC :: FreeCat Integer Bool 
is0CCC = simplify $ toCCC (Cat.== 0)

i :: a -> a
i x = x

k :: a -> b -> a
k y _ = y

s :: (a -> b -> c) -> (a -> b) -> a -> c
s p q x = p x (q x)  

idCCC' :: FreeCat Int Int
idCCC' = simplify $ toCCC (s k k)

spec :: Spec
spec = do
  describe "The CCC Interpreter" $ do
    it "interpretes the id function" $
      property $ \x -> interp idCCC x `shouldBe` x
    it "interpretes addition" $
      property $ \x y -> interp addCCC (x, y) `shouldBe` x + y
    it "interpretes multiplication" $
      property $ \x y -> interp mulCCC (x, y) `shouldBe` x * y
    it "interpretes substraction" $
      property $ \x y -> interp subCCC (x, y) `shouldBe` x - y
    it "interpretes negation" $
      property $ \x -> interp negCCC x `shouldBe` negate x
    it "interpretes absolute" $
      property $ \x -> interp absCCC x `shouldBe` abs x
    it "interpretes combination of + and *" $
      property $ \x y -> interp example6 (x, y) `shouldBe` (\(a, b) -> (b + (a * b), a * b)) (x, y)
    it "interpretes partial evaluated functions" $
      property $ \x -> interp add2CCC x `shouldBe` x + 2
    it "interpretes partial evaluated functions with booleans" $
      property $ \x -> interp isTrueCCC x `shouldBe` x Prelude.&& True
    it "checks equality on numbers" $
      property $ \x -> interp is0CCC x `shouldBe` x Prelude.== 0
    it "can compile combinatory Logic (S K K) = id" $
       property $ \x -> interp idCCC' x `shouldBe` x
    it "can evaluate K" $   
       property $ \x -> interp (interp (toCCC k) x) 8 `shouldBe` (x :: Integer)
    it "can evaluate I" $
       property $ \x -> interp (toCCC i) x `shouldBe` (x :: Integer)

    -- Tests for Uncurry
    it "interprets uncurried functions" $
      let curriedAdd = toCCC (+) :: FreeCat Integer (FreeCat Integer Integer)
          uncurriedAdd = Uncurry curriedAdd
      in property $ \x y -> interp uncurriedAdd (x, y) `shouldBe` x + y

    -- Tests for IfThenElse
    it "interprets if-then-else with True" $
      let test = interp IfThenElse (True, (Add, Sub))
      in interp test (3, 2) `shouldBe` 5

    it "interprets if-then-else with False" $
      let test = interp IfThenElse (False, (Add, Sub))
      in interp test (3, 2) `shouldBe` 1

    -- Tests for Fix (recursive functions)
    -- Fix now takes a categorical step function: FreeCat (FreeCat a b, a) b
    -- The step function receives (rec, input) as a pair and produces the result
    it "computes countdown via fix" $
      let -- Step: given (rec, n), produce: if n==0 then 0 else rec(n-1)
          -- Input type: (FreeCat Integer Integer, Integer)
          isZero = Comp Eql (fanC Snd (Comp (IntConst 0) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Bool
          thenVal = Comp (IntConst 0) Snd :: FreeCat (FreeCat Integer Integer, Integer) Integer
          -- rec(n-1): Apply . (rec, n-1)
          decN = Comp Sub (fanC Snd (Comp (IntConst 1) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          elseVal = Comp Apply (fanC Fst decN) :: FreeCat (FreeCat Integer Integer, Integer) Integer

          countdownStep :: FreeCat (FreeCat Integer Integer, Integer) Integer
          countdownStep = Comp IfVal (fanC isZero (fanC thenVal elseVal))

          countdown = Fix countdownStep
      in interp countdown 10 `shouldBe` 0

    it "computes factorial via fix" $
      let -- Step: given (rec, n), produce: if n==0 then 1 else n * rec(n-1)
          isZero = Comp Eql (fanC Snd (Comp (IntConst 0) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Bool
          thenVal = Comp (IntConst 1) Snd :: FreeCat (FreeCat Integer Integer, Integer) Integer
          -- n * rec(n-1)
          n = Snd :: FreeCat (FreeCat Integer Integer, Integer) Integer
          decN = Comp Sub (fanC Snd (Comp (IntConst 1) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          recDecN = Comp Apply (fanC Fst decN) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          elseVal = Comp Mul (fanC n recDecN) :: FreeCat (FreeCat Integer Integer, Integer) Integer

          facStep :: FreeCat (FreeCat Integer Integer, Integer) Integer
          facStep = Comp IfVal (fanC isZero (fanC thenVal elseVal))

          factorial = Fix facStep
      in interp factorial 5 `shouldBe` 120

    it "computes fibonacci via fix" $
      let -- Step: given (rec, n), produce: if n==0 then 0 else if n==1 then 1 else rec(n-1) + rec(n-2)
          -- We need nested conditionals, so we'll build them compositionally
          isZero = Comp Eql (fanC Snd (Comp (IntConst 0) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Bool
          isOne = Comp Eql (fanC Snd (Comp (IntConst 1) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Bool

          val0 = Comp (IntConst 0) Snd :: FreeCat (FreeCat Integer Integer, Integer) Integer
          val1 = Comp (IntConst 1) Snd :: FreeCat (FreeCat Integer Integer, Integer) Integer

          -- rec(n-1) + rec(n-2)
          dec1 = Comp Sub (fanC Snd (Comp (IntConst 1) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          dec2 = Comp Sub (fanC Snd (Comp (IntConst 2) Snd)) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          recDec1 = Comp Apply (fanC Fst dec1) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          recDec2 = Comp Apply (fanC Fst dec2) :: FreeCat (FreeCat Integer Integer, Integer) Integer
          recSum = Comp Add (fanC recDec1 recDec2) :: FreeCat (FreeCat Integer Integer, Integer) Integer

          -- Inner conditional: if n==1 then 1 else rec(n-1)+rec(n-2)
          innerCond = Comp IfVal (fanC isOne (fanC val1 recSum))
          -- Outer conditional: if n==0 then 0 else innerCond
          fibStep = Comp IfVal (fanC isZero (fanC val0 innerCond))

          fib = Fix fibStep
      in interp fib 10 `shouldBe` 55