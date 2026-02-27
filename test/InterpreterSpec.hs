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
    -- Fix takes a morphism (a -> a) and returns the fixpoint
    it "computes countdown via fix" $
      let countdownStep :: FreeCat Integer Integer -> FreeCat Integer Integer
          countdownStep rec = Lift $ \n -> if n Prelude.== 0 then 0 else interp rec (n - 1)
          -- interp Fix returns a FreeCat, so we need interp again to get a function
          countdown = interp (interp Fix (Lift countdownStep))
      in countdown 10 `shouldBe` 0

    it "computes factorial via fix" $
      let facStep :: FreeCat Integer Integer -> FreeCat Integer Integer
          facStep rec = Lift $ \n -> if n Prelude.== 0 then 1 else n * interp rec (n - 1)
          factorial = interp (interp Fix (Lift facStep))
      in factorial 5 `shouldBe` 120

    it "computes fibonacci via fix" $
      let fibStep :: FreeCat Integer Integer -> FreeCat Integer Integer
          fibStep rec = Lift $ \n ->
            if n Prelude.== 0 then 0
            else if n Prelude.== 1 then 1
            else interp rec (n - 1) + interp rec (n - 2)
          fib = interp (interp Fix (Lift fibStep))
      in fib 10 `shouldBe` 55