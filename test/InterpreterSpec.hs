{-# LANGUAGE NoImplicitPrelude #-}

module InterpreterSpec where

import           CCC
import           Cat
import           FreeCat
import           Interpreter
import           Prelude         (Bool (..), Double, Float, Int, Num, Integer, abs,
                                  id, negate, uncurry, ($), (&&), (*), (+), (-),
                                  (.), (==), show)
import           Rewrite
import           Test.Hspec
import           Test.QuickCheck
import Control.Arrow ((&&&))

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
      property $ \x -> eval idCCC x `shouldBe` x
    it "interpretes addition" $
      property $ \x y -> eval addCCC (x, y) `shouldBe` x + y
    it "interpretes multiplication" $
      property $ \x y -> eval mulCCC (x, y) `shouldBe` x * y
    it "interpretes substraction" $
      property $ \x y -> eval subCCC (x, y) `shouldBe` x - y
    it "interpretes negation" $
      property $ \x -> eval negCCC x `shouldBe` negate x
    it "interpretes absolute" $
      property $ \x -> eval absCCC x `shouldBe` abs x
    it "interpretes combination of + and *" $
      property $ \x y -> eval example6 (x, y) `shouldBe` (\(a, b) -> (b + (a * b), a * b)) (x, y)
    it "interpretes partial evaluated functions" $
      property $ \x -> eval add2CCC x `shouldBe` x + 2
    it "interpretes partial evaluated functions with booleans" $
      property $ \x -> eval isTrueCCC x `shouldBe` x Prelude.&& True
    it "checks equality on numbers" $
      property $ \x -> eval is0CCC x `shouldBe` x Prelude.== 0
    it "can compile combinatory Logic (S K K) = id" $
       property $ \x -> eval idCCC' x `shouldBe` x
    it "can evaluate K" $   
       property $ \x -> eval (eval (toCCC k) x) 8 `shouldBe` (x :: Integer)
    it "can evaluate I" $   
       property $ \x -> eval (toCCC i) x `shouldBe` (x :: Integer)