{-# LANGUAGE NoImplicitPrelude         #-}
module InterpreterSpec where


import           Test.Hspec
import           Test.QuickCheck

import Interpreter
import Rewrite
import FreeCat
import CCC
import Cat
--import GHC.Num (Integer)
--import Data.Int (Int)
--import GHC.Base (Double, Float)
import Prelude(Integer, Int, Double, Float, (.), ($), id, (+), (*), (-), negate, abs, uncurry, Bool (..), (&&))

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

spec :: Spec
spec = do
  describe "The CCC Interpreter" $ do
    it "interpretes the id function" $
      property $ \x -> eval idCCC x `shouldBe` x
    it "interpretes addition" $
      property $ \x y -> eval addCCC (x,y) `shouldBe` x+y
    it "interpretes multiplication" $
      property $ \x y -> eval mulCCC (x,y) `shouldBe` x*y
    it "interpretes substraction" $
      property $ \x y -> eval subCCC (x,y) `shouldBe` x-y
    it "interpretes negation" $
      property $ \x -> eval negCCC x `shouldBe` negate x
    it "interpretes absolute" $
      property $ \x -> eval absCCC x `shouldBe` abs x
    it "interpretes combination of + and *" $
      property $ \x y -> eval example6 (x,y) `shouldBe` (\(a, b) -> (b + (a * b), a * b)) (x,y)
    it "interpretes partial evaluated functions" $
      property $ \x -> eval add2CCC x `shouldBe` x + 2
    it "interpretes partial evaluated functions with booleans" $
      property $ \x -> eval isTrueCCC x `shouldBe` x Prelude.&& True