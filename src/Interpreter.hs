{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-- This module exposes a function eval that takes a (FreeCat a b) expression as input and returns a function
    of type (a -> b) which is the semantic interpretation of the CCC expression in the (->) category.

    > cccFst = simplify $ toCCC (\(x, y) -> x)
    > cccFst
    Fst
    > :t cccFst
    cccFst :: FreeCat (a, b) a
    > fnFst = eval cccFst
    > :t fnFst
    fnFst :: (a, b) -> a
    > fnFst ("hello", "world")
    "hello"

--}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter (eval, ev, fix) where

import           Cat     (BoolCat (andC, ifTE, notC, orC),
                          BoolLike (false, true), Cartesian (dupC),
                          EqCat (eqlC), Monoidal (parC),
                          NumCat (addC, geqC, greC, leqC, lesC, mulC, subC),
                          applyC)
import           FreeCat (FreeCat (..))
import           Hask    ()

fix :: (a -> a) -> a
fix f = let x = f x in x

-- red :: FreeCat a1 (a2 -> a2) -> a1 -> a2
-- red term arg = fix $ eval term arg



ev :: FreeCat a (FreeCat b c) -> (a -> b -> c)
ev (Curry f)    = curry (eval f)


eval :: FreeCat a b -> (a -> b)
eval (Comp f g)   = eval f . eval g
eval (Par f g)    = parC (eval f) (eval g)
eval (Curry f)    = Wrap . curry (eval f)
eval (Uncurry f)  = error "not yet implemented" -- _f (eval f)
eval Apply        = uncurry eval
eval Id           = id
eval (IntConst i) = const i
eval FromInt      = fromInteger
eval Fst          = fst
eval Snd          = snd
eval Dup          = dupC
eval Add          = addC
eval Sub          = subC
eval Abs          = abs
eval Neg          = negate
eval Mul          = mulC
eval (Wrap f)     = f
eval Eql          = eqlC
eval Leq          = leqC
eval Geq          = geqC
eval Les          = lesC
eval Gre          = greC
eval And          = andC
eval Or           = orC
eval Not          = notC
eval T            = const true
eval F            = const false

--eval IfThenElse   = \(test, (f,g)) -> Id
