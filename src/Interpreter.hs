{-- This module exposes a function interp that takes a (FreeCat a b) expression as input and returns a function
    of type (a -> b) which is the semantic interpretation of the CCC expression in the (->) category.

    > cccFst = simplify $ toCCC (\(x, y) -> x)
    > cccFst
    Fst
    > :t cccFst
    cccFst :: FreeCat (a, b) a
    > fnFst = interp cccFst
    > :t fnFst
    fnFst :: (a, b) -> a
    > fnFst ("hello", "world")
    "hello"

--}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter (interp) where

import           Cat     (BoolCat (andC, ifTE, notC, orC),
                          BoolLike (false, true), Cartesian (dupC),
                          EqCat (eqlC), Monoidal (parC),
                          NumCat (addC, geqC, greC, leqC, lesC, mulC, subC),
                          applyC)
import           FreeCat (FreeCat (..))
import           Hask    ()

interp :: FreeCat a b -> (a -> b)
interp (Comp f g)   = interp f . interp g
interp (Par f g)    = parC (interp f) (interp g)
interp (Curry f)    = Lift . curry (interp f)
interp (Uncurry f)  = error "not yet implemented" -- _f (interp f)
interp Apply        = uncurry interp
interp Id           = id
interp (IntConst i) = const i
interp FromInt      = fromInteger
interp Fst          = fst
interp Snd          = snd
interp Dup          = dupC
interp Add          = addC
interp Sub          = subC
interp Abs          = abs
interp Neg          = negate
interp Mul          = mulC
interp (Lift f)     = f
interp Eql          = eqlC
interp Leq          = leqC
interp Geq          = geqC
interp Les          = lesC
interp Gre          = greC
interp And          = andC
interp Or           = orC
interp Not          = notC
interp T            = const true
interp F            = const false