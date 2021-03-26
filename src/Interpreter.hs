{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter where

import           Cat
import           FreeCat
import           Hask    ()

fix :: (a -> a) -> a
fix f = let x = f x in x

red :: FreeCat a1 (a2 -> a2) -> a1 -> a2
red term arg = fix $ eval term arg

eval :: FreeCat a b -> (a -> b)
eval (Comp f g)   = eval f . eval g
eval (Par f g)    = parC (eval f) (eval g)
eval (Curry f)    = Wrap . curry (eval f)
eval (Uncurry f)  = error "not yet implemented" -- _f (interp f)
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
eval AddCurry     = \x -> Wrap (x +) -- just an experiment not really needed
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
eval IfThenElse   = ifTE
