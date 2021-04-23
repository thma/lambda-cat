{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Interpreter where

import Cat
    ( EqCat(eqlC),
      BoolCat(ifTE, andC, orC, notC),
      NumCat(greC, addC, subC, mulC, leqC, geqC, lesC),
      BoolLike(false, true),
      Monoidal(parC),
      Cartesian(dupC) )
import FreeCat ( FreeCat(..) )
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
--eval IfThenElse   = \(test, (f,g)) -> Id

p x = do
  printIt x
  putStr "\n"

printIt :: FreeCat a b -> IO ()
printIt (Comp f g)   = do 
                          putStr "Comp ("
                          printIt f 
                          putStr " "
                          printIt g
                          putStr ")"
printIt (Par f g)    = do
                          putStr "Par ("
                          printIt f 
                          putStr " "
                          printIt g
                          putStr ")"
printIt (Curry f)    = do
                          putStr "Curry "
                          printIt f 
printIt (Uncurry f)  = do
                          putStr "Uncurry "
                          printIt f 
printIt Apply        = putStr "Apply "
printIt Id           = putStr "Id "
printIt (IntConst i) = putStr $ "IntConst " ++ show i
printIt FromInt      = putStr "FromInt "
printIt Fst          = putStr "Fst "
printIt Snd          = putStr "Snd "
printIt Dup          = putStr "Dup "
printIt Add          = putStr "Add "
printIt Sub          = putStr "Sub "
printIt Abs          = putStr "Abs "
printIt Neg          = putStr "Neg "
printIt Mul          = putStr "Mul "
printIt (Wrap f)     = putStr $"Wrap " ++ show f
printIt Eql          = putStr "Eql "
printIt Leq          = putStr "Leq "
printIt Geq          = putStr "Geq "
printIt Les          = putStr "Les "
printIt Gre          = putStr "Gre "
printIt And          = putStr "And "
printIt Or           = putStr "or "
printIt Not          = putStr "Not "
printIt T            = putStr "T "
printIt F            = putStr "F "