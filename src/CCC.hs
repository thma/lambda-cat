{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module CCC (
     toCCC )where

import           Cat
import           Control.Category
import           Prelude          hiding (id, (.))

class IsTup a b | a -> b
instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a,b) c
instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a -> b) c
instance {-# INCOHERENT #-} (b ~ 'False) => IsTup a b

data Left a
data Right a

class EitherTree index input out | index out -> input where
    inj :: input  -> out -- EitherFromFanTree b
    ext :: out -> input

instance (EitherTree a b o, out ~ Either o q) => EitherTree (Left a) b out where
   inj x = Left (inj @a @b x)
   ext (Left x) = ext @a @b x
   ext _        = error "Tried to extract left"

instance (EitherTree a b o, out ~ Either q o) => EitherTree (Right a) b out where
   inj x = Right (inj @a @b x)
   ext (Right x) = ext @a @b x
   ext _         = error "Tried to extract Right"

instance (b ~ out) => EitherTree () b out where
   inj x = x
   ext x = x

instance (Num b, Num a) => Num (Either a b) where
    (Left f) + (Left g)   = Left (f + g)
    (Right f) + (Right g) = Right (f + g)
    (Left f) * (Left g)   = Left (f * g)
    (Right f) * (Right g) = Right (f * g)
    negate f = error "Todo"
    f - g = error "todo"
    abs f = error "todo"
    signum = error "TODO"
    fromInteger = error "TODO"

type family Reverse a b where
    Reverse (Left a) b = Reverse a (Left b)
    Reverse (Right a) b = Reverse a (Right b)
    Reverse () b = b
class CCC (flag :: Bool) fanindex input out  | flag fanindex input -> out where --
    toCCC' :: input -> out

-- toCCC reduces to the case of (stuff) -> single thing that is not -> or (,) curry and fan
toCCC :: forall k a b a' b' fb. (
          Category k,
          CCC fb () (a -> b) (k a' b'),
          IsTup b fb ) => (a -> b) -> k a' b'
toCCC = toCCC' @fb @() @(a -> b) @(k a' b')

instance (Cartesian k,
          IsTup b fb,
          IsTup c fc,
          CCC fb (Left ind) (a -> b) (k a' b'),
          CCC fc (Right ind) (a -> c) (k a' c')) => CCC 'True ind (a -> (b,c)) (k a' (b', c')) where
    toCCC' f = fanC (toCCC' @fb @(Left ind) (fst . f)) (toCCC' @fc @(Right ind) (snd . f))

-- curry and then uncurry result
instance (Closed k,
          IsTup c fc,
          CCC fc ind ((a,b)->c)  (k (a',b') c')
          ) => CCC 'True ind (a -> (b -> c)) (k a' (k b' c')) where
    toCCC' f = curryC (toCCC' @fc @ind (uncurry f))

-- base case actually builds the input once the output cannot be detructed more
-- input can be anything, arrow tuple or polymorphic. Output has to be polymorphic
instance (Cartesian k,
          IsTup a fa,
          BuildInput a fa ind' (k a' a'),
          ind' ~ Reverse ind (),
          EitherTree ind' (k a' b') b -- (k a' b') ~ b
          ) => CCC 'False ind (a -> b) (k a' b') where
    toCCC' f = ext @ind' (f input) where
        input = buildInput @a @fa @ind' (idC @k @a')

-- does path actually need to be here? Maybe it does. because we need to be able to extract from it or not
class BuildInput tup (flag :: Bool) fanindex path where
    buildInput :: path -> tup
    -- buildInput :: forall k. Cartesian k => k a b -> tup

instance (Cartesian k,
          IsTup a fa,
          IsTup b fb,
          BuildInput a fa ind (k x a'),
          BuildInput b fb ind (k x b'),
          (k x (a',b') ~ cat)) =>  BuildInput (a,b) 'True ind cat where
    buildInput path = (buildInput @a @fa @ind patha, buildInput @b @fb @ind pathb) where
                 patha = fstC . path
                 pathb = sndC . path


instance (Closed k,
         cat ~ k x (k a' b'), -- cat extract morphisms from input tuple
         FanOutput fa a cat',
         cat' ~ k x a', -- ? Is this acceptable?
         cat'' ~ k x b', -- the type of path'
         IsTup b fb,
         IsTup a fa,
         BuildInput b fb ind cat'') => BuildInput (a -> b) 'True ind cat where -- toCCC x?
    -- path is location of input morphism in question inside of tuple
    -- x may be a tuple to be deucosturcted
    -- or x may be arrow to be toCCC ed
    buildInput path = \x -> let path' = applyC . fanC path (fanOutput @fa x) in buildInput @b @fb @ind path'


instance (Category k,  --,
         EitherTree ind b a, --
         b ~ k a' b') => BuildInput a 'False ind b where
    buildInput path = inj @ind path

-- Does FanOput even need the flag?
-- isn't it all directed now?
-- it doesn't need the incoherent version. A regular overlapping instance.

class FanOutput (flag :: Bool) out cat where -- | out flag -> cat
    fanOutput :: out -> cat

instance (Category k,
          IsTup b fb,
          CCC fb () (a -> b) (k a' b')
    ) => FanOutput 'True (a -> b) (k a' b') where
    fanOutput f = toCCC' @fb @() f

instance (Category k, kab ~ k a b) => FanOutput 'False kab (k a b) where
    fanOutput f = f

instance (Cartesian k,
         IsTup a fa,
         IsTup b fb,
         FanOutput fa a (k x a'),
         FanOutput fb b (k x b'),
         k x (a', b') ~ cat
         )
          => FanOutput 'True (a, b) cat where
    fanOutput (x,y) = fanC (fanOutput @fa x) (fanOutput @fb y)