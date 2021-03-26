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

module CCC (
     toCCC )where

import           Cat
import           Control.Category
import           Prelude          hiding (id, (.))


-- not IsTup anymore. IsArrTup
class IsTup a b | a -> b
instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a,b) c
instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a -> b) c
instance {-# INCOHERENT #-} (b ~ 'False) => IsTup a b


-- class IsCurry a b | a -> b
-- instance {-# INCOHERENT #-} (d ~ 'True) => IsCurry (a -> (b -> c)) d
-- instance {-# INCOHERENT #-} (b ~ 'False) => IsCurry a b

-- Use the V and refresh the parametric variables. V a b = V {unV :: a} -- do I have to actually store b?
-- not using datakind because getting the kinds to work out was annoying
data Left a
data Right a

class EitherTree index input out | index out -> input where
    inj :: input  -> out -- EitherFromFanTree b
    ext :: out -> input

instance (EitherTree a b o, out ~ (Either o q)) => EitherTree (Left a) b out where
   inj x = Left (inj @a @b x)
   ext (Left x) = ext @a @b x
   ext _        = error "Tried to extract left"

instance (EitherTree a b o, out ~ (Either q o)) => EitherTree (Right a) b out where
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

{- for the injection of categorical literals, we also need to make Either tree a Category.
-- might need a newtype wrapper to carry a and b through
-- ah. This might fix the Num problem too.
EitherCat is a categry
If k is a NumCat, then EitherCat is
-}

-- maybe this needs to be a typeclass with unification?
{-
type family EitherCat' tag tree k a b where
   EitherCat' (Left a) (Either l r) k a b = EitherCat' a l k a b
   EitherCat' Right a ... = a l
   EitherCat () x k a b   =    x ~ (k a b)

-- These compile, but they are wrong. You'll not be able to use them as expected.

newtype EitherCat l r tag a b = EitherCat (Either l r)
-- newtype EitherCat l r tag a b = EitherCat (Either l r)

instance (Category k, EitherTree tag (k a b) (Either l r), -- No this will never work. l and r are different for all the terms.
     EitherTree tag (k b c) (Either l r), -- we need a way to pass down the important a and b
     EitherTree tag (k a c) (Either l r),
     EitherTree tag (k d d) (Either l r) ) => Category (EitherCat l r tag) where
    (EitherCat f) . (EitherCat g) = EitherCat (inj @tag @(k a c) ((ext @tag @(k b c) f) . (ext @tag @(k a b) g)))
    id =  EitherCat (inj @tag (id @k @d)) -- will this compile?
-}
-- instance Cartesian




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
toCCC f = toCCC' @fb @() @(a -> b) @(k a' b') f

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


{-
instance (Cartesian k,
          IsTup a fa,
          IsTup b fb,
          BuildInput a fa (k a' a'),
          FanOutput fb b (k a' b')) => CCC 'False (a -> b) (k a' b') where
    toCCC' f = fanOutput @fb output where
        input = (buildInput @a @fa (idC @k @a'))
        output = f input

-}




-- does path actually need to be here? Maybe it does. because we need to be able to extract from it or not
class BuildInput tup (flag :: Bool) fanindex path where
    buildInput :: path -> tup
    -- buildInput :: forall k. Cartesian k => k a b -> tup

instance (Cartesian k,
          IsTup a fa,
          IsTup b fb,
          BuildInput a fa ind (k x a'),
          BuildInput b fb ind (k x b'),
          ((k x (a',b')) ~ cat)) =>  BuildInput (a,b) 'True ind cat where
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
    buildInput path = \x -> let path' = applyC . (fanC path (fanOutput @fa x)) in buildInput @b @fb @ind path'


instance (Category k,  --,
         EitherTree ind b a, --
         b ~ k a' b') => BuildInput a 'False ind b where
    buildInput path = inj @ind path

{-
class BuildInputArr (flag :: Bool) arr where
    buildArr :: path -> arr
instance BuildInputArr 'True (a -> b) where -- toCCC x?
    buildArr path = \x -> let path' = applyC . (fanC path (autoUncurry x)) in buildInput @b path'
-}

-- 'a' could be a tuple value, or it could be an arrow value. or a raw morphism
-- seperate type classe instances? for all of them?



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

{-
class DestructOutput (flag :: Bool) out cat path | out flag path -> cat where
    destructOutput :: path -> out -> cat

instance DesctructOutput 'True (a -> b) cat path where
    destructOutput p f = destructOutput (post . curryC) (sndC . pre)  output where
        input = buildInput @a @fa (fstC . p)
        output = f input

-}


{-
toCCC :: forall k a b a' b' fa fb x. (IsTup a fa, IsTup b fb, Cartesian k, BuildInput a fa (k a' a'), FanOutput fb b (k a' b')) => (a -> b) -> k a' b'
toCCC f = fanOutput @fb output where
                                      input = buildInput @a @fa (idC @k @a')
                                      output = f input

class AutoUncurry (flag :: Bool) a b | a flag -> b where
    autoUncurry :: a -> b

instance ( IsCurry c f,
           AutoUncurry f ((a,b) -> c) d) => AutoUncurry 'True (a -> (b -> c)) d where
   autoUncurry f = autoUncurry @f (uncurry f) -- postprocess' = curryC . postprocess

instance (a ~ d) => AutoUncurry 'False a d where
   autoUncurry f = f -- Actually recurse into BuildTup and FanOutput here. and then apply post processing
-- autoUncurry post f = post (f buildInput)
-- autoUncurry post f = (post, f) -- to be put together later after fanning
-- toCCC' path post f
-- positive and negative position, do different thing,
-- for positive tuple, fan
-- for positive arrow, uncrry
-- for negative arrow, apply
-- for negative tuple, path
-}
-- (( -> ) -> ) -> .
-- means that the function is going to give us another function, which we'll have to build the input for
-- That's recursive toCCC call? Or partial toCCC without postprocessing.

-- combine BuildInputTup and BuildInputArr into single typeclass
-- think about it in terms of  k a b -> (, , )



-- autoCPS. CPS in the category layer? forall b. k (k a b) b

-- what if output is (a, a -> b)? -> ( a , k a b). I guess we call toCCC on it again? but we need
 {-   (a -> (b -> a)
    curry fstC
    parC i -}
