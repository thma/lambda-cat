{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-- This module exposes a compilation function toCCC, which takes a function as input 
    and returns a closed cartesian category representation of that function.

    In a typical use case you will use the GADT data type CatExpr as a compilation target:

    > toCCC @CatExpr (\(x, y) -> x)
    Comp Fst Id
--}

module CCC (toCCC) where

import           Cat
import           Prelude hiding (id, (.))

class IsTup a b | a -> b

instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a, b) c

instance {-# INCOHERENT #-} (c ~ 'True) => IsTup (a -> b) c

instance {-# INCOHERENT #-} (b ~ 'False) => IsTup a b

data Left a

data Right a

-- Local sum used during symbolic input routing in toCCC.
-- Keeping this local avoids needing orphan instances for Prelude.Either.
data Branch a b = BLeft a | BRight b

class EitherTree index input out | index out -> input where
  inj :: input -> out -- EitherFromFanTree b
  ext :: out -> input

instance (EitherTree a b o, out ~ Branch o q) => EitherTree (Left a) b out where
  inj x = BLeft (inj @a @b x)
  ext (BLeft x) = ext @a @b x
  ext _        = error "Tried to extract left"

instance (EitherTree a b o, out ~ Branch q o) => EitherTree (Right a) b out where
  inj x = BRight (inj @a @b x)
  ext (BRight x) = ext @a @b x
  ext _         = error "Tried to extract Right"

instance (b ~ out) => EitherTree () b out where
  inj x = x
  ext x = x

instance (Num b, Num a) => Num (Branch a b) where
  -- Arithmetic is only meaningful when both operands refer to the same route.
  -- Cross-route arithmetic has no coherent interpretation in this routing model.
  (BLeft f) + (BLeft g)   = BLeft (f + g)
  (BRight f) + (BRight g) = BRight (f + g)
  _ + _                   = error "Num Branch: mixed branches are unsupported"
  (BLeft f) * (BLeft g)   = BLeft (f * g)
  (BRight f) * (BRight g) = BRight (f * g)
  _ * _                   = error "Num Branch: mixed branches are unsupported"
  negate (BLeft x)        = BLeft (negate x)
  negate (BRight x)       = BRight (negate x)
  (BLeft f) - (BLeft g)   = BLeft (f - g)
  (BRight f) - (BRight g) = BRight (f - g)
  _ - _                   = error "Num Branch: mixed branches are unsupported"
  abs (BLeft x)           = BLeft (abs x)
  abs (BRight x)          = BRight (abs x)
  signum (BLeft x)        = BLeft (signum x)
  signum (BRight x)       = BRight (signum x)
  -- Literals do not carry routing information, so we choose BLeft as a deterministic
  -- default embedding. This keeps numeric desugaring total without introducing extra
  -- constraints or ambiguous branch selection at call sites.
  fromInteger n           = BLeft (fromInteger n)

type family Reverse a b where
  Reverse (Left a) b = Reverse a (Left b)
  Reverse (Right a) b = Reverse a (Right b)
  Reverse () b = b

class CCC (flag :: Bool) fanindex input out | flag fanindex input -> out where --
  toCCC' :: input -> out

-- toCCC reduces to the case of (stuff) -> single thing that is not -> or (,) curry and fan
toCCC ::
  forall k a b a' b' fb.
  ( Category k,
    CCC fb () (a -> b) (k a' b'),
    IsTup b fb
  ) =>
  (a -> b) ->
  k a' b'
toCCC = toCCC' @fb @() @(a -> b) @(k a' b')

instance
  ( Cartesian k,
    IsTup b fb,
    IsTup c fc,
    CCC fb (Left ind) (a -> b) (k a' b'),
    CCC fc (Right ind) (a -> c) (k a' c')
  ) =>
  CCC 'True ind (a -> (b, c)) (k a' (b', c'))
  where
  toCCC' f = fanC (toCCC' @fb @(Left ind) (fst . f)) (toCCC' @fc @(Right ind) (snd . f))

-- curry and then uncurry result
instance
  ( Closed k,
    IsTup c fc,
    CCC fc ind ((a, b) -> c) (k (a', b') c')
  ) =>
  CCC 'True ind (a -> (b -> c)) (k a' (k b' c'))
  where
  toCCC' f = curryC (toCCC' @fc @ind (uncurry f))

-- base case actually builds the input once the output cannot be detructed more
-- input can be anything, arrow tuple or polymorphic. Output has to be polymorphic
instance
  ( Cartesian k,
    IsTup a fa,
    BuildInput a fa ind' (k a' a'),
    ind' ~ Reverse ind (),
    EitherTree ind' (k a' b') b -- (k a' b') ~ b
  ) =>
  CCC 'False ind (a -> b) (k a' b')
  where
  toCCC' f = ext @ind' (f input)
    where
      input = buildInput @a @fa @ind' (idC @k @a')

-- does path actually need to be here? Maybe it does. because we need to be able to extract from it or not
class BuildInput tup (flag :: Bool) fanindex path where
  buildInput :: path -> tup

instance
  ( Cartesian k,
    IsTup a fa,
    IsTup b fb,
    BuildInput a fa ind (k x a'),
    BuildInput b fb ind (k x b'),
    (k x (a', b') ~ cat)
  ) =>
  BuildInput (a, b) 'True ind cat
  where
  buildInput path = (buildInput @a @fa @ind patha, buildInput @b @fb @ind pathb)
    where
      patha = fstC . path
      pathb = sndC . path

instance
  ( Closed k,
    cat ~ k x (k a' b'), -- cat extract morphisms from input tuple
    FanOutput fa a cat',
    cat' ~ k x a', -- ? Is this acceptable?
    cat'' ~ k x b', -- the type of path'
    IsTup b fb,
    IsTup a fa,
    BuildInput b fb ind cat''
  ) =>
  BuildInput (a -> b) 'True ind cat -- toCCC x?
  -- path is location of input morphism in question inside of tuple
  -- x may be a tuple to be deconstructed
  -- or x may be arrow to be toCCC ed
  where
    buildInput path x = 
      let path' = applyC . fanC path (fanOutput @fa x) 
      in buildInput @b @fb @ind path'

instance
  ( Category k, --,
    EitherTree ind b a, --
    b ~ k a' b'
  ) =>
  BuildInput a 'False ind b
  where
  buildInput = inj @ind

-- Does FanOput even need the flag?
-- isn't it all directed now?
-- it doesn't need the incoherent version. A regular overlapping instance.
class FanOutput (flag :: Bool) out cat | out flag -> cat where
  fanOutput :: out -> cat

instance
  ( Category k,
    IsTup b fb,
    CCC fb () (a -> b) (k a' b')
  ) =>
  FanOutput 'True (a -> b) (k a' b')
  where
  fanOutput = toCCC' @fb @()

instance (Category k, kab ~ k a b) => FanOutput 'False kab (k a b) where
  fanOutput f = f

instance
  ( Cartesian k,
    IsTup a fa,
    IsTup b fb,
    FanOutput fa a (k x a'),
    FanOutput fb b (k x b'),
    k x (a', b') ~ cat
  ) =>
  FanOutput 'True (a, b) cat
  where
  fanOutput (x, y) = fanC (fanOutput @fa x) (fanOutput @fb y)
