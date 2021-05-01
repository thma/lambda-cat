{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

-- :set -XDataKinds
-- 


module Proofs where

data Boolean = 'Tru | 'Fls  

data SBool (a :: Boolean) where
  STrue :: SBool 'Tru
  SFalse :: SBool 'Fls
  
deriving instance Show (SBool a)

type family And a b where
  And 'Tru 'Tru = 'Tru
  And 'Tru 'Fls = 'Fls
  And 'Fls 'Tru = 'Fls
  And 'Fls 'Fls = 'Fls

type family Not a where
  Not 'Tru = 'Fls
  Not 'Fls = 'Tru

type family Or a b where
  Or 'Tru 'Tru = 'Tru
  Or 'Tru 'Fls = 'Tru
  Or 'Fls 'Tru = 'Tru
  Or 'Fls 'Fls = 'Fls
  
-- Equality
data a :~: b where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "QED"

transitivity :: (a :~: b) -> (b :~: c) -> (a :~: c)
transitivity Refl Refl = Refl

symmetry :: (a :~: b) -> (b :~: a)
symmetry Refl = Refl


calculateAnd :: SBool (And 'Tru 'Fls)
calculateAnd = SFalse   