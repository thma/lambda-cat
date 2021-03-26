{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude  #-}

module Rewrite where

import           Control.Category ((.))
import           FreeCat
import           Prelude          hiding (id, (.))

-- a dead dumb simple rewrite system
-- it is expecially easy since our ctagoerical lnauggae has no names in it

-- even if I get GHC rewrite rules to work. this will be useful for comparison.
-- with a test suite for example. Would give us clues as to which rewrite rules are not firing.

-- we can just abuse haskell pattern matching to do most of the work for us

-- we should move the definition of FreeCat here.
-- Add a literal constructor
-- Lit :: k a b -> FreeCat a b... No this hides k....
-- well, we'l;l figure out that problem later

-- Maybe we want to check out that TTT tool.
-- dev.stephendiehl.com/rewrite.pdf

-- Either (FreeCat a b) (FreeCat a b) -- return original argyument if fails?

type Rule = forall a b. FreeCat a b -> Maybe (FreeCat a b)

-- stop using prefix rule. It is annoying
ruleParen :: Rule -- FreeCat a b -> Maybe (FreeCat a b)
ruleParen (Comp (Comp f g) h) = Just (Comp f (Comp g h))
ruleParen _                   = Nothing

ruleFstsndpar :: FreeCat a b -> Maybe (FreeCat a b)
ruleFstsndpar (Comp (Par Fst Snd) Dup) = Just Id
ruleFstsndpar _                        = Nothing

ruleFstDup :: FreeCat a b -> Maybe (FreeCat a b)
ruleFstDup (Comp Fst Dup) = Just Id
ruleFstDup _              = Nothing

ruleSndDup :: FreeCat a b -> Maybe (FreeCat a b)
ruleSndDup (Comp Snd Dup) = Just Id
ruleSndDup _              = Nothing

ruleParDup :: FreeCat a b -> Maybe (FreeCat a b)
ruleParDup (Comp (Par (Comp f Fst) (Comp g Snd)) Dup) = Just (Par f g)
ruleParDup _                                          = Nothing

ruleParDup' :: FreeCat a b -> Maybe (FreeCat a b)
ruleParDup' (Comp (Par (Comp f Fst) (Comp g Fst)) Dup) = Just (((Par f g) . Dup) . Fst)
ruleParDup' _ = Nothing

ruleParDup'' :: FreeCat a b -> Maybe (FreeCat a b)
ruleParDup'' (Comp (Par (Comp f Snd) (Comp g Snd)) Dup) = Just (((Par f g) . Dup) . Snd)
ruleParDup'' _ = Nothing

-- parC dupC" forall f. (_parC f f) . _dupC = _dupC . f
{- -- needs equality.
rule_par_dup_eq :: FreeCat a b -> Maybe (FreeCat a b)
rule_par_dup_eq (Comp (Par f f) Dup) | f == f = Dup . f
-}

-- build the curry rules.
ruleCurry :: FreeCat a b -> Maybe (FreeCat a b)
ruleCurry (Curry (Uncurry f)) = Just f
ruleCurry _                   = Nothing

ruleCurry' :: FreeCat a b -> Maybe (FreeCat a b)
ruleCurry' (Uncurry (Curry f)) = Just f
ruleCurry' _                   = Nothing

ruleCurryApply :: Rule
ruleCurryApply (Curry Apply) = Just Id
ruleCurryApply _             = Nothing

ruleIdLeft :: FreeCat a b -> Maybe (FreeCat a b)
ruleIdLeft (Comp Id f) = Just f
ruleIdLeft _           = Nothing

ruleIdRight :: FreeCat a b -> Maybe (FreeCat a b)
ruleIdRight (Comp f Id) = Just f
ruleIdRight _           = Nothing

allRules :: [Rule]
allRules =
  [ ruleFstsndpar,
    ruleIdRight,
    ruleIdLeft,
    ruleFstDup,
    ruleSndDup,
    ruleParDup,
    ruleParDup',
    ruleParDup'',
    ruleCurry,
    ruleCurry',
    ruleCurryApply,
    ruleParen
  ]

-- yeah. Easily possible to get nasty infinite loops
recurseMatch :: Rule -> FreeCat a b -> Maybe (FreeCat a b)
recurseMatch rule x = case rule x of
  Nothing -> goDown (recurseMatch rule) x -- This rule didn't match. Try going down and matching there.
  Just x' -> Just x' -- travFree rule x' -- or can keep trying while we're already there.

goDown :: Rule -> FreeCat a b -> Maybe (FreeCat a b)
goDown z (Comp f g) = case (z f) of
  Nothing -> case (z g) of
    Nothing -> Nothing
    Just x  -> Just (Comp f x)
  Just x -> Just (Comp x g)
goDown z (Par f g) = case (z f) of
  Nothing -> case (z g) of
    Nothing -> Nothing -- nothing in either subtree macthed
    Just x  -> Just (Par f x) --
  Just x -> Just (Par x g) -- something in f matched the rule
goDown z (Curry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Curry x)
goDown z (Uncurry f) = case z f of
  Nothing -> Nothing
  Just x  -> Just (Uncurry x)
goDown _ _ = Nothing -- can't go down
{-
goDown z (Par f g) = Par (z f) (z g)
goDown z Dup = Dup
goDown _ Fst = Fst
goDown _ Snd = Snd
goDown _ f = f
-}

{-
travFree rule x@(Comp f g) = case rule x of
                             Nothing -> Comp (travFree rule f) (travFree rule g)
                             Just x' -> travFree rule x'
travFree rule Id = case rule Id of
	                  Nothing -> Id
	                  Just x' -> travFree rule x'
-}
-- can recursively go down rules until onew hits, then start all over. Put common rules first.

type Rule' a b = FreeCat a b -> Maybe (FreeCat a b)

rewrite' :: [Rule] -> [Rule] -> FreeCat a b -> FreeCat a b
rewrite' _ [] k = k -- no rules matched
rewrite' allrules (rule : rules) k = case recurseMatch rule k of
  Nothing -> rewrite' allrules rules k -- try the next rule
  Just k' -> rewrite' allrules allrules k' -- start over from the beginning

rewrite :: [Rule] -> FreeCat a b -> FreeCat a b
rewrite rules = rewrite' rules rules

simplify :: FreeCat a b -> FreeCat a b
simplify = rewrite allRules
