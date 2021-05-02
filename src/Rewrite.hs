{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-- This module exposes a function simplify which takes a FreeCat expression as input and
    returns an equivalent yet syntactically simplified FreeCat expression.
    
    > toCCC @FreeCat (\(x, y) -> x)
    Comp Fst Id
    
    > simplify $ toCCC (\(x, y) -> x)
    Fst

--}  
module Rewrite (simplify) where
  
--import Control.Category ((.))
import Cat
import FreeCat
import Prelude hiding (id, (.))

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

ruleParen :: Rule
ruleParen (Comp (Comp f g) h) = Just (Comp f (Comp g h))
ruleParen _ = Nothing

ruleFstsndpar :: Rule
ruleFstsndpar (Comp (Par Fst Snd) Dup) = Just Id
ruleFstsndpar _ = Nothing

ruleFstDup :: Rule
ruleFstDup (Comp Fst Dup) = Just Id
ruleFstDup _ = Nothing

ruleSndDup :: Rule
ruleSndDup (Comp Snd Dup) = Just Id
ruleSndDup _ = Nothing

ruleParDup :: Rule
ruleParDup (Comp (Par (Comp f Fst) (Comp g Snd)) Dup) = Just (Par f g)
ruleParDup _ = Nothing

ruleParDup' :: Rule
ruleParDup' (Comp (Par (Comp f Fst) (Comp g Fst)) Dup) = Just ((Par f g . Dup) . Fst)
ruleParDup' _ = Nothing

ruleParDup'' :: Rule
ruleParDup'' (Comp (Par (Comp f Snd) (Comp g Snd)) Dup) = Just ((Par f g . Dup) . Snd)
ruleParDup'' _ = Nothing

-- parC dupC" forall f. (_parC f f) . _dupC = _dupC . f
{- -- needs equality.
ruleParDupEq :: Rule -- FreeCat a b -> Maybe (FreeCat a b)
ruleParDupEq (Comp (Par f g) Dup) | f == g = Just (Dup . f)
ruleParDupEq _                             = Nothing
--}

-- build the curry rules.
ruleCurry :: Rule
ruleCurry (Curry (Uncurry f)) = Just f
ruleCurry _ = Nothing

ruleCurry' :: Rule
ruleCurry' _ = Nothing

ruleCurryApply :: Rule
ruleCurryApply (Curry Apply) = Just Id
ruleCurryApply _ = Nothing

ruleIdLeft :: Rule
ruleIdLeft (Comp Id f) = Just f
ruleIdLeft _ = Nothing

ruleIdRight :: Rule
ruleIdRight (Comp f Id) = Just f
ruleIdRight _ = Nothing

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

maxDepth :: Int
maxDepth = 1000

-- Avoid infinite loops by allowing only a recursion depth of `maxDepth`
recurseMatch :: Int -> Rule -> FreeCat a b -> Maybe (FreeCat a b)
recurseMatch 0 _rule _x = Nothing
recurseMatch depth rule x = case rule x of
  Nothing -> goDown (recurseMatch (depth -1) rule) x -- This rule didn't match. Try going down and matching there.
  Just x' -> Just x'

goDown :: Rule -> Rule --FreeCat a b -> Maybe (FreeCat a b)
goDown z (Comp f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing
    Just x -> Just (Comp f x)
  Just x -> Just (Comp x g)
goDown z (Par f g) = case z f of
  Nothing -> case z g of
    Nothing -> Nothing -- nothing in either subtree macthed
    Just x -> Just (Par f x) --
  Just x -> Just (Par x g) -- something in f matched the rule
goDown z (Curry f) = case z f of
  Nothing -> Nothing
  Just x -> Just (Curry x)
goDown z (Uncurry f) = case z f of
  Nothing -> Nothing
  Just x -> Just (Uncurry x)
goDown _ _ = Nothing -- can't go down

rewrite' :: [Rule] -> [Rule] -> FreeCat a b -> FreeCat a b
rewrite' _ [] k = k -- no rules matched
rewrite' allrules (rule : rules) k = case recurseMatch maxDepth rule k of
  Nothing -> rewrite' allrules rules k     -- try the next rule
  Just k' -> rewrite' allrules allrules k' -- start over from the beginning

rewrite :: [Rule] -> FreeCat a b -> FreeCat a b
rewrite rules = rewrite' rules rules

simplify :: FreeCat a b -> FreeCat a b
simplify = rewrite allRules
