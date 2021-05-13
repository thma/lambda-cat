# Lambda Cat

### This is still heavily work in progress!!!

<img src="./lambda-cat-logo.png" width="400" height="400" alt="Mieke the official lambda cat 2021"/>





## Introduction

Recently I read the very interesting [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) paper by Conal Elliot.

He presents the idea to compile haskell programs into expressions of cartesian closed categories by λ-elimination.
These expressions can then be used for different purposes like alternative program evaluation, graphic representation
of program graphs, designing hardware layouts for algorithms, etc.


## Cartesian Closed Categories (CCC)

In his famous paper [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) Conal Elliot describes a way to compile from simply typed lambda-calculus terms to cartesian closed categories(CCC).

At the core of his approach sits a transformation from lambda-terms to CCC expressions that are done by eliminating variables by an abstraction function `absCCC` (again in pseudo-Haskell):

```haskell
absCCC (\x -> x)   = id
absCCC (\x -> y)   = const y
absCCC (\x -> p q) = apply . ((\x -> p) △ (\x -> q))
```

Where `(△)` is introduced by the `Cartesian` category:

```haskell
class Category k => Cartesian k where
  (△) :: (a `k` c) -> (a `k` d) -> (a `k` (c, d))
```

In the `(->)` instance of `Cartesian` `(△)` is defined as: 

```haskell
(△):: (t -> a) -> (t -> b) -> t -> (a, b)
(f △ g) x = (f x, g x)
```

And where `apply` is introduced by the `Closed` category:

```haskell
class Cartesian k => Closed k where
  apply :: ((a -> b), a) `k` b
```

In the `(->)` instance of `Closed` `apply` is defined as 

```haskell
apply :: (a -> b, a) -> b
apply (f, x) = f x
```

## Conal's GHC plugin 

## Philip Zuckers 


## disclaimer:

This code is based on Philip Zuckers
[http://www.philipzucker.com/compiling-to-categories-3-a-bit-cuter/](http://www.philipzucker.com/compiling-to-categories-3-a-bit-cuter/)

I forked from a [specific branch of his repository](https://github.com/philzook58/not-bad-ccc/tree/fan2).
All the really complicated stuff is his invention. I just filled the blanks, cleaned up the code a bit and added an interpreter that executes CCC code.
