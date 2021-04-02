# Lambda Cat

### This is still heavily work in progress!!!

<img src="./lambda-cat-logo.png" width="400" height="400" alt="Mieke the official lambda cat 2021"/>



## disclaimer: 

This code is heavily based on Philip Zuckers 
[http://www.philipzucker.com/compiling-to-categories-3-a-bit-cuter/](http://www.philipzucker.com/compiling-to-categories-3-a-bit-cuter/)

I forked from a [specific branch of his repository](https://github.com/philzook58/not-bad-ccc/tree/fan2).
All the really complicated stuff is his invention. I just filled the blanks, cleaned up the code a bit and added an interpreter that executes CCC code.

I'm particlulary interested to research the parallels between compiling lambda to CCC and compiling lambda to (SKI) combinators.

## Introduction

Recently I read the very interesting [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) paper by Conal Elliot.

He presents the idea to compile haskell programs into a expressions of closed cartesian categories by λ-elimination.

The process applied reminded me of the [bracket abstraction](https://crypto.stanford.edu/~blynn/lambda/sk.html) used when compiling λ-terms to SKI-Combinators.

In the following I'm having a closer look at this connection.

## bracket abraction in combinatory logic

λ-terms can be converted to variable free combinator terms with a process called bracket abstraction.
Bracket abstraction `absCL` is defined by the following equations (given in pseudo Haskell notation, as pattern matching on functions is not possible in Haskell):


```haskell
absCL (\x -> x)   = i
absCL (\x -> y)   = k y
absCL (\x -> p q) = s (\x -> p) (\x -> q)
```

where the combinators `i`, `k` and `s` are defined as follows (these are valid haskell definitions):

```haskell
i :: a -> a
i x = x

k :: a -> b -> a
k y _ = y

s :: (a -> b -> c) -> (a -> b) -> a -> c
s p q x = (p x) (q x)  
```

Please note that `i` is identical to `id` and `k` is identical to `const` from the Haskell Prelude.

Once the λ-terms are compiled to combinator terms, these terms can be interpreted quite efficiently as they don't contain any variables and so no environment-handling is needed.

Combinator terms also allow to apply several more advanced interpretation techniques like graph-reduction, node-sharing, parallel reduction, etc.

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

The function `absCCC` looks surprisingly similar to the `absCL` function defined above. The first two pattern matches are obviously equivalent as `i` and `id` are identical as well as `k y` and `const y`.

But what about the third clause? We have:

```haskell
-- on the one hand: abstracting lambda-terms to combinator expresssions:
absCL (\x -> p q) = s (\x -> p) (\x -> q)

-- and on the other: abstracting lambda-terms to CCC expressions:
absCCC (\x -> p q) = apply . ((\x -> p) △ (\x -> q))
```
Are these two definitions equal? 

By eliminating all variables from the term `apply . ((\x -> p) △ (\x -> q))` we can write it as a combinator `s'` with variables `p, q, x`:

```haskell
s' p q x = (apply . (p △ q)) x
```

Now we can apply equational reasoning:

```haskell
s' p q x = (apply . (p △ q)) x   
         = apply ((p △ q) x)     -- by definition of (.)
         = apply (p x, q x)      -- by definition of (△)
         = (p x) (q x)           -- by definition of apply        
```

This equals the definition of the `s` combinator:

```haskell
s p q x = (p x) (q x)
```

So we can conclude that SKI-combinators and CCC are equivalent implementations of the λ-calculus. I admit this is a rather handwaving then a solid prove. You'll find a much more solid version in the classic paper [Categorical Combinators](https://core.ac.uk/download/pdf/82017242.pdf)
