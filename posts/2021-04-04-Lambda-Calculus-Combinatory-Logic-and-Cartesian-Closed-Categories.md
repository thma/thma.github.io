---
title: λ-Calculus, Combinatory Logic and Cartesian Closed Categories
author: Thomas Mahler
tags: haskell, lambda-calculus, combinatory logic, cartesian closed categories, bracket abstraction
---

## Introduction

Recently I read the very interesting [Compiling to Categories](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf) paper by Conal Elliot.

He presents the idea to compile haskell programs into a expressions of  catesian closed categories by λ-elimination.

The process applied reminded me a lot of the [bracket abstraction](https://crypto.stanford.edu/~blynn/lambda/sk.html) used when compiling λ-terms to SKI-Combinators.

In the following I'm having a closer look at the parallels between compiling lambda to CCC and compiling lambda to SKI-combinators

## Lambda Calculus

I assume at least a rough familiarity with the λ-calculus. 
If you need a refresher I recommend [the chapter on λ-calculus in Stephen Diels excellent Write You a Haskell](http://dev.stephendiehl.com/fun/003_lambda_calculus.html).

Instead of the classical notation of lambda terms I'll use the the Haskell notation throughout this post. So instead of writing `λx.x a` I'll write `\x -> x a`.

## bracket abraction in combinatory logic

> The SKI combinator calculus is a combinatory logic, a computational system that may be perceived as a reduced version of the untyped lambda calculus. 
> It can be thought of as a computer programming language [...] because it is an extremely simple Turing complete language. 
> It was introduced by Moses Schönfinkel and Haskell Curry.
>
> Quoted from [Wikipedia](https://en.wikipedia.org/wiki/SKI_combinator_calculus)

λ-terms can be converted to variable free SKI combinator terms with a process called [bracket abstraction](https://en.wikipedia.org/wiki/Combinatory_logic#Completeness_of_the_S-K_basis).
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
k x y = x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s p q x = p x (q x)  
```

Please note that `i` is identical to `id` and `k` is identical to `const` from the Haskell Prelude.

Once the λ-terms are compiled to combinator terms, these terms can be interpreted quite efficiently as they don't contain any variables and so no environment-handling is needed.

Combinator terms also allow to apply several more advanced interpretation techniques like graph-reduction, node-sharing, parallel reduction, etc.

For a very cool demo have a look at the [web assembly based graph reduction engine by Ben Lynn](https://crypto.stanford.edu/~blynn/lambda/sk.html).

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

So we can conclude that the transformations from λ-calculus to SKI-combinators and CCC are equivalent. 

For me this was a new insight. But it seems that I was not the first to discover this: P.-L. Curien presents a much more elaborate proof of this correspondence in his classic paper [Categorical Combinators](https://core.ac.uk/download/pdf/82017242.pdf).
See also [Cartesian Closed Categories and Lambda-Calculus](http://pauillac.inria.fr/~huet/PUBLIC/cat.pdf).

## Nexts steps

In my next blog post I will have a closer look at a CCC based execution model for a subset of Haskell.
