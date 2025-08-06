---
title: Embedding MicroHs
author: Thomas Mahler
tags: haskell, compiler, GHC, MHS, MicroHs, combinatory logic, graph-reduction, bracket abstraction, Haskell in Haskell, performance, optimization, bulk combinators, Kiselyov, Ben Lynn, Lennart Augustsson
      
---

<a href="https://github.com/thma/lambda-ski"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## Abstract



## Introduction

Attentive readers of my blog will have noticed that I am a great fan of combinatory logic and graph reduction in the implementation of functional languages. 

Some time ago, I became aware of Lennart Augustsson's MicroHs project, which provides an alternative Haskell compiler that targets a runtime environment based on combinatory logic and graph reduction.

MicroHs is an awesome project for several reasons: 

* It demonstrates how a complex high-level language like Haskell can be translated and executed efficiently.

* It outlines an approach for bootstrapping a Haskell compiler using only a C compiler as a prerequisite. This represents a significant step for bootstrapping the GHC compiler (see: [Bootstrappable projects](https://www.bootstrappable.org/projects.html)).

* The entire codebase remains compact and manageable, making it an ideal textbook example for functional language compiler.

While studying the MicroHs codebase I noticed that it uses compilation techniques, combinatory logic expressions and graph reduction mechanics that are quite close to the concepts that I presented in my previous blog posts on this topic. I was particularly impressed by the MicroHs graph-reduction runtime implemented in C.

So I came up with the idea of adjusting my toy compilation system to generate object code that can be executed with the MicroHs runtime system.

In this blog post I'll explain what I did to achieve this goal. I'll also explain briefly the two pull requests that I added to MicroHs, which allow to embed MicroHs into Haskell code copiled with GHC.


## Using the MicroHs Runtime as a backend for my toy compiler

MicroHs is using a set of combinators that is quite close to those used in my toy language implementation.

The only difference I noticed was in the handling of conditional expressions. My toy compiler was using a dedicated `IF` combinator, whereas MicroHs is providing a much more generic and flexible system that makes clever use of combinators `A` and `K`. 

This gave me confidence that it shouldn't be too difficult to use the MicroHs runtime as a target to my compiler.

### Getting rid of the IF combinator

As my toy language is just dealing with functions over integers I tried to keep thngs as easy as possible and thus modelled `True` as `1` and `False` as `0`.
So for example in the `HhiReducer` the equality test `eql` is defined as:

```haskell
eql :: (Eq a, Num p) => a -> a -> p
eql n m = if n == m then 1 else 0
```

The `IF`-Combinator takes three arguments `condition`, `thenExp` and `elseExp`. The semantics is simple: if `condition` evaluates to `1`, `thenExp` is evaluated alse `elseExp`:  

```haskell
CFun (\(CInt condition) -> CFun $ \thenExp -> CFun $ \elseExp -> 
    if condition == 1 
        then thenExp 
        else elseExp)
```

MicroHs is encoding boolean values quite differently:

```haskell
True = A
False = K
```

Where `A` and `K` are Combinators defined as follows:
```haskell
K x y = x 
A x y = y
```

So in this Encoding `True` and `False` can be used as selector functions. 

We can apply this feature to eliminate the `IF`-combinator. We will achieve this by using a new function `desugarIf` before performing bracket abstraction. This function will desugar `If`-expressions to Scott encoded boolean applications. It will detect sourcecode patterns `if condition thenExpr elseExpr` and transforms it to: `condition elseExpr thenExpr`. 

```haskell
desugarIf :: Expr -> Expr
desugarIf (((Var "if" `App` condition) `App` thenExpr) `App` elseExpr) =
  (desugarIf condition `App` desugarIf elseExpr) `App` desugarIf thenExpr
desugarIf (App e1 e2) = App (desugarIf e1) (desugarIf e2)
desugarIf (Lam x e) = Lam x (desugarIf e)
desugarIf expr = expr  -- Var, Int remain unchanged
```

When `condition` evaluates to `True` (i.e. `A`) the second argument (`thenExpr`) is selected.
When `condition`evaluates to `False` (i.e. `K`) the first argument (`elseExpr`) is selected. 

To make this work we will have to change all comparison functions to return `A`and `K`, like in the following snippet from the HhiReducer:

```haskell
eql :: (Eq a) => a -> a -> CExpr
eql n m = if n == m then trueCExpr else falseCExpr

-- | Helper functions for Scott-encoded booleans
trueCExpr :: CExpr
trueCExpr = link primitives (translate (Com A))

falseCExpr :: CExpr
falseCExpr = link primitives (translate (Com K))
```

Let's have a closer look how this can be very handy when compiling conditional expressions to efficient code. Let's illustrate this with an example of my improved toy compiler. Let's say we have a very simple `main`-expression:

```haskell
main :: Int
main = if (eql 0 1) 23 42
```

With the new desugaring this will compiled to the following combinator expression. Please note that `thenExpr` and `elseExpr` have been swapped by `desugarIf`:

```haskell
EQL 0 1 42 23
```

Now let`s have a look at the combinator-reduction of this expression:

```Haskell
EQL 0 1 42 23 
K 42 23.       -- by reducing EQL 0 1 to K (representing 'False')
42             -- by reducing K x y to x

-- likewise for the 'True' case:
EQL 0 0 42 23
A 42 23.       -- by reducing EQL 0 0 to A (representing 'True')
23             -- by reducing A x y to y
```

### producing MicroHs compatible combinator code from my toy compiler

After fixing the incompatibility in the handling of conditional expressions there is is only one task left: We'll have to translate the combinator expressions generated by my compiler to a avlid MicroHs combinator program. This is done in the `MicroHsExp` module.

The first step is to map from my `CLTerm.CL`-terms to `MicroHs.Exp`-terms.
By looking at the type definitions we can see that the mapping will be straightforward:

```haskell
-- CLTerm.CL:
data CL = 
    Com Combinator 
  | INT Integer 
  | CL :@ CL 
  deriving (Eq, Data)

-- MicroHs.Exp:
data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Lit Lit
  deriving (Eq)  
```

The `Exp` data type is used to store desugared λ-Terms as well as combinator terms. That is the reason why it allows free variables (`Var`) and λ-terms (`Lam`). But after running bracket abstraction over such a term it will only `App` and `Lit` constructors.

With this knowledge we can define a `toMhsExp :: CL -> Exp`function:

```haskell
import CLTerm (CL(..))
import MicroHs.Exp (Exp(..))
import MicroHs.Expr (Lit(..))

toMhsExp :: CL -> Exp
toMhsExp (Com c) = Lit (LPrim (combToMhscomb c))
toMhsExp (INT i) = Lit (LInt (fromIntegral i))
toMhsExp (t :@ u) = App (toMhsExp t) (toMhsExp u)
```

The interesting part he is that both integers and combinators are treated as `Lit` instances with specific constructors `LInt` and `LPrim`.

In order to map the combinators and primops of my compiler to MicroHs we can simply use `show` to produce a String representation and only for a few arimthmetic and comparison primops we need a specific translation:

```haskell
import CLTerm (Combinator(..))

combToMhscomb :: Combinator -> String
combToMhscomb ADD = "+"
combToMhscomb SUB = "-"
combToMhscomb MUL = "*"
combToMhscomb DIV = "/"
combToMhscomb REM = "rem"
combToMhscomb EQL = "=="
combToMhscomb GEQ = ">="
combToMhscomb c = show c
```

### translating my combinator expressions into MHS out.comb format

The final step of producing a valid MicroHs `out.comb` file is even simpler.
MicroHs defines a function `toStringCMdl`, which takes a list of definitions (i.e. all functions, expressions and CAFs defined in a haskell program) and an expression representing the `main`-expression of a Haskell program as input. The result is a tuple with the number of definitions, a list of all foreign export identifiers, and the program as a string. This last element of the tuple will contain the contain the actual combinator code, i.e. the contents of an `out.comb`file.

So by using this function we can can compile our `CL`-term to a valid MicroHs program with very little effort:

```haskell
import MicroHs.ExpPrint (toStringCMdl)

toMhsPrg :: CL -> String
toMhsPrg cl = 
  let (n, exps, prg) = toStringCMdl ([], toMhsExp cl)
   in prg
```



## storyline


- first pull request: exposing mhs as a library
- testing my out.comb with a patched mhseval
- writing an FFI Wrapper MhsEval around eval.c
- using MhsEval.withMhsContext \ctx -> eval ctx prg to reduce our code.
- microbenchmarking MhsEval against my toy runtimes.

- using the FFI wrapper to provide an implementation for "MHS -r Example.hs"
- second pull request the FFI wrapper + MHS -r



## Appendix: my earlier posts on combinatory logic and graph-reduction

- In (λ-Calculus, Combinatory Logic and Cartesian Closed Categories)[https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html] I explained the basic concepts of compiling λ-Calculus based languages to combinatory logic expressions. 
This post also explores the deep correspondence between simply‑typed λ‑calculus, combinatory logic, and cartesian closed categories (CCC). 

- In [Implementing a Functional Language with Graph Reduction](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) I'm describing a minimal functional language implemented in Haskell using three core components: (1) a parser for untyped λ‑calculus, (2) a compiler translating λ‑terms into a fixed combinator set (S,K,I,B,C,Y) via bracket abstraction with basic optimizations, and (3) a graph‑reduction engine using in‑place mutable STRef nodes to implement combinator rewrite rules efficiently

- [Evaluating SKI Combinators as Native Haskell Functions](https://thma.github.io/posts/2022-02-05-Evaluating-SKI-combinators-as-native-Haskell-functions.html) This follow‑up provides an alternative to the graph reduction backend with direct evaluation of combinators as first‑class Haskell functions. The result is a significantly simpler and more compact implementation that achieves performance improvements by factors of 10 to 100 over the previous graph‑reduction approach

- [Optimizing Bracket Abstraction for Combinator Reduction](https://thma.github.io/posts/2023-10-08-Optimizing-bracket-abstraction-for-combinator-reduction.html)
Building on the last two posts, this article focuses on improving the bracket‑abstraction phase. 
I'm implementing several different optimizations of bracket abstraction introduced by Kiselyov. In particular I perform benchmarks to study their impact on execution speed.
