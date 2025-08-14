---
title: Embedding MicroHs
author: Thomas Mahler
tags: haskell, compiler, GHC, MHS, MicroHs, combinatory logic, graph-reduction, bracket abstraction, Haskell in Haskell, performance, optimization, bulk combinators, Kiselyov, Ben Lynn, Lennart Augustsson
      
---

<a href="https://github.com/thma/lambda-ski"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## Abstract



## Introduction

Attentive readers of my blog will have noticed that I am a big fan of combinatory logic and graph reduction in the implementation of functional languages. 

Some time ago, I became aware of [Lennart Augustsson's MicroHs project](https://github.com/augustss/MicroHs?tab=readme-ov-file#micro-haskell), which provides an alternative Haskell compiler that targets a runtime environment based on combinatory logic and graph reduction.

MicroHs is an awesome project for several reasons: 

* It demonstrates how a complex high-level language like Haskell can be translated and executed efficiently using bracket abstraction, Scott encoding, combinatory logic and a graph reduction runtime.

* It outlines an approach for bootstrapping a Haskell compiler using only a C compiler as a prerequisite. This represents a significant step for bootstrapping the GHC compiler (see: [Bootstrappable projects](https://www.bootstrappable.org/projects.html)).

* The entire codebase remains compact and manageable, making it an ideal textbook example for a functional language compiler.

While studying the MicroHs codebase I noticed that it uses compilation techniques, combinatory logic expressions and graph reduction mechanics that are quite close to the concepts that I presented in [some of my previous blog posts on this topic](#appendix-my-earlier-posts-on-combinatory-logic-and-graph-reduction). I was particularly impressed by the MicroHs graph-reduction runtime implemented in C.

So I came up with the idea of adjusting my toy compilation system to generate object code that can be executed with the MicroHs runtime system.

In this blog post I'll explain what I did to achieve this goal. I'll also explain briefly the two pull requests that I added to MicroHs, which allow to embed MicroHs into Haskell code copiled with GHC.


## Using the MicroHs Runtime as a backend for my toy compiler

MicroHs is using a set of combinators that is quite close to those used in my toy language implementation.

The only difference I noticed was in the handling of conditional expressions. My toy compiler was using a dedicated `IF` combinator, whereas MicroHs is providing a much more generic and flexible system that makes clever use of combinators `A` and `K`. 

This gave me confidence that it shouldn't be too difficult to use the MicroHs runtime as a target to my compiler.

### Getting rid of the IF combinator

As my toy language is just dealing with functions over integers I tried to keep thngs as easy as possible and thus modelled `True` as `1` and `False` as `0`.
So for example in the [HhiReducer](https://github.com/thma/lambda-ski/blob/main/src/HhiReducer.hs) the equality test `eql` is defined as:

```haskell
eql :: (Eq a, Num p) => a -> a -> p
eql n m = if n == m then 1 else 0
```

The `IF`-Combinator takes three arguments `condition`, `thenExpr` and `elseExpr`. The semantics is simple: if `condition` evaluates to `1`, `thenExpr` is evaluated alse `elseExpr`:  

```haskell
CFun (\(CInt condition) -> CFun $ \thenExpr -> CFun $ \elseExpr -> 
    if condition == 1 
        then thenExpr 
        else elseExpr)
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

So in this Encoding `True` and `False` can be used as selector functions to either pick the `thenExpr` or the `elseExpr` for evaluation. 

We can apply this feature to eliminate the `IF`-combinator. We will achieve this by using a new function `desugarIf` before performing bracket abstraction. This function will desugar `If`-expressions to Scott encoded boolean applications. It will detect sourcecode patterns `if condition thenExpr elseExpr` and transforms it to: `condition elseExpr thenExpr`. 

```haskell
desugarIf :: Expr -> Expr
desugarIf (((Var "if" `App` condition) `App` thenExpr) `App` elseExpr) =
  (desugarIf condition `App` desugarIf elseExpr) `App` desugarIf thenExpr
desugarIf (App e1 e2) = App (desugarIf e1) (desugarIf e2)
desugarIf (Lam x e) = Lam x (desugarIf e)
desugarIf expr = expr  -- Var, Int remain unchanged
```

When `condition` evaluates to `True` (i.e. `A`) the second argument (`thenExpr`) will be selected.
When `condition`evaluates to `False` (i.e. `K`) the first argument (`elseExpr`) will be selected. 

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

After fixing the incompatibility in the handling of conditional expressions there is is only one task left: We'll have to translate the combinator expressions generated by my compiler to a avlid MicroHs combinator program. This is done in the [MicroHsExp](https://github.com/thma/lambda-ski/blob/main/src/MicroHsExp.hs) module.

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

With this knowledge we can define a `toMhsExp :: CL -> Exp` function:

```haskell
import CLTerm (CL(..))
import MicroHs.Exp (Exp(..))
import MicroHs.Expr (Lit(..))

toMhsExp :: CL -> Exp
toMhsExp (Com c) = Lit (LPrim (combToMhscomb c))
toMhsExp (INT i) = Lit (LInt (fromIntegral i))     -- LInt only works with Int
toMhsExp (t :@ u) = App (toMhsExp t) (toMhsExp u)
```

The interesting part here is that both integers and combinators are treated as `Lit` instances with specific constructors `LInt` and `LPrim`.

In order to be able to import MicroHs code I contributed a PR which exposes the MicroHs source code as a library in the [MicroHs.cabal](https://github.com/augustss/MicroHs/blob/master/MicroHs.cabal) file. Now we can simply embed the MicroHs compiler (or parts of it) in any Haskell program by adding `MicroHs`as a dependency to Haskell projects.

Mapping the combinators and primops of my compiler to MicroHs can simply be achieved by `show`, only for a few arithmetic and comparison primops we need specific translations as MicroHs uses [other names for them](https://github.com/augustss/MicroHs/blob/master/ghc/PrimTable.hs#L32):

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

Even when compiling a program with many top-level definitions my toy compiler just returns the compiled `main` expression (with all calls to other toplevel definitions expanded to combinator code). As we we don't have any top-level definitions left after compilation we just hand over an empty list

With this knowledge we can can compile our `CL`-term to a valid MicroHs program with very little effort:

```haskell
import MicroHs.ExpPrint (toStringCMdl)

toMhsPrg :: CL -> String
toMhsPrg cl = 
  let
    definitions = [] 
    (n, exps, prg) = toStringCMdl (definitions, toMhsExp cl)
   in prg
```

### A first test drive

Now it's time for a first test drive of our solution:

```haskell
factorial :: String
factorial = [r| 
  fact = y(\f n. if (eql n 0) 1 (* n (f (- n 1))))
  main = fact 10
|]

main :: IO ()
main = do
  let source = factorial
      env = parseEnvironment source
      expr' = compileEta env.  -- compileEta is a good default for dense combinator code
      prg = toMhsPrg expr'     -- use MicroHs.ExpPrint.toStringCMdl to produce MicroHs code    

  putStrLn $ "Factorial compiled to combinator expression:\n" ++ show expr'
  putStrLn $ "The resulting MicroHs program: \n" ++ prg
  writeFile "out.comb" prg.    -- out.comb is the default file name for code executed by mhseval
```

```
ghci> main
Factorial compiled to combinator expression: 
Y(R 1(B C(B(S(C EQL 0))(B(S MUL)(R(C SUB 1) B))))) 10
The resulting MicroHs program: 
v8.2
0
Y R #1 @B C @B S C == @#0 @@@B S * @@R C - @#1 @@B @@@@@@#10 @ }
```

Please note that `Int` literals are encoded with a leading `#`in MicroHs code format, like `#0`, `#1`and `#10` in the program above.

Now we use the MicroHs evaluator `mhseval` to run this program written to the file `out.comb`:

```bash
bash> mhseval 
#3628800
```

I think this is quite impressive: MicroHs knows all the combinators my toy compiler is emmitting, including the `Y`-combinator
and the reduction works in the expected way and produces the correct result!

So using the MicroHs evaluator as a runtime environment for combinator code generated by other compilers seems quite feasible!


## Using an FFI Wrapper to call `mhseval` from Haskell

My intial idea was to use the benchmark suite presented in [my earlier posts](#appendix-my-earlier-posts-on-combinatory-logic-and-graph-reduction) to find out how MicroHs compares to my toy implementations.

As a first attempt I encapsulated the generation of MicroHs code and calling `mhseval` like follows:

```haskell
microHsevalTest :: CL -> IO String
microHsevalTest expr = do
  let prg = toMhsPrg expr
  readProcess "mhseval" [] prg
```

This worked, but due to the overhead caused by executing `mhseval` as an external process I did not see any performance gains.

So I came up with a new plan: let's write an FFI wrapper around `mhseval` to avoid spawning external processes.

I won't go into the details of this wrapper. The good news is that the respective PR was accepted and is now part of the official MicroHs codebase.
If you are interested you can study the code of the `C`-wrapper here: [mhseval.h](https://github.com/augustss/MicroHs/blob/master/src/runtime/mhseval.h) and [mhseval.c](https://github.com/augustss/MicroHs/blob/master/src/runtime/mhseval.c).
The Haskell wrapper is in [MhsEval.hs](https://github.com/augustss/MicroHs/blob/master/ghc/MhsEval.hs).

```haskell
import MicroHsExp (toMhsPrg)
import MhsEval (withMhsContext, eval, run)

main :: IO ()
main = do
  let source = factorial
  let env = parseEnvironment source
  let expr = compileEta env
  putStrLn $ "Factorial compiled to combinator expression:\n" ++ show expr

  let prg = toMhsPrg expr
  putStrLn $ "The resulting MicroHs program: \n" ++ prg
  
  result <- withMhsContext $ \ctx ->
    eval ctx prg
  putStrLn $ "Result: " ++ result
```

The resulting output does not bring any surprises:

```
ghci> main
Factorial compiled to combinator expression: 
Y(R 1(B C(B(S(C EQL 0))(B(S MUL)(R(C SUB 1) B))))) 10
The resulting MicroHs program: 
v8.2
0
Y R #1 @B C @B S C == @#0 @@@B S * @@R C - @#1 @@B @@@@@@#10 @ }
Result: #3628800
```

The function `withMhsContext :: (MhsContext -> IO a) -> IO a` executes an action (like `eval` or `run`) with a MicroHs context. It initializes a context, runs the action, and cleans up afterwards. It is useful for one-off evaluations without needing to manage the context manually.

The function `eval :: MhsContext -> MhsCombCode -> IO String` takes a string containing MicroHs combinator code, evaluates it, and returns the result as a string. If evaluation fails, it throws an `MhsEvalError`. The type `MhsCombCode` is just an alias for `String`. This currently works properly only for results of type `Int`.

The function `run :: MhsContext -> MhsCombCode -> IO ()` takes a string containing MicroHs combinator code, and runs it without returning any result. 

In a scenario like a performance benchmark it is not a good idea to create a new context in the tight benchmark loop. For such use cases I have also provided functions for explicitely managing the context: `createMhsContext :: IO MhsContext` and `closeMhsContext :: MhsContext -> IO ()`.

## Benchmarking MhsEval against my toy runtime.

In my last blog post on this matter I focussed on comparing different bracket abstraction algorithms.

|testcase|HHI-Reducer|MicroHs|Haskell native| 
|----|----|----|----|
|fib 37|8.641 s|6.296 s|727.3 ms|
|ackermann 3 9 |4.170 s|1.575 s| 105.6 ms|
|tak 18 6 3|1.225 ms|1.101 ms|42.01 μs|

<--
benchmarking ackermann HHI-Reduce
time                 4.170 s    (4.148 s .. 4.180 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.166 s    (4.158 s .. 4.169 s)
std dev              5.893 ms   (1.075 ms .. 8.071 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking ackermann MicroHs
time                 1.575 s    (1.525 s .. 1.605 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.595 s    (1.581 s .. 1.621 s)
std dev              25.03 ms   (1.942 ms .. 30.98 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking ackermann Native
time                 105.6 ms   (105.4 ms .. 105.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 105.6 ms   (105.5 ms .. 105.7 ms)
std dev              196.7 μs   (106.3 μs .. 343.3 μs)

benchmarking fibonacci HHI-Eta
time                 8.641 s    (8.556 s .. 8.800 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.597 s    (8.580 s .. 8.622 s)
std dev              25.16 ms   (2.640 ms .. 31.99 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking fibonacci MicroHs
time                 6.296 s    (6.286 s .. 6.310 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 6.302 s    (6.300 s .. 6.305 s)
std dev              3.384 ms   (1.956 ms .. 4.782 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking fibonacci Native
time                 727.3 ms   (722.4 ms .. 732.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 729.4 ms   (728.2 ms .. 730.7 ms)
std dev              1.555 ms   (581.8 μs .. 2.052 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking tak       HHI-Eta
time                 1.225 ms   (1.220 ms .. 1.230 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.221 ms   (1.219 ms .. 1.224 ms)
std dev              8.311 μs   (6.513 μs .. 10.89 μs)

benchmarking tak       MicroHs
time                 1.101 ms   (1.080 ms .. 1.125 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 1.077 ms   (1.067 ms .. 1.092 ms)
std dev              43.67 μs   (36.26 μs .. 49.96 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking tak       Native
time                 42.01 μs   (41.90 μs .. 42.11 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 42.00 μs   (41.87 μs .. 42.12 μs)
std dev              417.9 ns   (339.7 ns .. 505.4 ns)
-->

## Using the FFI wrapper to compile and execute Haskell programs 

In this blog post, I have so far only used the parts of MiscroHs that deal either directly with the generation of Combinator code or with the execution of Combinator code.

But my two pull requests allow to embed MicroHs in GHC compiled Haskell programs in a much more complete way: 

Let's assume we have a file `Example.hs` with the following code:

```haskell
module Example where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n - 1)

main :: IO ()
main = do
  putStrLn "computing some factorials"
  print $ map fac [0..10]
```

Now let's use MicroHs to compile and execute this code from some arbitry GHC compiled Haskell program:

```haskell
import           MhsEval (withMhsContext, eval, run)
import qualified MicroHs.Main as MHS (main)
import           System.Process (withArgs)

main :: IO ()
main = do
  -- use microHs to compile 'Example.hs' to 'out.comb'
  withArgs ["Example.hs"] MHS.main
  -- read the program 'out.comb' into a string
  prg' <- readFile "out.comb"
  -- use the MicroHs runtime to execute the program
  withMhsContext $ \ctx ->
    run ctx prg'
```

The output of this program in GHCI looks like follows:

```bash
ghci> main
computing some factorials
[1,1,2,6,24,120,720,5040,40320,362880,3628800]
```

Ok, this works as expected but it feels a bit clumsy to send the compiler output to a file and then read in that file to be able to execute it.
In order to improve this sketchy solution I integrated the MhsEval wrapper more tightly into the mhs compiler by implementing the `mhs -r` option also for GHC based compiles. With this goody the compile and execute cycle can be unified in a single command, as shown in the following snippet:

```haskell
import qualified MicroHs.Main as MHS (main)
import           System.Process (withArgs)

main :: IO ()
main = do
  -- use MicroHs to compile AND execute the 'Example.hs' program
  withArgs ["-r", "Example.hs"] MHS.main
```


## Appendix: my earlier posts on combinatory logic and graph-reduction

- In [λ-Calculus, Combinatory Logic and Cartesian Closed Categories](https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html) I explained the basic concepts of compiling λ-Calculus based languages to combinatory logic expressions. 
This post also explores the deep correspondence between simply‑typed λ‑calculus, combinatory logic, and cartesian closed categories (CCC). 

- In [Implementing a Functional Language with Graph Reduction](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) I'm describing a minimal functional language implemented in Haskell using three core components: (1) a parser for untyped λ‑calculus, (2) a compiler translating λ‑terms into a fixed combinator set (S,K,I,B,C,Y) via bracket abstraction with basic optimizations, and (3) a graph‑reduction engine using in‑place mutable STRef nodes to implement combinator rewrite rules efficiently

- [Evaluating SKI Combinators as Native Haskell Functions](https://thma.github.io/posts/2022-02-05-Evaluating-SKI-combinators-as-native-Haskell-functions.html) This follow‑up provides an alternative to the graph reduction backend with direct evaluation of combinators as first‑class Haskell functions. The result is a significantly simpler and more compact implementation that achieves performance improvements by factors of 10 to 100 over the previous graph‑reduction approach

- [Optimizing Bracket Abstraction for Combinator Reduction](https://thma.github.io/posts/2023-10-08-Optimizing-bracket-abstraction-for-combinator-reduction.html)
Building on the last two posts, this article focuses on improving the bracket‑abstraction phase. 
I'm implementing several different optimizations of bracket abstraction introduced by Kiselyov. In particular I perform benchmarks to study their impact on execution speed.
