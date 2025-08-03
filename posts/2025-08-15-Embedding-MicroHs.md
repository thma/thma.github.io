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

## What happened so far

- In (λ-Calculus, Combinatory Logic and Cartesian Closed Categories)[https://thma.github.io/posts/2021-04-04-Lambda-Calculus-Combinatory-Logic-and-Cartesian-Closed-Categories.html] I explained the basic concepts of compiling λ-Calculus based languages to combinatory logic expressions. 
This post also explores the deep correspondence between simply‑typed λ‑calculus, combinatory logic, and cartesian closed categories (CCC). 

- In [Implementing a Functional Language with Graph Reduction](https://thma.github.io/posts/2021-12-27-Implementing-a-functional-language-with-Graph-Reduction.html) I'm describing a minimal functional language implemented in Haskell using three core components: (1) a parser for untyped λ‑calculus, (2) a compiler translating λ‑terms into a fixed combinator set (S,K,I,B,C,Y) via bracket abstraction with basic optimizations, and (3) a graph‑reduction engine using in‑place mutable STRef nodes to implement combinator rewrite rules efficiently

- [Evaluating SKI Combinators as Native Haskell Functions](https://thma.github.io/posts/2022-02-05-Evaluating-SKI-combinators-as-native-Haskell-functions.html) This follow‑up provides an alternative to the graph reduction backend with direct evaluation of combinators as first‑class Haskell functions. The result is a significantly simpler and more compact implementation that achieves performance improvements by factors of 10 to 100 over the previous graph‑reduction approach

- [Optimizing Bracket Abstraction for Combinator Reduction](https://thma.github.io/posts/2023-10-08-Optimizing-bracket-abstraction-for-combinator-reduction.html)
Building on the last two posts, this article focuses on improving the bracket‑abstraction phase. 
I'm implementing several different optimizations of bracket abstraction introduced by Kiselyov. In particular I perform benchmarks to study their impact on execution speed.


## storyline

- producing MicroHs compatible combinator code from my mini compiler
- getting rid of IF combinator
- compiling True to A and False to K
- translating my combinator expressions into MHS out.comb format
- first pull request: exposing mhs as a library
- testing my out.comb with a patched mhseval
- writing an FFI Wrapper MhsEval around eval.c
- using MhsEval.withMhsContext \ctx -> eval ctx prg to reduce our code.
- microbenchmarking MhsEval against my toy runtimes.

- using the FFI wrapper to provide an implementation for "MHS -r Example.hs"
- second pull request the FFI wrapper + MHS -r