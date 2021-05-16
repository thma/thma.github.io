---
title: What's awesome about functional programming
author: Thomas Mahler
tags: haskell, anonymous-functions, function-composition, functional-programming, first-class-functions, 
      higher-order-functions, partial-application, pattern-matching, algebraic-data-types, polymorphic-types, 
      declarative-programming, mapping, folding, lazy-evaluation, foldable, monads, monadic-containers, jupyter, notebook, ihaskell, reveal.js, rise
---

Some weeks ago I gave an introductory talk on functional programming at an in-house mini-conference where I tried to explain basic concepts of functional programming to a crowd of developers with their backgrounds mostly in imperative and OO languages.

Initially I had planned to present the contents of my [Why Haskell Matters article](https://thma.github.io/posts/2020-04-01-why-haskell-matters.html) but that turned out impractical because of the time constraints of my talk.

So I prepared a condensed slide deck focussing on the more elementary sections, which worked quite well.

Recently I stumbled across [IHaskell](https://github.com/gibiansky/IHaskell), a Haskell kernel for the Jupyter Notebook platform. As I like the interactive approach of Jupyter notebooks quite a lot I transferred my presentation into the Jupyter notebook format.

As I'm quite happy with the result I'd like to make it available to a wider audience. I have prepared three different versions:


1. The notebook as rendered by 
[nbviewer.jupyter.org](https://nbviewer.jupyter.org/github/thma/IHaskellExperiments/blob/main/WhatsAwesomeAboutFunctionalProgramming.ipynb).
Unfortunately it's not interactive. But you won't need a local Jupyter installation.

2. A [reveal.js presentation](/static/WhatsAwesomeAboutFunctionalProgramming.slides.html) which I generated from the notebook. This version just needs a web-browser.

3. An Interactive version of the reveal.js slides hosted on Binder: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/thma/IHaskellExperiments/HEAD?filepath=ihaskell_examples/WhatsAwesomeAboutFunctionalProgramming.ipynb)
(Press ALT-r to start the presentation.) This version uses [RISE](https://rise.readthedocs.io/en/stable/) to allow interactive notebook cells while still being in presentation mode.
This version is based on a [Dockerfile](https://github.com/thma/IHaskellExperiments/blob/main/Dockerfile) that adds the RISE extension to an [ihaskell-notebook](https://hub.docker.com/r/crosscompass/ihaskell-notebook) docker image. This Dockerfile is then built and served by [Binder](https://mybinder.org/).



If you intend to use IHaskell notebooks locally [Please follow this installation guide](https://github.com/gibiansky/IHaskell#installation).

Sourcecode of my IHaskell notebook and the dockerfile are hosted in [this github repo](https://github.com/thma/IHaskellExperiments).





