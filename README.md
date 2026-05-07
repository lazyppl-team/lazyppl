# <img style="height:45px" src="https://user-images.githubusercontent.com/8027127/223598298-21dd4207-612d-4b4e-be9c-4daa2ae2de5b.png" /> LazyPPL

LazyPPL is a Haskell library for Bayesian probabilistic programming, drawing on the fact that lazy data structures are a natural idiom for programming with infinite-dimensional Bayesian methods such as Poisson/Gaussian/Dirichlet processes.

The crucial semantic idea, inspired by developments in synthetic probability theory, is to work with two separate monads: an affine monad of probability, which supports laziness, and a commutative, non-affine monad of measures, which does not. This gives a Metropolis-Hastings implementation that works with lazy programs.

The main LazyPPL modules are on [Hackage](https://hackage.haskell.org/package/lazyppl) and [Stackage](https://www.stackage.org/). Various worked examples in literate Haskell are at [https://lazyppl-team.github.io](https://lazyppl-team.github.io).

Laziness lets us specify non-parametric models cleanly: processes or infinite-dimensional functions only have their finite explored portion materialised. We have first-class

* stochastic memoization: `memoize :: (a -> Prob b) -> Prob (a -> b)`
* Gaussian processes: `wiener :: Prob (Double -> Double)`
* point processes: `poissonPP :: Prob [Double]`
* Dirichlet processes: `dp :: Double -> Prob a -> Prob (Prob a)`


## Examples

* [`WienerDemo`](https://lazyppl-team.github.io/WienerDemo.html) ([`lhs`](src/WienerDemo.lhs)) is a simple implementation of regression using a Wiener process. Via maintaining a hidden table of previous calls, it appears to be a bona fide random function $R\to R$ constructed lazily. Some values are sampled during simulation, others during plotting.

![Wiener process regression](https://lazyppl-team.github.io/images/wiener-reg.svg)

* [`RegressionDemo`](https://lazyppl-team.github.io/RegressionDemo.html) ([`lhs`](src/RegressionDemo.lhs)) demonstrates piecewise linear regression. The change points are drawn from a lazy Poisson process.

![Poisson-split piecewise linear regression](https://lazyppl-team.github.io/images/regression-piecewise-reg.svg)

* [`Clustering`](https://lazyppl-team.github.io/ClusteringDemo.html) ([`lhs`](src/ClusteringDemo.lhs)) contains some simple clustering examples, where the number of clusters is unknown. Stick-breaking is lazy, and we use stochastic memoization.

![Dirichlet process clustering](https://lazyppl-team.github.io/images/clustering-map.svg)

* [`ProgramInduction`](https://lazyppl-team.github.io/ProgramInductionDemo.html) ([`lhs`](src/ProgramInductionDemo.lhs)) is a simple example of program induction over an arithmetic language. Random expressions are represented as an infinite forest with a finite path through it.

![Program induction](https://lazyppl-team.github.io/images/programinduction-reg.svg)

* Feature extraction via the [Indian Buffet Process](src/AdditiveClustering.hs), and relation inference via the [Mondrian Process](src/MondrianExample.hs) and a Chinese-Restaurant-Process-based [Infinite Relational Model](src/IrmTest.hs).


## Library

* [`LazyPPL.hs`](src/LazyPPL.hs) provides the core monads and inference. `Prob` is an affine monad of probability measures. `Meas` is a non-affine monad of unnormalised measures with `sample` and `score`. The Metropolis-Hastings algorithm `mh` takes a probability `p` of changing any given site of the rose tree of random seeds. Setting `p=1/num_sites` approximates single-site MH and `p=1` recovers multi-site MH. We can't compute the number of active sites without forcing more of the lazy tree, so we use this `p`-parameterised form. A second algorithm `mh1` implements a single-site proposal kernel by inspecting the Haskell heap.
* [`LazyPPL/Distributions.hs`](src/LazyPPL/Distributions.hs) and `LazyPPL/Distributions/*` provide common distributions and abstract types capturing exchangeability. For example, `newCustomer :: Restaurant -> Prob Table` for the Chinese Restaurant Process ([`DirichletP.hs`](src/LazyPPL/Distributions/DirichletP.hs)) and `Restaurant -> Prob [Dish]` for the Indian Buffet Process ([`IBP.hs`](src/LazyPPL/Distributions/IBP.hs)).


## Installation

Build with stack: [install stack](https://docs.haskellstack.org/en/stable/), then `stack build` in the repo root. First build takes a few minutes.

Run a demo: `stack run wiener-exe` (or `regression-exe`, `clustering-exe`, etc.), or explore in the REPL with `stack ghci`.

The `hmatrix`-based Gaussian process module needs LAPACK. On Ubuntu: `sudo apt-get install libgsl-dev liblapack-dev libblas-dev`. The plotting demos use the `matplotlib` Haskell wrapper, which calls into Python: `python3 -m pip install -U matplotlib numpy scipy`.

To try lazyppl in your browser without installing anything, click [![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/lazyppl-team/lazyppl). This opens a ready-to-use VS Code environment with GHC, stack, and the system libraries already in place.

A pre-built Docker image is at [`ghcr.io/lazyppl-team/lazyppl`](https://github.com/lazyppl-team/lazyppl/pkgs/container/lazyppl).


## Writing your own probabilistic models

Add an executable stanza to `package.yaml`:

```yaml
mymodel-exe:
  main:                MyModel.hs
  source-dirs:         src
  ghc-options:
  - -threaded
  - -rtsopts
  - -with-rtsopts=-N
  - -main-is MyModel
  dependencies:
  - lazyppl
```

Run with `stack run mymodel-exe`, or open in the REPL: `stack ghci src/MyModel.hs`.
