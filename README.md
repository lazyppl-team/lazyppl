# <img style="height:45px" src="https://user-images.githubusercontent.com/8027127/223598298-21dd4207-612d-4b4e-be9c-4daa2ae2de5b.png" /> LazyPPL


LazyPPL is a Haskell library for Bayesian probabilistic programming, drawing on the fact that lazy data structures are a natural idiom for programming with infinite-dimensional Bayesian methods such as Poisson/Gaussian/Dirichlet processes, etc. 
The crucial semantic idea, inspired by developments in synthetic probability theory, is to work with two separate monads: an affine monad of probability, which supports laziness, and a commutative, non-affine monad of measures, which does not.

It provides a Metropolis-Hastings implementation in Haskell that works with lazy programs, together with some examples.

Various examples are given in Literate Haskell at [https://lazyppl-team.github.io](https://lazyppl-team.github.io). 

To try the different examples, use ``stack run wiener-exe`` and so on.
The source is in ``src/``.

A Docker image is [available](https://hub.docker.com/r/youkad/lazyppl). To start a container, run the following command:

```bash
docker pull youkad/lazyppl:0.2
docker run -it --rm -v ~/images:/opt/lazyppl/images youkad/lazyppl:0.2
```

Laziness appears to be a useful method for specifying non-parametric models. For example, we often use processes or infinite dimensional functions, and this is fine because only finite parts are explored in practice. 

The aim of this implementation is to demonstrate that it's possible to have first-class lazy structures in probabilistic programming. For example, we have first-class

* stochastic memoization: `memoize :: (a -> Prob b) -> Prob (a -> b)`
* Gaussian processes: `wiener :: Prob (Double -> Double)`
* point processes: `poissonPP :: Prob [Double]`
* Dirichlet process: `dp :: Double -> Prob a -> Prob (Prob a)`


## Examples

* [``WienerDemo``](https://lazyppl.bitbucket.io/WienerDemo.html) ([``lhs``](src/WienerDemo.lhs)) contains a simple implementation of regression using a Wiener process. Via maintaining a hidden table of previous calls, it appears to be a bona fide random function $R\to R$ that is constructed lazily. Because the functions are built lazily, some values of the functions will be sampled during the simulation, and others just during the plotting.

![Wiener process regression](https://lazyppl.bitbucket.io/images/wiener-reg.svg)

* [``RegressionDemo``](https://lazyppl.bitbucket.io/RegressionDemo.html) ([``lhs``](src/RegressionDemo.lhs)) demonstrates piecewise linear regression. Key idea: the change points are drawn from a lazy Poisson process.

![Poisson-split piecewise linear regression](https://lazyppl.bitbucket.io/images/regression-piecewise-reg.svg)

* [``Clustering``](https://lazyppl.bitbucket.io/ClusteringDemo.html)  ([``lhs``](src/ClusteringDemo.lhs)) contains some simple clustering examples, where the number of clusters is unknown. Key uses of laziness: stick-breaking is lazy, and we also use stochastic memoization.

![Dirichlet process clustering](https://lazyppl.bitbucket.io/images/clustering-map.svg)

* [``ProgramInduction``](https://lazyppl.bitbucket.io/ProgramInductionDemo.html)  ([``lhs``](src/ProgramInductionDemo.lhs)) contains a simple example of program induction over a simple arithmetic language. Key use of laziness: Random expressions are represented as an infinite forest together with a finite path through it.

![Program induction](https://lazyppl.bitbucket.io/images/programinduction-reg.svg)

* We also have examples of feature extraction ([Additive Clustering](src/AdditiveClustering.hs) via the Indian Buffet Process), and relation inference (via the [Mondrian Process](src/MondrianExample.hs), and a simple Chinese-Restaurant-Process based [Infinite Relational Model](src/IrmTest.hs)). 


## Library

* [``LazyPPL.hs``](src/LazyPPL.hs) contains a likelihood weighted importance sampling algorithm (lwis) and a Metropolis-Hastings algorithm, together with the basic monads. There are two monads, `Prob` and `Meas`. 
    * `Prob` is to be thought of as a monad of probability measures. It provides a function `uniform`. 
    * `Meas` is to be thought of as a monad of unnormalized measures. It provides an interface using `sample` (which draws from a `Prob`) and `score` (which weights a trace, typically by a likelihood). 
    * Our first Metropolis Hastings algorithm `mh` takes as an argument a probability `p` of changing any given site. This is different from single-site MH (which picks exactly one site to change each step) and multi-site MH (which changes all sites at each step). The reason for this is that in the lazy setting we cannot explore how many sites there are without triggering more computation (that would require a lot of Haskell hacking). We can recover single-site MH by putting `p=1/num_sites` and multi-site MH by putting `p=1`.
	* A second Metropolis Hastings algorithm `mh1` implements a single-site proposal kernel by inspecting the Haskell heap.
    * A key implementation idea is that the underlying sample space is `Tree`, which comprises a lazy tree of `Double`s that is infinitely wide and infinitely deep. Informally, at any moment, if you need some unknown or lazy amount of randomness, you can grab just one branch of the tree, without worrying about affecting the other branches. That branch will itself be a `Tree`, so this can all happen recursively or in a nested way without any problems. 
* [``Distr.hs``](src/Distr.hs) contains common parametric distributions such as normal distributions etc.. We find that the types of Haskell are also quite illuminating for complex distributions, see the types of the processes `memoize`, `wiener`, `poissonPP`, `dp` [above](#top). We also use abstract types, which capture some essence of exchangeability, for example:
    * in a Chinese Restaurant Process, `newCustomer :: Restaurant -> Prob Table` ([``DirichletP.hs``](src/Distr/DirichletP.hs));
    * in an Indian Buffet Process, `newCustomer :: Restaurant -> Prob [Dish]` ([``IBP.hs``](src/Distr/IBP.hs)).


## Installation

The system uses Haskell stack.
You need to [install stack](https://docs.haskellstack.org/en/v1.1.2/install_and_upgrade/) first if you want to use the system in the standard way. 

To build, type
``stack build``.
This may take some time (>1 hour) if it is your first time ever using stack.

For plotting, install Python packages required for the `matplotlib` Haskell wrapper: `python3 -m pip install -U matplotlib numpy tk scipy`.

To run, type
``stack run wiener-exe`` or ``stack run regression-exe`` or ``stack run clustering-exe``, or explore with ``stack ghci``. 

For the Gaussian process example, the `hmatrix` library requires the GSL, BLAS and LAPACK development packages. On Ubuntu, you can install them with `sudo apt-get install libgsl0-dev liblapack-dev`.


### Contributing

The core of the library can be found in `src/LazyPPL.hs` and some useful, common, and interesting distributions can be found in `src/Distr.hs` and `src/Distr/`.

* `src/LazyPPL.hs` contains a likelihood-weighted importance sampling algorithm (`lwis`) and a Metropolis-Hastings algorithm, together with the basic monads. There are two monads, `Prob` and `Meas`. 
    * `Prob` is to be thought of as an affine monad of probability measures. It comes with various predefined probability distributions such as `uniform` (in `src/LazyPPL.hs`), `normal`, `exponential`, `beta`, `poisson`, `dirichlet`, `iid`, etc (in `src/Distr.hs`). 
    * `Meas` is to be thought of as a non-affine monad of unnormalized measures. It provides an interface using `sample` (which draws from a `Prob`) and `score` (which multiplies the current trace by a given likelihood weight). 
    * Our first Metropolis Hastings algorithm `mh` takes as an argument a probability `p` of changing any given site of the rose tree of random seeds. This is different from single-site MH (which picks exactly one site to change each step) and multi-site MH (which changes all sites at each step). The reason for this is that, in the lazy setting, we cannot explore how many active sites there are without triggering more computation (which would require a lot of Haskell hacking). We can approximate single-site MH by putting `p=1/num_sites` and recover multi-site MH by putting `p=1`.
	* A second Metropolis Hastings algorithm `mh1` implements a single-site proposal kernel by inspecting the Haskell heap.
    * A key implementation idea is that the underlying sample space is `Tree`, which comprises a lazy tree of `Double`s that is infinitely wide and infinitely deep. Informally, at any moment, if you need some unknown or lazy amount of randomness, you can target just one branch of the tree, without worrying about affecting the other branches. That branch will itself be a `Tree`, so this can all happen recursively without any problems. 
* `src/Distr.hs` contains common distributions such as, among others, the normal, exponential, gamma, dirichlet distributions. We find that Haskell types are quite illuminating to construct sophisticated distributions. For example, abstract types capture some essence of exchangeability, for example:
    * in the Chinese Restaurant Process, `newCustomer :: Restaurant -> Prob Table` (`src/Distr/DirichletP.hs`);
    * in the Indian Buffet Process, `newCustomer :: Restaurant -> Prob [Dish]` (`src/Distr/IBP.hs`).

The rest of the files in the `src/` directory consists of various examples of probabilistic models that we can write using our library.

### Writing your own probabilistic models

If you write your own Haskell module, add it to `package.yaml` first (remembering to change the `main:` and `-main-is` parts accordingly), and then execute `stack run` or `stack ghci` as needed.

For example, if the module is named `ReviewerTest` (saved as `src/ReviewerTest.hs`), you will need to add the following to `package.yaml`:
  
    reviewertest-exe:
      main:                ReviewerTest.hs
      source-dirs:         src
      ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
      - -main-is ReviewerTest
      dependencies:
      - lazyppl

Then, run it with `stack run reviewertest-exe` or open it up in the REPL using `stack ghci src/ReviewerTest.hs`.
