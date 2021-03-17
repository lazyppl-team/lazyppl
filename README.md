# lazyppl

> "lazyppl accomplish more..."

This is provides a simple Metropolis-Hastings implementation that works with lazy programs, together with some examples.

To try different examples, change ``app/Main.hs``
or use ``stack ghci``.
The source is in ``src/`` .

## Simple probabilistic programming library

* [``LazyPPL.hs``](src/LazyPPL.hs) contains a likelihood weighted importance sampling algorithm (lwis) and a Metropolis-Hastings algorithm, together with the basic monads. There are two monads, `Prob` and `Meas`. `Prob` is to be thought of as a monad of probability measures, and it provides a function `uniform`. `Meas` is to be thought of as a monad of unnormalized measures, and it provides an interface using `sample` (which draws from a `Prob`) and `score` (which weights a trace, typically by a likelihood). 
* [``Distr.hs``](src/Distr.hs) contains common distributions such as normal distributions etc..

## Examples

* [``Regression.hs``](src/Regression.hs) contains the piecewise linear regression, where the change points are drawn from a lazy Poisson process. 
* [``Clustering.hs``](src/Clustering.hs) contains some simple clustering examples, where the number of clusters is unknown. These programs make use of lazy stick-breaking and also memoization.  
* [``Wiener.hs``](src/Wiener.hs) contains a simple implementation of regression using a Wiener process. This appears to be a bona fide random function R->R which is constructed lazily. 

## Installation

The system uses Haskell stack.
You need to [install stack](https://docs.haskellstack.org/en/v1.1.2/install_and_upgrade/) first if you want to use the system in the
standard way.

To build, type
``stack build``.
This may take some time (>1 hour) if it is your first time ever using stack.

To run, type
``stack run`` or ``stack ghci``.  
