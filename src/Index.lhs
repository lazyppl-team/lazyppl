---
title: LazyPPL
---

<section class="hero">
<div class="hero-content">
<h1 class="hero-title"><img src="images/logo.png" width="72" height="72" class="hero-logo" alt="LazyPPL">LazyPPL</h1>
<p class="hero-tagline">Haskell library for Bayesian probabilistic programming</p>
<div class="hero-actions">
<div class="lazy-bars bars-left">
<div class="bar bar-1"></div>
<div class="bar bar-2"></div>
<div class="bar bar-3"></div>
</div>
<a href="https://github.com/lazyppl-team/lazyppl" class="btn btn-primary"><i class="fa-brands fa-github"></i> View on GitHub</a>
<a href="MinimalDemo.html" class="btn btn-secondary">Get Started</a>
<div class="lazy-bars bars-right">
<div class="bar bar-1"></div>
<div class="bar bar-2"></div>
<div class="bar bar-3"></div>
</div>
</div>
</div>
</section>

<section class="examples-section" id="examples">
<h2 class="examples-heading">Explore Examples</h2>
<p class="examples-intro">LazyPPL is a Haskell library for Bayesian probabilistic programming. It supports lazy use of probability, and we provide new Metropolis-Hastings algorithms to allow this. LazyPPL is inspired by recent ideas in synthetic probability theory and synthetic measure theory, such as <a href="https://ncatlab.org/nlab/show/quasi-Borel+space">quasi-Borel spaces</a> and <a href="https://ncatlab.org/nlab/show/Markov+category">Markov categories</a>. Laziness appears to be a good paradigm for non-parametric statistics. LazyPPL is inspired by many other languages, including <a href="v1.probmods.org">Church</a>, <a href="https://probprog.github.io/anglican/">Anglican</a>, and <a href="https://www.tweag.io/blog/2019-09-20-monad-bayes-1/">MonadBayes</a>. Several aspects are now incorporated into MonadBayes (see <a href="https://www.tweag.io/blog/2022-10-18-monad-bayes-fellowship/">here</a>).</p>
<div class="examples-grid">
<article class="example-card">
<a href="RegressionDemo.html" class="card-preview"><img src="images/regression-piecewise-reg.svg" alt="Linear and piecewise linear regression"></a>
<div class="card-content">
<span class="card-tag">Regression</span>
<h3><a href="RegressionDemo.html">Linear & Piecewise Regression</a></h3>
<p>Start simple with Bayesian linear regression, then extend to piecewise models using a Poisson point process.</p>
</div>
</article>
<article class="example-card">
<a href="WienerDemo.html" class="card-preview"><img src="images/wiener-reg.svg" alt="Wiener process regression"></a>
<div class="card-content">
<span class="card-tag">Stochastic Processes</span>
<h3><a href="WienerDemo.html">Wiener Process Regression</a></h3>
<p>Model time series with Wiener process priors for continuous, nowhere-differentiable random functions.</p>
</div>
</article>
<article class="example-card">
<a href="ProgramInductionDemo.html" class="card-preview"><img src="images/programinduction-reg.svg" alt="Program induction"></a>
<div class="card-content">
<span class="card-tag">Program Induction</span>
<h3><a href="ProgramInductionDemo.html">Program Induction</a></h3>
<p>Infer arithmetic expressions that explain observed data using probabilistic program induction.</p>
</div>
</article>
<article class="example-card">
<a href="ClusteringDemo.html" class="card-preview"><img src="images/clustering-map.svg" alt="Dirichlet Process clustering"></a>
<div class="card-content">
<span class="card-tag">Clustering</span>
<h3><a href="ClusteringDemo.html">Non-parametric Clustering</a></h3>
<p>Dirichlet Process mixture models that discover the number of clusters automatically.</p>
</div>
</article>
<article class="example-card">
<a href="GraphDemo.html" class="card-preview"><img src="images/graph-data.svg" alt="Observed graph for inference"></a>
<div class="card-content">
<span class="card-tag">Graph Inference</span>
<h3><a href="GraphDemo.html">Graph Inference</a></h3>
<p>Infer parameters and structure of random graphs, such as geometric versus Erdős–Rényi models, from observed adjacency data.</p>
</div>
</article>
<article class="example-card">
<a href="Physics.html" class="card-preview"><img src="images/physics4.svg" alt="2D physics bumper configuration"></a>
<div class="card-content">
<span class="card-tag">Physics</span>
<h3><a href="Physics.html">Inference in a Physics Model</a></h3>
<p>Perform Bayesian inference in a 2D physics simulation, inferring bumper configurations so a falling ball lands in a cup.</p>
</div>
</article>
</div>
</section>

<h2 class="intro-title" id="two-monad-architecture">Two-Monad Architecture</h2>

LazyPPL provides two monads:

* `Prob a`{.haskell}: probability measures, supporting probability measure such as
  `uniform :: Prob Double`{.haskell},
  `normal :: Double -> Double -> Prob Double`{.haskell},
  `bernoulli :: Double -> Prob Bool`{.haskell}.
  This is lazy, in other words it is an affine monad.

* `Meas a`{.haskell}: unnormalized measures, as used in Monte Carlo simulations for Bayesian
  statistics. There are two key functions:
    * `sample :: Prob a -> Meas a`{.haskell}, which samples from a probability measure;
    * `score :: Double -> Meas ()`{.haskell}, which weights a measure by a given value,
      typically coming from a likelihood function.

<h2 class="intro-title" id="simple-example">Simple example</h2>

To illustrate the basic usage, here is a very simple first example,
that doesn’t use laziness. More advanced examples are in the menu above,
and further examples in the <a
href="https://github.com/lazyppl-team/lazyppl">GitHub
repository</a>.

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
{-# LANGUAGE ExtendedDefaultRules #-}
module Index where
import LazyPPL
import LazyPPL.Distributions
import Graphics.Matplotlib hiding (density)
import Data.List
\end{code}
</details>

Suppose we we know that there are fewer buses on Sundays than on
other days. I notice 4 buses in an hour, what is the probability it is a
Sunday? 

\begin{code}
model :: Meas Bool
model = do
  -- Prior belief: it is Sunday with prob. 1/7
  sunday <- sample $ bernoulli (1/7)
  -- I know the rates of buses on the different days:
  let rate = if sunday then 3 else 10
  -- observe 4 buses
  score $ poissonPdf rate 4
  return sunday
\end{code}

We run a Metropolis-Hastings simulation to get a stream of draws from
this unnormalized measure. We plot a histogram of the results, which
shows the posterior probability that it is Sunday, given that we saw 4
buses.

\begin{code}
inference :: IO ()
inference = do
  xws <- mh 1 model
  plotHistogram "images/index-posterior.svg" (map fst $ take 1000 xws)
\end{code}

![Posterior probability of Sunday given 4 buses observed](images/index-posterior.svg)

<details class="code-details">
<summary>Plotting utilities</summary>
\begin{code}
plotHistogram :: (Show a , Eq a) => String -> [a] -> IO ()
plotHistogram filename xs = do
  putStrLn $ "Generating " ++ filename ++ "..."
  let categories = nub xs
  let counts = map (\c -> length $ filter (==c) xs) categories
  file filename $ bar (map show categories) $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
  putStrLn $ "Done."

main = do {inference}
\end{code}
</details>
