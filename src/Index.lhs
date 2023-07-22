---
title: <img src="images/logo.png" width="60" height="60" class="align-bottom d-inline-block" id="title-logo" alt="">LazyPPL
---

LazyPPL is a Haskell library for Bayesian probabilistic programming. It supports lazy use of probability, and we provide new Metropolis-Hastings algorithms to allow this. LazyPPL is inspired by recent ideas in synthetic probability theory and synthetic measure theory, such as [quasi-Borel spaces](https://ncatlab.org/nlab/show/quasi-Borel+space) and [Markov categories](https://ncatlab.org/nlab/show/Markov+category). Laziness appears to be a good paradigm for non-parametric statistics. LazyPPL is inspired by many other languages, including [Church](v1.probmods.org), [Anglican](https://probprog.github.io/anglican/), and [MonadBayes](https://www.tweag.io/blog/2019-09-20-monad-bayes-1/). Several aspects are now incorporated into MonadBayes (see [here](https://www.tweag.io/blog/2022-10-18-monad-bayes-fellowship/)).

LazyPPL provides two monads:

* `Prob a`{.haskell}: probability measures, supporting probability measure such as `uniform :: Prob Double`{.haskell}, `normal :: Double -> Double -> Prob Double`{.haskell}, `bernoulli :: Double -> Prob Bool`{.haskell}. This is lazy, in other words it is an affine monad.
* `Meas a`{.haskell}: unnormalized measures, as used in Monte Carlo simulations for Bayesian statistics. There are two key functions:
  * `sample :: Prob a -> Meas a`{.haskell}, which samples from a probability measure;
  * `score :: Double -> Meas ()`{.haskell}, which weights a measure by a given value, typically coming from a likelihood function.


Simple example
---

To illustrate the basic usage, here is a very simple first example, that doesn't use laziness. More advanced examples are in the menu above, and further examples in the [GitHub repository](https://github.com/lazyppl-team/lazyppl).


<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
{-# LANGUAGE ExtendedDefaultRules #-}
module Index where
import LazyPPL
import Distr
import Graphics.Matplotlib hiding (density)
import Data.List
\end{code}
</details>
<br>
Suppose we we know that there are fewer buses on Sundays than on other days. I notice 4 buses in an hour, what is the probability it is a Sunday? 
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
We run a Metropolis-Hastings simulation to get a stream of draws from this unnormalized measure. We plot a histogram of the results, which shows the posterior probability that it is Sunday, given that we saw 4 buses.
\begin{code}
inference :: IO ()
inference = do
  xws <- mh 1 model
  plotHistogram "images/index-posterior.svg" (map fst $ take 1000 xws)
\end{code}
![](images/index-posterior.svg)
<details class="code-details">
<summary>Code for plotting histograms.</summary>
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
