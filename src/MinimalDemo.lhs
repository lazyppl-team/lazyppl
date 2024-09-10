---
title: Minimal examples with LazyPPL
---

Here are two minimal demonstrations of how to use LazyPPL, with no other libraries, to help with getting started. 

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
module MinimalDemo where
import LazyPPL
import LazyPPL.Distributions
import Data.List
\end{code}
</details>


Vote prediction example {#vote}
---------


Suppose I am standing for election in a large population, and that there are two candidates.
I run a poll over 100 people, and 51 say they will vote for me and 49 say they won't.
What is the chance that I will win the election? 

Here are the poll results:
\begin{code}
poll :: [Bool]
poll = replicate 51 True ++ replicate 49 False
\end{code}

Here is my model:
\begin{code}
-- Likelihood function of Bernoulli distribution.
bernoulliPdf r x = if x then r else (1-r)

voteModel :: Meas (Bool)
voteModel = do
  -- prior belief: vote share is unknown, so uniform
  voteShare <- sample uniform
  -- incorporate each poll result as an observation, using bernoulli likelihood
  mapM_ (\actualVote-> score (bernoulliPdf voteShare actualVote)) poll
  -- I win if my vote share is more than 50%.
  return $ voteShare > 0.5
\end{code}

<details class="code-details">
<summary>(Simulation code)</summary>
\begin{code}
runVoteModel =
  do
    xws <- mh 1 voteModel
    let n=10000 
    let m = length $ filter id $ map fst $ take n $ xws
    putStrLn $ "Chance of winning = " ++ (show (100 * (fromIntegral m) / (fromIntegral n) )) ++ "%"
\end{code}
</details>

When run, this should print the following (roughly).
```
  Chance of winning = 57.9%
```
(It may be tempting to think that the chance of winning should be 51%, but that is the _expected_ vote share, not the chance that the vote share is greater than 50%!)

This model can be solved analytically, as $\frac {\int_{0.5}^1 v^{51}(1-v)^{49}\,\mathrm{d}v}{\int_{0}^1 v^{51}(1-v)^{49}\,\mathrm{d}v}$.

A clever program analysis could derive this automatically using the Beta/Bernoulli conjugacy, but that is not implemented in LazyPPL.
<br></br>

Simulating a Poisson distribution {#poisson}
---------
Here is a simple example using laziness, but not using inference. 

A one-dimensional Poisson point process has steps that are exponentially distributed.
The following function produces this infinite stream, lazily. 
\begin{code}
poissonPP :: Double -> Double -> Prob [Double]
poissonPP start rate = do
  step <- exponential rate
  let next = start + step
  rest <- poissonPP next rate 
  return $ next : rest
\end{code}

We can simulate the Poisson distribution by counting how many points in the unit interval of a Poisson point process.
\begin{code}
poissonSim :: Double -> Prob Int
poissonSim rate = do
  xs <- poissonPP 0 rate
  return $ length $ takeWhile (<1) xs
\end{code}

<details class="code-details">
<summary>(Simulation code)</summary>
\begin{code}
runPoissonSim =
  do
    xws <- mh 1 $ sample $ poissonSim 1
    let n=10000
    let xs = sort $ map fst $ take n $ xws
    -- histogram generator
    let categories = nub xs
    let counts = map (\c -> length $ filter (==c) xs) categories
    putStrLn $ "k\tpoisson(k)"
    putStrLn $ "--\t----------"
    mapM_ (\(c,k) -> putStrLn $ (show c) ++ "\t" ++ (show $ (fromIntegral k)/(fromIntegral n))) (zip categories counts)
\end{code}
</details>
When run, this should print a Poisson distribution table (approximately).
```
  k       poisson(k)
  --      ----------
  0       0.3679
  1       0.3679
  2       0.1839
  3       6.13e-2
  4       1.53e-2
  5       3.1e-3
  6       0.5e-3
  7       1.0e-4
```
Note that it might often be better to use the LazyPPL `poisson` distribution than to use this simulation method. This simple example is just for illustration.
<br></br>

We can simulate the Poisson distribution by counting how many points in the unit interval of a Poisson point process.
\begin{code}
bernoulliPi4 :: Prob Bool
bernoulliPi4 = do
  x <- uniform
  y <- uniform
  return $ x * x + y * y < 1
\end{code}

<details class="code-details">
<summary>(Simulation code)</summary>
\begin{code}
runPi =
  do
    bws <- mh 1 $ sample $ bernoulliPi4
    let n=1000000
    let bs = map fst $ take n $ bws
    -- histogram generator
    putStrLn $ "pi = " ++ (show $ 4 * (fromIntegral $ length $ filter id bs) / (fromIntegral n))
\end{code}
</details>
When run, this should print a Poisson distribution table (approximately).
```
  k       poisson(k)
  --      ----------
  0       0.3679
  1       0.3679
  2       0.1839
  3       6.13e-2
  4       1.53e-2
  5       3.1e-3
  6       0.5e-3
  7       1.0e-4
```
Note that it might often be better to use the LazyPPL `poisson` distribution than to use this simulation method. This simple example is just for illustration.
<br></br>

<details class="code-details">
<summary>(Main function)</summary>
\begin{code}
main :: IO ()
main = do {runVoteModel ; putStrLn "" ; runPoissonSim }
\end{code}
</details>
