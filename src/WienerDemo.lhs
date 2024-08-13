---
title: Wiener process regression in LazyPPL
---

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
module WienerDemo where
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.GP (wiener)

import Data.List
import Data.Map (empty,lookup,insert,size,keys)
import Control.Monad
import Data.IORef
import System.IO.Unsafe

import Graphics.Matplotlib hiding (density)
\end{code}
</details>
<br>

We can define a random function `wiener :: Prob(Double -> Double)`{.haskell} which describes a Wiener process.
This requires an infinite number of random choices, first because it is defined for arbitrarily large `x`, but also it needs an infinite number of random choices in each finite interval, because it is no-where differentiable. This is all dealt with using laziness. We can't plot this unbounded, undifferentiable function precisely, but when we plot it with a fixed resolution and viewport, the necessary finite random choices are triggered. 
![](images/wiener-prior.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotWienerPrior =
  do
    fws <- mh 1 $ sample wiener
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.1..6]) $ map fst $ take 100 $ fws
    plotCoords "images/wiener-prior.svg" [] xys (-7) 7 0.2
\end{code}
</details>
<br>
We will use this random function as a prior for Bayesian regression, as in [the other regression examples](RegressionDemo.html). Here is our example data set: 
\begin{code}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]
\end{code}
And here is our model where we combine a Wiener function `g` plus a random start point `a`.
(Note that we are treating this `g` as a function like any other. And we could also have built this model with the second-order `regress` function from [the other regression examples](RegressionDemo.html).)
\begin{code}
example :: Meas (Double -> Double)
example = do g <- sample wiener
             a <- sample $ normal 0 3
             let f x = a + 2 * (g x)
             forM_ dataset (\(x,y) -> score $ normalPdf (f x) 0.3 y)
             return f
\end{code}
We can now sample from the unnormalized distribution, using Metropolis-Hastings. 
Because of laziness, the values of the functions will be sampled at different times,
some only when we come to plot the functions. 
\begin{code}
plotWienerRegression =
  do
    fws <- mh 0.1 example
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.1..6]) $ map fst $ take 100 $ every 1000 $ drop 10000 $ fws
    plotCoords "images/wiener-reg.svg" dataset xys (-2) 10 0.1
\end{code}
![](images/wiener-reg.svg)


Jump diffusion compositionally {#jump}
-----


Recall the splice function `splice :: Prob [Double] -> Prob (Double -> Double) -> Prob (Double -> Double)` from [the linear regression example](RegressionDemo.html#piecewiseLinearRegression), which pieces together draws from a random function using a point process.
We can immediately apply this to the Wiener process, to get a jump diffusion process.

<details class="code-details">
<summary>Recalling splice function and Poisson point process</summary>
\begin{code}
splice :: Prob [Double] -> Prob (Double -> Double) -> Prob (Double -> Double)
splice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let h :: [(Double, Double -> Double)] -> Double -> Double
        h [] x = default_f x
        h ((a, f) : xfs) x | x <= a = f x
        h ((a, f) : xfs) x | x > a = h xfs x
    return (h (zip xs fs))

poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)

regress :: Double -> Prob (a -> Double) -> [(a, Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do
    f <- sample prior
    forM_ dataset (\(x, y) -> score $ normalPdf (f x) sigma y)
    return f
\end{code}
</details>

\begin{code}
jump :: Prob (Double -> Double)
jump = let p = do f <- wiener
                  a <- normal 0 3
                  return $ \x -> a + f x
       in splice (poissonPP 0 0.2) p
\end{code}
Here are six samples from this distribution.

![](images/wiener-jump-prior.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotJumpPrior =
  do
    fws <- mh 1 $ sample jump
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.02..6]) $ map fst $ take 6 $ fws
    plotCoords "images/wiener-jump-prior.svg" [] xys (-7) 7 1
\end{code}
</details>

We use a slightly different dataset to illustrate regression with this.
\begin{code}
datasetB :: [(Double, Double)]
datasetB = [(0,0.6), (1, 0.7), (2,8.2), (3,9.1), (4,3.2), (5,4.9), (6,2.9)]

plotJumpRegression =
  do
    fws <- mhirreducible 0.2 0.1 (regress 0.3 jump datasetB)
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.02..6]) $ map fst $ take 100 $ every 1000 $ drop 300000 $ fws
    plotCoords "images/wiener-jump-reg.svg" datasetB xys (-2) 10 0.1
\end{code}
![](images/wiener-jump-reg.svg)


<details class="code-details">
<summary>Graphing routines</summary>
\begin{code}
plotCoords :: String -> [(Double,Double)] -> [[(Double,Double)]] -> Double -> Double -> Double -> IO ()
plotCoords filename dataset xyss ymin ymax alpha = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a xys -> a % plot (map fst xys) (map snd xys) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" alpha, o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim ymin ymax) xyss
        putStrLn "Done."
        return ()
    

main :: IO ()
main = do { plotWienerPrior ; plotWienerRegression ; plotJumpPrior ; plotJumpRegression } 
\end{code}
</details>