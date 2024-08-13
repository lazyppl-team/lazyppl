---
title: Linear and piecewise linear regression with LazyPPL
---

We discuss Bayesian [linear regression](#linearRegression) and [piecewise linear regression](#piecewiseLinearRegression). 
Our piecewise linear regression uses an infinite Poisson process as the set of change points. 
The laziness of Haskell effectively truncates the infinite process as needed.
The examples also demonstrate that higher-order functions (such as `regress` and `splice`) are very useful. 

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
module RegressionDemo where
import LazyPPL
import LazyPPL.Distributions
import Data.Colour
import Data.Colour.Names
import Control.Monad

import Graphics.Matplotlib hiding (density)
\end{code}
</details>


Linear regression {#linearRegression}
---------


Regression is about finding a fitting function to some data. Bayesian regression is about finding a
posterior distribution on functions, given the data.

We start with a random linear function: 

\begin{code}
linear :: Prob (Double -> Double)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f
\end{code}
Note that this returns a random function. 
Here are 1000 draws from the distribution.
![](images/regression-linear-prior.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotLinearPrior =
  do
    fs' <- mh 1 (sample linear) 
    let fs = map fst $ take 1000 $ fs'
    plotFuns "images/regression-linear-prior.svg" [] fs 0.1
\end{code}
</details>
<br></br>

Here is a sample dataset that we will find a function for:
\begin{code}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]
\end{code}
![](images/regression-dataset.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotDataset =
  do
    plotFuns "images/regression-dataset.svg" dataset [] 0.1
\end{code}
</details>
<br></br>

Our regression here is noisy: the function has not _precisely_ generated this data set, because the points are not colinear. 

Our generic regression function takes a random function `prior`, and some input/output observations `dataset`, 
    which are assumed to be noisy according to `sigma`, 
    returns a conditioned random linear function (unnormalized).
\begin{code}
regress :: Double -> Prob (a -> Double) -> [(a, Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do
    f <- sample prior
    forM_ dataset (\(x, y) -> score $ normalPdf (f x) sigma y)
    return f
\end{code}
Now we can run Bayesian linear regression by sampling from the unnormalized measure using Metropolis-Hastings. The result is the posterior distribution over linear functions.
\begin{code}
plotLinReg =
  do fs' <- mh 0.5 (regress 0.5 linear dataset)
     let fs = map fst $ take 1000 $ every 50 $ drop 100 fs'
     plotFuns "images/regression-linear-reg.svg" dataset fs 0.01
\end{code}
![](images/regression-linear-reg.svg)


Piecewise linear regression and Poisson point processes {#piecewiseLinearRegression}
-----

To move to piecewise linear regression, we introduce a function `splice` which splices together different draws from a random function at a random selection of change points. NB if the point process is infinite then the resulting function has an infinite number of pieces, but this is all done lazily, so it's not a problem. 
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
\end{code}

Note that this second-order function works for any point process and for any random function. We will use the random linear function `linear`, and for a point process we will use the following Poisson point process, `poissonPP`. This generates an infinite random list of points, where the gaps between them are exponentially distributed.
\begin{code}
poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)
\end{code}
Here are five draws from the process. Each draw is really an infinite set of points, but we have truncated the display to the viewport [0,20]. Laziness then takes care of truncating the infinite sequences appropriately.
![](images/regression-poissonpp.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotPoissonPP =
  do
    pws <- mh 1 $ sample $ poissonPP 0 0.1
    let ps = map (takeWhile (20>)) $ map fst $ take 5 $ pws
    let filename = "images/regression-poissonpp.svg"
    putStrLn $ "Plotting " ++ filename ++ "..."
    let myscatter mpl i = mpl % setSubplot i % scatter (ps !! i) (map (const (0::Double)) (ps !! i)) @@ [o2 "s" (10::Int),o2 "c" "black"]  % xlim (0::Int) (20::Int) % ylim (-1 :: Int) (1::Int) % mp # "ax.yaxis.set_major_formatter(mticker.NullFormatter())"
    let myscatteraxes mpl i = if i < (length ps - 1) then myscatter mpl i % mp # "ax.xaxis.set_major_formatter(mticker.NullFormatter())" else myscatter mpl i
    file filename $ foldl myscatteraxes (subplots @@ [o2 "nrows" (length ps),o2 "ncols" (1::Int)]) [0..(length ps - 1)]
    putStrLn $ "Done."
\end{code}
</details>
<br></br>

We can now invoke a random piecewise linear function by calling `splice (poissonPP 0 0.1) linear`{.haskell}. 
Here are ten draws from this distribution. Because the viewport is bounded, laziness takes care of truncations to the point process that we passed to `splice`.

![](images/regression-piecewise-prior.svg)

<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotPiecewisePrior =
  do
    fs' <- mh 1 $ sample $ splice (poissonPP 0 0.1) linear
    let fs = map fst $ take 10 $ fs'
    plotFuns "images/regression-piecewise-prior.svg" [] fs 1
\end{code}
</details>
<br></br>
This is a random function just like any other, so we can use it as a prior, using the same regression routines as before, `regress 0.1 (splice (poissonPP 0 0.1) linear)`{.haskell}. We can then sample from the unnormalized distribution using Metropolis-Hastings.
\begin{code}
plotPiecewiseReg =
  do
    fs' <- mhirreducible 0.2 0.1 (regress 0.1 (splice (poissonPP 0 0.1) linear) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 fs'
    plotFuns "images/regression-piecewise-reg.svg" dataset fs 0.01
\end{code}
![](images/regression-piecewise-reg.svg)

Finally, we can also do our regression using piecewise constant functions. Our prior will now be `randConst`{.haskell}, a random linear function with slope 0.
\begin{code}
randConst :: Prob (Double -> Double)
randConst =
  do
    b <- normal 0 3
    let f = \x -> b
    return f
\end{code}
Using the same recipe as before, we can construct our prior by calling `splice (poissonPP 0 0.1) randConst`{.haskell} and perform inference on it to get the resultant unnormalized distribution of piecewise constant functions.
\begin{code}
plotPiecewiseConst =
  do
    fs' <- mhirreducible 0.2 0.1 (regress 0.1 (splice (poissonPP 0 0.1) randConst) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 fs'
    plotFuns "images/regression-piecewise-const.svg" dataset fs 0.01
\end{code}
![](images/regression-piecewise-const.svg)


<details class="code-details">
<summary>Graphing routines</summary>
\begin{code}
-- Plot the points drawn from weighted samples
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interestingPoints :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interestingPoints f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon
    then acc
    else interestingPoints f lower mid epsilon delta (mid : (interestingPoints f mid upper epsilon delta acc))

sampleFun f =
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interestingPoints f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs


plotFuns :: String -> [(Double,Double)] -> [Double -> Double] -> Double -> IO ()
plotFuns filename dataset funs alpha = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a f -> let xfs = sampleFun f in a % plot (map fst xfs) (map snd xfs) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" alpha, o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim (-2 :: Int) (10 :: Int)) funs
        putStrLn "Done."
        return ()


main :: IO ()
main = do {plotLinearPrior ; plotDataset ; plotLinReg ; plotPiecewisePrior ; plotPoissonPP ; plotPiecewiseReg ; plotPiecewiseConst }
\end{code}
</details>
