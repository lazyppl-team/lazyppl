module Regression where
import LazyPPL
import Distr
import Data.Colour
import Data.Colour.Names

import Graphics.Matplotlib hiding (density)


{- | Poisson process starting with given start point and rate.
    NB We don't specify an upper bound, it's a lazy infinite list -}
poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)

{- | Given a point process and a random function, slice together different draws of the function
    at the given points. NB if the point process is infinite then the resulting function has
    an infinite number of pieces, but this is all done lazily -}
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

{- | Random linear function -}
linear :: Prob (Double -> Double)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f

{- | Regression: a random function "prior", and some input/output observations "dataset", 
    which are assumed to be noisy according to "sigma", 
    return a conditioned random linear function (unnormalized) -}
regress :: Double -> Prob (a -> Double) -> [(a, Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do
    f <- sample prior
    mapM_ (\(x, y) -> score $ normalPdf (f x) sigma y) dataset
    return f


{- | A sample dataset -}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

-- | plot the results of piecewise linear regression, using Metropolis Hastings
testPiecewiseRegression =
  do
    fs' <- mhirreducible 0.2 0.1 (regress 0.1 (splice (poissonPP 0 0.1) linear) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 fs'
    plotFuns "piecewise-reg.svg" dataset fs

-- | similar, but using likelihood weighted importance sampling
testPiecewiseRegressionLWIS =
  do
    fs' <- lwis 100000 $ regress 0.1 (splice (poissonPP 0 0.1) linear ) dataset
    let fs = take 500 fs'
    plotFuns "piecewise-regLWIS.svg" dataset fs

testPiecewiseRegressionMH1 =
  do
    fs' <- mh1 (regress 0.1 (splice (poissonPP 0 0.1) linear) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 fs'
    plotFuns "piecewise-regMH1.svg" dataset fs


{-- GRAPHING ROUTINES --}


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


plotFuns :: String -> [(Double,Double)] -> [Double -> Double] -> IO ()
plotFuns filename dataset funs = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a f -> let xfs = sampleFun f in a % plot (map fst xfs) (map snd xfs) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" (0.01 :: Double), o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim (-2 :: Int) (10 :: Int)) funs
        putStrLn "Done."
        return ()


main :: IO ()
main = do
          testPiecewiseRegression
          -- testPiecewiseRegressionMH1
          -- testPiecewiseRegressionLWIS
