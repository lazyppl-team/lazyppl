module Regression where
import LazyPPL
import Distr
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names

import Data.Default.Class
import Control.Lens
import Data.Monoid 



{-- Poisson process starting with given start point and rate.
    NB We don't specify an upper bound, it's a lazy infinite list --} 
poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do step <- exponential rate
     let x = lower + step
     xs <- poissonPP x rate
     return (x : xs)

{-- Given a point process and a random function, slice together different draws of the function
    at the given points. NB if the point process is infinite then the resulting function has
    an infinite number of pieces, but this is all done lazily --}
splice :: Prob [Double] -> Prob (Double -> Double) -> Prob (Double -> Double)
splice pointProcess randomFun =
  do xs <- pointProcess
     fs <- mapM (\_ -> randomFun) xs
     default_f <- randomFun
     let h :: [(Double, Double -> Double)] -> Double -> Double
         h [] x = default_f x
         h ((a,f):xfs) x | x <= a = f x
         h ((a,f):xfs) x | x > a = h xfs x
     return (h (zip xs fs) )

{-- Random linear function --} 
linear :: Prob (Double -> Double)
linear =
  do a <- normal 0 3 
     b <- normal 0 3 
     let f = \x -> a*x + b
     return f

{-- Regression: a random function "prior", and some input/output observations "dataset", 
    which are assumed to be noisy according to "sigma", 
    return a conditioned random linear function (unnormalized) --} 
regress :: Double -> Prob (a -> Double) -> [(a,Double)] -> Meas (a -> Double)
regress sigma prior dataset =
  do f <- sample prior
     mapM (\(x,y) -> score $ normalPdf (f x) sigma y) dataset
     return f


{-- A sample dataset --}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

-- plot the results of piecewise linear regression, using Metropolis Hastings
testPiecewiseRegression =
  do
    fs' <- mh 0.2 (regress 0.1 (splice (poissonPP 0 0.1) linear) dataset)
    let fs = map fst $ take 500 $ every 1000 $ drop 10000 $ fs'
    plot_funs "piecewise-reg.svg" dataset fs

-- similar, but using likelihood weighted importance sampling
testPiecewiseRegressionLWIS =
  do
    fs' <- lwis 100000 $ regress 0.1 (splice (poissonPP 0 0.1) linear ) dataset
    let fs = take 500 $ fs'
    plot_funs "piecewise-regLWIS.svg" dataset fs



{-- GRAPHING ROUTINES --}
 

-- Plot the points drawn from weighted samples
-- More graphing routines
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interesting_points :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interesting_points f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon 
    then acc
    else interesting_points f lower mid epsilon delta (mid : (interesting_points f mid upper epsilon delta acc))
 
sample_fun f = 
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interesting_points f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs 

plot_funs :: String -> [(Double,Double)] -> [Double -> Double] -> IO ()
plot_funs filename dataset funs =
  let graphs  = map sample_fun funs                 in
  let my_lines  = plot_lines_style . line_color .~ blue `withOpacity` 0.01 
                $ plot_lines_values .~ graphs $ def in
  let my_dots = plot_points_style .~ filledCircles 4 (opaque black)
              $ plot_points_values .~ dataset
              $ def in               
  let my_layout = layout_plots .~ [toPlot my_lines , toPlot my_dots]
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (0,6)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (-2,10)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()



main :: IO ()
main = do { testPiecewiseRegression } 
