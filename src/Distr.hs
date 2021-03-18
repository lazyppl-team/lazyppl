module Distr where

import LazyPPL
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import qualified Statistics.Distribution.Poisson as Poisson
import Data.List
import Control.Spoon

import Debug.Trace


normal :: Double -> Double -> Prob Double
normal m s = do x <- uniform
                return $ quantile (normalDistr m s) x

normalPdf :: Double -> Double -> Double -> Double
normalPdf m s = density $ normalDistr m s

exponential :: Double -> Prob Double
exponential rate =
  do x <- uniform
     return $ - (log x / rate)

expPdf :: Double -> Double -> Double
expPdf rate x = exp (-rate*x) * rate

gamma :: Double -> Double -> Prob Double
gamma a b = do
  x <- uniform
  return $ quantile (gammaDistr a b) x 

-- Modified to catch a rare bug
-- in the Haskell implementation of the
-- inverse incomplete beta function.
-- (Not a fix, just resample if the bug occurs.)
beta :: Double -> Double -> Prob Double
beta a b = do
  x <- uniform
  case (spoon $ quantile (betaDistr a b) (min x 1)) of
    Just x -> return x
    Nothing -> beta a b
--  return $ quantile (betaDistr a b) (min x 1) 

poisson :: Double -> Prob Integer
poisson lambda = do
  x <- uniform
  let cmf = scanl1 (+) $ map (probability $ Poisson.poisson lambda) $ [0,1..]
  let (Just n) = findIndex (\r -> r > x) cmf
  return $ fromIntegral n

poissonPdf :: Double -> Integer -> Double
poissonPdf rate n = probability (Poisson.poisson rate) (fromIntegral n)

dirichlet :: [Double] -> Prob[Double]
dirichlet as = do
  xs <- mapM exponential as
  let s = Prelude.sum xs
  let ys = map (/ s) xs
  return ys

uniformbounded :: Double -> Double -> Prob Double
uniformbounded lower upper = do
  x <- uniform
  return $ (upper - lower) * x - lower

bernoulli :: Double -> Prob Bool
bernoulli r = do
  x <- uniform
  return $ x < r
