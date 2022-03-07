{-# LANGUAGE BangPatterns #-}

module Distr where

import LazyPPL
import Statistics.Distribution
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import qualified Statistics.Distribution.Poisson as Poisson
import Data.List
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import Control.Monad
import Control.Monad.Extra
import System.IO.Unsafe
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Debug.Trace


normal :: Double -> Double -> Prob Double
normal m s = do 
  x <- uniform
  return $ quantile (normalDistr m s) x

normalPdf :: Double -> Double -> Double -> Double
normalPdf m s = density $ normalDistr m s

exponential :: Double -> Prob Double
exponential rate = do 
  x <- uniform
  return $ - (log x / rate)

expPdf :: Double -> Double -> Double
expPdf rate x = exp (-rate*x) * rate

gamma :: Double -> Double -> Prob Double
gamma a b = do
  x <- uniform
  return $ quantile (gammaDistr a b) x

beta :: Double -> Double -> Prob Double
beta a b = do
  x <- uniform
  return $ quantile (betaDistr a b) x

poisson :: Double -> Prob Integer
poisson lambda = do
  x <- uniform
  let cmf = scanl1 (+) $ map (probability $ Poisson.poisson lambda) [0,1..]
  let (Just n) = findIndex (> x) cmf
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
  return $ (upper - lower) * x + lower

bernoulli :: Double -> Prob Bool
bernoulli r = do
  x <- uniform
  return $ x < r

{-
  uniform distribution on [0, ..., n-1]
-}
uniformdiscrete :: Int -> Prob Int
uniformdiscrete n =
  do
    let upper = fromIntegral n
    r <- uniformbounded 0 upper
    return $ floor r

{-- Categorical distribution: takes a list of k numbers that sum to 1, 
    and returns a number between 0 and (k-1) --}
categorical :: [Double] -> Prob Int
categorical xs = do 
  r <- uniform
  case findIndex (>r) $ tail $ scanl (+) 0 xs of
    Just i -> return i
    Nothing -> error "categorical: probabilities do not sum to 1"


iid :: Prob a -> Prob [a]
iid p = do r <- p; rs <- iid p; return $ r : rs