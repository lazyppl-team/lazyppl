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

beta :: Double -> Double -> Prob Double
beta a b = do
  x <- uniform
  return $ quantile (betaDistr a b) x

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
  let (Just i) = findIndex (>r) $ tail $ scanl (+) 0 xs
  return i

{-- Stochastic memoization.
    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If a is finite, we could just sample all values of a in advance
    and avoid unsafePerformIO.
    If it is countably infinite, there probably also are implementation tricks.
--}
memoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
memoize f =  Prob $ \(Tree _ gs) -> 
                    unsafePerformIO $ do
                             ref <- newIORef Data.Map.empty
                             return $ \x -> unsafePerformIO $ do
                                      m <- liftM (Data.Map.lookup x) (readIORef ref)
                                      case m of
                                           Just y -> return y
                                           Nothing -> do
                                                        n <- readIORef ref
                                                        let y = runProb (f x) (gs !! (1 + size n))
                                                        modifyIORef' ref (Data.Map.insert x y)
                                                        return y






{-- Stochastic memoization for recursive functions.
    Applying 'memoize' to a recursively defined function only memoizes at the
    top-level: recursive calls are calls to the non-memoized function.
    'memrec' is an alternative implementation which resolves recursion and
    memoization at the same time, so that recursive calls are also memoized.
--}
memrec :: Ord a => Show a => ((a -> b) -> (a -> Prob b)) -> Prob (a -> b)
memrec f =
   Prob $ \(Tree _ gs) -> 
          unsafePerformIO $ do
                  ref <- newIORef Data.Map.empty
                  let memoized_fixpoint = \x -> unsafePerformIO $ do
                                m <- liftM (Data.Map.lookup x) (readIORef ref)
                                case m of
                                      Just y -> return y
                                      Nothing -> do
                                                  n <- readIORef ref
                                                  let fix = f memoized_fixpoint
                                                  let y = runProb (fix x) (gs !! (1 + size n))
                                                  modifyIORef' ref (Data.Map.insert x y)
                                                  return y
                  return memoized_fixpoint
