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
  return $ (upper - lower) * x - lower

bernoulli :: Double -> Prob Bool
bernoulli r = do
  x <- uniform
  return $ x < r



{-- Stochastic memoization.
    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If a is finite, we could just sample all values of a in advance
    and avoid unsafePerformIO.
    If it is countably infinite, there probably also implementation tricks.
--}
memoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
memoize f = Prob $ do g <- get
                      let ( (Tree _ gs), g2) = splitTree g
                      put g2
                      return $ unsafePerformIO $ do
                                ref <- newIORef Data.Map.empty
                                return $ \x -> unsafePerformIO $ do
                                          m <- liftM (Data.Map.lookup x) (readIORef ref)
                                          case m of
                                              Just y -> return y
                                              Nothing -> do let (Prob m) = f x
                                                            n <- readIORef ref
                                                            let (y,_) = runState m (gs !! (1 + size n))
                                                            modifyIORef' ref (Data.Map.insert x y)
                                                            return y
