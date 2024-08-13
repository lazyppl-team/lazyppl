{- | Handy distributions for the `LazyPPL` library, based on the `uniform` distribution. Mostly defined using the `Statistics.Distribution` module and family. 

Sometimes both a distribution (type @Prob a@) and pdf (type @a -> Double@) are given. Distributions are useful for sampling, densities are used for scoring. -}


module LazyPPL.Distributions (
       -- * Continuous distributions
       normal,normalPdf,exponential,expPdf,gamma, beta, dirichlet, uniformbounded,
       -- * Discrete distributions
       bernoulli, uniformdiscrete, categorical, poisson, poissonPdf,
       -- * Streams
       iid)
       where

import LazyPPL (Prob,uniform)
import Statistics.Distribution (quantile, density, probability)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Gamma (gammaDistr)
import qualified Statistics.Distribution.Poisson as Poisson (poisson) 
import Data.List (findIndex)

{-|
  [Normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
-}
normal :: Double -- ^ mu, mean
       -> Double -- ^ sigma, standard deviation
       -> Prob Double
normal m s = do 
  x <- uniform
  return $ quantile (normalDistr m s) x

normalPdf :: Double -> Double -> Double -> Double
normalPdf m s = density $ normalDistr m s

{-|
  [Exponential distribution](https://en.wikipedia.org/wiki/Exponential_distribution)
-}
exponential :: Double -- ^ lambda, rate
            -> Prob Double
exponential rate = do 
  x <- uniform
  return $ - (log x / rate)

expPdf :: Double -> Double -> Double
expPdf rate x = exp (-rate*x) * rate

{-|
  [Gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
-}
gamma :: Double -- ^ k, shape
      -> Double -- ^ theta, scale
      -> Prob Double
gamma a b = do
  x <- uniform
  return $ quantile (gammaDistr a b) x

{-|
  [Beta distribution](https://en.wikipedia.org/wiki/Beta_distribution)
-}
beta :: Double -- ^ alpha
     -> Double -- ^ beta
     -> Prob Double
beta a b = do
  x <- uniform
  return $ quantile (betaDistr a b) x

{-|
  [Poisson distribution](https://en.wikipedia.org/wiki/Poisson_distribution)
-}
poisson :: Double -- ^ lambda, rate
        -> Prob Integer
poisson lambda = do
  x <- uniform
  let cmf = scanl1 (+) $ map (probability $ Poisson.poisson lambda) [0,1..]
  let (Just n) = findIndex (> x) cmf
  return $ fromIntegral n

poissonPdf :: Double -> Integer -> Double
poissonPdf rate n = probability (Poisson.poisson rate) (fromIntegral n)

{-|
  [Dirichlet distribution](https://en.wikipedia.org/wiki/Dirichlet_distribution)
-}
dirichlet :: [Double] -- ^ vector of alphas; length is dimension
          -> Prob[Double]
dirichlet as = do
  xs <- mapM (\a -> gamma a 1) as
  let s = Prelude.sum xs
  let ys = map (/ s) xs
  return ys

-- | [Continuous uniform distribution on a bounded interval](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
uniformbounded :: Double -- ^ lower
               -> Double -- ^ upper
               -> Prob Double
uniformbounded lower upper = do
  x <- uniform
  return $ (upper - lower) * x + lower

-- | [Bernoulli distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
bernoulli :: Double -- ^ bias
          -> Prob Bool
bernoulli r = do
  x <- uniform
  return $ x < r

{-|
  [Discrete uniform distribution](https://en.wikipedia.org/wiki/Discrete_uniform_distribution) on [0, ..., n-1]
-}
uniformdiscrete :: Int -- ^ n
                -> Prob Int
uniformdiscrete n =
  do
    let upper = fromIntegral n
    r <- uniformbounded 0 upper
    return $ floor r

{-| [Categorical distribution](https://www.google.com/search?client=safari&rls=en&q=categorical+distribution&ie=UTF-8&oe=UTF-8): Takes a list of k numbers that sum to 1, 
    and returns a random number between 0 and (k-1), weighted accordingly -}
categorical :: [Double] -> Prob Int
categorical xs = do 
  r <- uniform
  case findIndex (>r) $ tail $ scanl (+) 0 xs of
    Just i -> return i
    Nothing -> error "categorical: probabilities do not sum to 1"

{-| Returns an infinite stream of samples from the given distribution. --}
iid :: Prob a -> Prob [a]
iid p = do r <- p; rs <- iid p; return $ r : rs
