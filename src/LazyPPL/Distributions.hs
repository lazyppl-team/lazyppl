{- | Handy distributions for the `LazyPPL` library, based on the `uniform` distribution. Mostly defined using the `Statistics.Distribution` module and family. 

Sometimes both a distribution (type @Prob a@) and pdf (type @a -> Double@) are given. Distributions are useful for sampling, densities are used for scoring. 

For more distributions, see the Statistics.Distribution in the statistics package. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}


module LazyPPL.Distributions (
        -- * Continuous distributions
        normal,normalPdf,exponential,expPdf,gamma, beta, dirichlet, uniformbounded,
        -- * Discrete distributions
        bernoulli, uniformdiscrete, categorical, poisson, poissonPdf,
        -- * Streams
        iid)
      where

import LazyPPL (Prob,uniform)
import Data.List (findIndex)
import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants

{-|
  [Normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
-}
normal :: Double -- ^ mu, mean
        -> Double -- ^ sigma, standard deviation
        -> Prob Double
normal m s = do
  x <- uniform
  return $ (- invErfc (2 * x)) * (m_sqrt_2 * s) + m
normalPdf :: Double -> Double -> Double -> Double
normalPdf m s x = exp (- ((x - m) * (x - m) / (2 * s * s)) - log (m_sqrt_2_pi * s))


{-|
  [Exponential distribution](https://en.wikipedia.org/wiki/Exponential_distribution)
-}
exponential :: Double -- ^ lambda, rate
            -> Prob Double
exponential rate = do
  x <- uniform
  return $ - (log x / rate)
expPdf :: Double -> Double -> Double
expPdf rate x = exp (- (rate * x)) * rate

{-|
  [Gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
-}
gamma :: Double -- ^ k, shape
      -> Double -- ^ theta, scale
      -> Prob Double
gamma a b = do
  x <- uniform
  return $ b * invIncompleteGamma a x

{-|
  [Beta distribution](https://en.wikipedia.org/wiki/Beta_distribution)
-}
beta :: Double -- ^ alpha
      -> Double -- ^ beta
      -> Prob Double
beta a b = do
  x <- uniform
  return $ invIncompleteBeta a b x

{-|
  [Poisson distribution](https://en.wikipedia.org/wiki/Poisson_distribution)
-}
poisson :: Double -- ^ lambda, rate
        -> Prob Integer
poisson lambda = do
  x <- uniform
  let cmf = map (\x -> 1 - incompleteGamma (fromIntegral (x + 1)) lambda) [0,1..]
  let (Just n) = findIndex (> x) cmf
  return $ fromIntegral n

poissonPdf :: Double -> Integer -> Double
poissonPdf rate n = let result = exp(-rate) * rate ^^ fromIntegral n / factorial (fromIntegral n) in
  if isInfinite result || isNaN result then exp (-rate + fromIntegral n * log rate - logGamma (fromIntegral (n+1))) else result


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
