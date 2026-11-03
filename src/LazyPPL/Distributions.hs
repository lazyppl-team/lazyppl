{- | Handy distributions for the `LazyPPL` library, based on the `uniform` distribution.

Sometimes both a distribution (type @Prob a@) and pdf (type @a -> Double@) are given. Distributions are useful for sampling, densities are used for scoring. 

For more distributions, see the Statistics.Distribution in the statistics package. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE BangPatterns #-}


module LazyPPL.Distributions (
        -- * Continuous distributions
        normal,normalPdf,normalLogPdf,exponential,expPdf,exponentialLogPdf,
        gamma, gammaLogPdf, beta, BetaLabelError(..), betaLogPdf, dirichlet, dirichletLogPdf,
        uniformbounded, uniformBoundedLogPdf,
        logNormalLogPdf,
        -- * Discrete distributions
        bernoulli, bernoulliLogPmf, uniformdiscrete, categorical, categoricalLogPmf,
        poisson, poissonPdf, poissonLogPmf,
        binomialLogPmf,
        -- * Streams
        iid)
      where

import LazyPPL (Prob,uniform)
import Control.Exception (Exception, throw)
import Data.Bits (bit, shift)
import Data.List (findIndex, zipWith5)
import qualified Numeric.Sum as Sum
import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants
import Numeric.RootFinding (Root(..), RiddersParam(..), Tolerance(..), ridders)

{-|
  [Normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
-}
normal :: Double -- ^ mu, mean
        -> Double -- ^ sigma, standard deviation
        -> Prob Double
normal m s
  | not (finite m && positiveFinite s) = error "normal: the mean must be finite and the standard deviation finite and positive"
  | otherwise = do
    x <- uniform
    -- The standard normal quantile is finite on the open interval; scaling it
    -- last avoids overflowing m_sqrt_2 * s.
    return $ m + s * (- invErfc (2 * x) * m_sqrt_2)
normalPdf :: Double -> Double -> Double -> Double
normalPdf m s x = exp (normalLogPdf m s x)


{-|
  [Exponential distribution](https://en.wikipedia.org/wiki/Exponential_distribution)
-}
exponential :: Double -- ^ lambda, rate
            -> Prob Double
exponential rate
  | not (positiveFinite rate) = error "exponential: the rate must be finite and positive"
  | otherwise = do
    x <- uniform
    return $ - (log x / rate)
expPdf :: Double -> Double -> Double
expPdf rate x = exp (exponentialLogPdf rate x)

{-|
  [Gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
-}
gamma :: Double -- ^ k, shape
      -> Double -- ^ theta, scale
      -> Prob Double
gamma a b
  | not (positiveFinite a && positiveFinite b) = error "gamma: the shape and scale must be finite and positive"
  | otherwise = do
    x <- uniform
    return $ b * invIncompleteGamma a x

{-|
  [Beta distribution](https://en.wikipedia.org/wiki/Beta_distribution)

  Each parameter must be at least 0.001, and their sum at most 2000, unless one
  of them is 1: Beta(a,1) and Beta(1,b) take every finite positive a and b.
  Other parameters raise an error. For other shapes, custom tree labels below
  @2^-53@ raise 'BetaLabelBelowGeneratorRange'; the default generator's
  smallest label is @2^-53@. The analytic one-shape cases accept all labels in
  @(0,1)@. On the supported rows of the reference grid, the drawn u-quantile has a distribution
  function within @1e-9 * min u (1 - u)@ of u, or is one of the two doubles
  next to the exact quantile.
-}
beta :: Double -- ^ alpha
      -> Double -- ^ beta
      -> Prob Double
beta a b
  | not (positiveFinite a && positiveFinite b && (betaSupported [a, b] || a == 1 || b == 1)) =
      error ("beta: the parameters must each be at least " ++ show betaLeast ++ " and sum to at most " ++ show betaSumMost
             ++ ", or be 1 and a finite positive number, got " ++ show (a, b))
  | otherwise = do
    x <- uniform
    return $ fst (betaQuantile a b x)

-- | A custom tree label lies below the supported range of the general Beta
-- sampler. The default random-tree generator never produces such a label.
data BetaLabelError = BetaLabelBelowGeneratorRange Double deriving (Eq, Show)
instance Exception BetaLabelError

{-|
  [Poisson distribution](https://en.wikipedia.org/wiki/Poisson_distribution)
-}
poisson :: Double -- ^ lambda, rate
        -> Prob Integer
poisson lambda
  | not (lambda >= 0 && finite lambda) = error "poisson: the rate must be finite and non-negative"
  | otherwise = do
    x <- uniform
    -- Compare the smaller tail directly, so subtracting it from 1 cannot erase it.
    let tails = map (\n -> incompleteGamma (fromIntegral (n + 1)) lambda) [0,1..]
        cumulative = map Sum.kbn $ drop 1 $ scanl Sum.add Sum.zero (map (exp . poissonLogPmf lambda) [0..])
        (Just n) = if x >= 0.5 then findIndex (< 1 - x) tails else findIndex (> x) cumulative
    return $ fromIntegral n

poissonPdf :: Double -> Integer -> Double
poissonPdf rate n = exp (poissonLogPmf rate n)


{-|
  [Dirichlet distribution](https://en.wikipedia.org/wiki/Dirichlet_distribution)

  Drawn by stick breaking: the first coordinate is Beta(a1, a2 + ... + ak), each
  later coordinate takes a Beta(ai, a(i+1) + ... + ak) share of the stick that
  remains, and the last coordinate is the rest. This is the Dirichlet law, and no
  coordinate is NaN even when the concentrations are so small that Gamma draws
  underflow to 0.

  Each concentration must be at least 0.001, and their sum at most 2000; other
  concentrations raise an error. Each share is a quantile of its Beta law, with
  the accuracy of 'beta'.
-}
dirichlet :: [Double] -- ^ vector of alphas; length is dimension
          -> Prob[Double]
dirichlet as
  | not (betaSupported as) = error ("dirichlet: the concentrations must each be at least " ++ show betaLeast
                                    ++ " and sum to at most " ++ show betaSumMost ++ ", got " ++ show as)
  | otherwise = breakStick 1 as (drop 1 (scanr (+) 0 as))
  where
    breakStick rest [_] _ = return [rest]
    breakStick rest (a : rest') (later : laters) = do
      u <- uniform
      let (v, w) = betaQuantile a later u
      xs <- breakStick (rest * w) rest' laters
      return (rest * v : xs)
    breakStick _ _ _ = return []

-- | The supported parameters of dirichlet, and of beta apart from Beta(a,1) and
-- Beta(1,b) (see betaQuantile): each at least betaLeast, with a sum of at most
-- betaSumMost. Every stick of dirichlet as is then a Beta law whose two
-- parameters lie in the domain, as their sum is a suffix sum of as.
betaSupported :: [Double] -> Bool
betaSupported as = all (>= betaLeast) as && sum as <= betaSumMost

-- The bounds are measured. test/reference/reference.py writes, with mpmath, the
-- rows of test/reference/beta-quantiles.tsv: Beta(a,b) at a label u, with the
-- interval of doubles that a supported draw may take (see betaQuantile), or a
-- rejection marker when both samplers reject the input. It covers 23
-- parameters from 1e-10 to 1e10 at 13 labels from 1e-300 to 1 - 2^-53, and for
-- the corners of the bounds, the doubles just outside them and random pairs
-- inside. On 1 Nov 2026 (math-functions 0.3.4.4, GHC 9.14.1), before the label
-- restriction and with the former first-order intervals, the bounds removed,
-- and each row run in a process killed after 2 s, beta a b and dirichlet [a,b]
-- landed in the intervals at all 2057 rows inside the shape bounds,
-- the slowest in 0.35 s (Beta(1999.999,0.001) at 1e-300). Outside, rows landed
-- outside the intervals where both parameters exceed 3000, where math-functions
-- computes I by quadrature (Beta(1e4,1e4) at 1e-300), and ran past 2 s with a
-- parameter of 1e-5 or below (Beta(1000,1e-5) at 2^-53). Draws with a parameter
-- near 0.001 slow down as the sum grows: Beta(1e4,0.001) took 1.6 s at 1e-20.
betaLeast, betaSumMost :: Double
betaLeast = 1e-3
betaSumMost = 2000

-- | The u-quantile x of Beta(a,b), with 1 - x, for parameters in the supported
-- domain. The quantile is a root of the regularized incomplete Beta function I
-- of math-functions: its inverse, invIncompleteBeta (0.3.4.4), starts from
-- guesses that fail outside a few regimes (its issues #29, #44 and #47), and
-- returns 0 for Beta(1, 0.001) at 1/4, whose quantile is 1 - 0.75^1000. Of x
-- and 1 - x, the one at most 1/2 is solved for, through
-- I_x(a,b) = 1 - I_(1-x)(b,a), so that each keeps its relative precision even
-- when the other rounds to 1. Both probabilities u and 1 - u are passed on:
-- 1 - u is exact when u >= 1/2, and u when it is the smaller.
betaQuantile :: Double -> Double -> Double -> (Double, Double)
betaQuantile a b u
  -- Beta(a,1) has the distribution function x^a and Beta(1,b) has 1 - (1-x)^b,
  -- so their u-quantiles are u^(1/a) and 1 - (1-u)^(1/b). With t = log u / a or
  -- t = log1p (-u) / b, exp t and -expm1 t retain the smaller share without
  -- subtracting it from 1. If division overflows to negative infinity, that
  -- smaller share rounds to 0.
  | b == 1 = let t = log u / a in (exp t, - expm1 t)
  | a == 1 = let t = log1p (- u) / b in (- expm1 t, exp t)
  | u < encodeFloat 1 (-53) = throw (BetaLabelBelowGeneratorRange u)
  | u <= incompleteBeta a b 0.5 = let x = smallQuantile a b u (1 - u) in (x, 1 - x)
  | otherwise = let y = smallQuantile b a (1 - u) u in (1 - y, y)

-- | The v-quantile z <= 1/2 of Beta(p,q), given I_(1/2)(p,q) >= v and w = 1 - v:
-- the root of a residual in t = log z, between the least positive double and
-- 1/2, by math-functions' Ridders method, which keeps the bracket. Solving for
-- log z keeps full relative precision for quantiles far below 1, such as
-- 0.75^1000. The residual compares the logarithms of the smaller of the two
-- probabilities: for w at least 2^-19, log I_z(p,q) with log v; below, the
-- upper tail 1 - I_z(p,q) = I_(1-z)(q,p) with log w. Near 1, I_z(p,q) is a
-- double within 2^-53 of 1 - w, so comparing it with v would leave the root
-- anywhere on a plateau of relative width 2^-53 / w in w, at most 2^-34 for w
-- at least 2^-19. Where a probability underflows, its logarithm is held at that
-- of the least positive double, so the residual stays finite and keeps its
-- sign; a probability that is not a number in [0,1] raises an error. A quantile
-- below the least positive double is 0, the nearer double or the one below it.
-- The search stops within one ulp of t (AbsTol 0) and may take 200 steps,
-- twice math-functions' default, because the bracket spans the whole double
-- range on the log scale; a search that does not converge raises an error
-- rather than returning a guess.
smallQuantile :: Double -> Double -> Double -> Double -> Double
smallQuantile p q v w
  | f hi <= 0 = 0.5
  | f lo > 0 = 0
  | otherwise = case ridders RiddersParam { riddersMaxIter = 200, riddersTol = AbsTol 0 } (lo, hi) f of
      Root t -> exp t
      _ -> error ("beta: no quantile found for parameters " ++ show (p, q) ++ " and probability " ++ show v)
  where
    f t | w < encodeFloat 1 (-19) = logProbability w - logProbability (incompleteBeta q p (1 - exp t))
        | otherwise = logProbability (incompleteBeta p q (exp t)) - logProbability v
    logProbability c
      | c >= 0 && c <= 1 = log (max leastPositive c)
      | otherwise = error ("beta: the incomplete Beta function gave " ++ show c ++ " for parameters " ++ show (p, q))
    leastPositive = encodeFloat 1 (-1074)
    lo = log leastPositive
    hi = log 0.5

-- | [Continuous uniform distribution on a bounded interval](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
uniformbounded :: Double -- ^ lower
                -> Double -- ^ upper
                -> Prob Double
uniformbounded lower upper
  | not (finite lower && finite upper && lower < upper) = error "uniformbounded: the bounds must be finite, with lower < upper"
  | otherwise = do
    x <- uniform
    -- The width overflows for finite bounds of opposite signs near the largest
    -- double; the weighted sum of the bounds does not (random 1.3.1's
    -- scaleFloating handles an infinite width the same way).
    return $ if isInfinite (upper - lower) then x * upper + (1 - x) * lower else (upper - lower) * x + lower

-- | [Bernoulli distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
bernoulli :: Double -- ^ bias
          -> Prob Bool
bernoulli r
  | not (probability r) = error "bernoulli: the probability must be in [0,1]"
  | otherwise = do
    x <- uniform
    return $ x < r

{-|
  [Discrete uniform distribution](https://en.wikipedia.org/wiki/Discrete_uniform_distribution) on [0, ..., n-1]
-}
uniformdiscrete :: Int -- ^ n
                -> Prob Int
uniformdiscrete n
  | n < 1 = error "uniformdiscrete: n must be at least 1"
  | otherwise = do
      let upper = fromIntegral n
      r <- uniformbounded 0 upper
      return $ floor r

{-| [Categorical distribution](https://www.google.com/search?client=safari&rls=en&q=categorical+distribution&ie=UTF-8&oe=UTF-8): Takes a list of k numbers that sum to 1, 
    and returns a random number between 0 and (k-1), weighted accordingly.
    Only the probabilities up to the chosen index are read, so the list may be infinite, and each one read must be in [0,1]. -}
categorical :: [Double] -> Prob Int
categorical xs = do
  r <- uniform
  return $! pickCategorical r xs

pickCategorical :: Double -> [Double] -> Int
pickCategorical r = pick 0 Sum.zero 0 Nothing
  where
      -- The upper bounds below are added as exact integers, in units of
      -- 2^-1075 (half the smallest positive Double). For example, 0.5 is
      -- 2^1074 units and the smallest positive Double is 2 units. Every
      -- bound from upperWeight is a whole number of these units, so their
      -- sum never rounds. Probability 1 is 2^1075 units. The probabilities
      -- themselves are added as Doubles (a compensated sum), which can
      -- round, as in the example further down.
      limit = bit 1075 :: Integer
      -- The largest exact probability that rounds to the Double q:
      -- q plus half the gap to the next Double, in the same units.
      upperWeight q
        | q == 0 = 1
        | otherwise = let (mantissa, exponent) = decodeFloat q
                      in shift mantissa (exponent + 1075) + bit (max 0 (exponent + 1074))
      pick !i !total !upper !previous (q:qs)
        | not (probability q) = error "categorical: probabilities must be in [0,1]"
        | q > 0 && Sum.kbn next > r = i
        | otherwise = pick (i + 1) next (min limit (upper + upperWeight q))
          -- Capped at 1, so an infinite list cannot grow it.
                           (if q > 0 then Just i else previous) qs
        where next = Sum.add total q
      -- The list ended before the running sum passed r. Valid input can do
      -- this. For example, with 49 weights of 1/49:
      --   stored Double:  0.02040816326530612  (just below 1/49)
      --   sum of all 49:  0.9999999999999999   (not 1)
      --   draw r:         0.9999999999999999   (the largest draw below 1)
      -- No running sum passes r. Each true weight is at most upperWeight of
      -- its Double, so if the true weights sum to 1, the upper bounds sum to
      -- at least 1. If they do, the gap between the sum and 1 (1.1e-16 in
      -- the example) comes from rounding: return the last index with
      -- positive weight (index 48 in the example). If not, the weights sum
      -- to less than 1 by more than rounding can explain.
      pick _ _ !upper (Just i) []
        | upper >= limit = i
        | otherwise = error "categorical: probabilities do not sum to 1"
      pick _ _ _ Nothing [] = error "categorical: probabilities do not sum to 1"

{-| Returns an infinite stream of samples from the given distribution. --}
iid :: Prob a -> Prob [a]
iid p = do r <- p; rs <- iid p; return $ r : rs

normalLogPdf :: Double -> Double -> Double -> Double
normalLogPdf mu sigma x
  | not (finite mu && positiveFinite sigma) = error "normalLogPdf: the mean must be finite and the standard deviation finite and positive"
  | otherwise = -0.5 * log (2 * pi) - log sigma - (0.5 * z) * z
  where
    d = x - mu
    -- Standardise before squaring, so (x - mu)^2 and sigma^2 cannot overflow or
    -- underflow on their own. x - mu overflows only for huge x and mu of opposite
    -- signs, and then x/sigma - mu/sigma loses nothing.
    z | isInfinite d && not (isInfinite x) = x / sigma - mu / sigma
      | otherwise = d / sigma

exponentialLogPdf :: Double -> Double -> Double
exponentialLogPdf rate x
  | rate <= 0 || isNaN rate || isInfinite rate = error "exponentialLogPdf: rate must be finite and positive"
  | x < 0    = m_neg_inf
  | otherwise = log rate - rate * x

gammaLogPdf :: Double -> Double -> Double -> Double
gammaLogPdf k theta x
  | not (positiveFinite k && positiveFinite theta) = error "gammaLogPdf: the shape and scale must be finite and positive"
  | x <= 0 || isInfinite x = m_neg_inf
  | isInfinite lambda && isInfinite relativeRate = m_neg_inf
  | isInfinite lambda = k * log1pmx relativeRate - stirlingError k
      - m_ln_sqrt_2_pi - 0.5 * log k + log k - log x
  -- The density is the Poisson mass of lambda = x/theta at k - 1, divided by
  -- theta (at k when k < 1, times k/x), so the Poisson kernel applies.
  | lambda == 0 = (k - 1) * log x - k * log theta - logGamma k
  | k == 1 = - lambda - log theta
  | k > 1 = poissonKernel (k - 1) lambda (nearResidual k (lambda - (k - 1)) (rate - (toRational k - 1))) - log theta
  | otherwise = poissonKernel k lambda (nearResidual k (lambda - k) (rate - toRational k)) + log k - log x
  where
    lambda = x / theta
    rate = toRational x / toRational theta
    relativeRate = fromRational ((rate - toRational k) / toRational k)

-- | Log density of the Beta distribution, or negative infinity outside (0,1).
-- Parameters that are not positive, or whose sum is not finite, raise an error.
betaLogPdf :: Double -> Double -> Double -> Double
betaLogPdf a b x
  | not (positiveFinite a && positiveFinite b && positiveFinite (a + b)) = error "betaLogPdf: both parameters must be positive, with a finite sum"
  | x <= 0 || x >= 1 = m_neg_inf
  | otherwise = betaKernel a b x (1 - x) (log x) (log1p (-x)) (toRational x)

bernoulliLogPmf :: Double -> Bool -> Double
bernoulliLogPmf p outcome
  | isNaN p || p < 0 || p > 1 = error "bernoulliLogPmf: probability must be in [0,1]"
  | outcome = log p
  | otherwise = log1p (-p)

-- | Log-probability of the k-th category. Only the probabilities at indices
-- 0..k are inspected, so an infinite probability stream, as the Dirichlet
-- process supplies, is answered after reading its needed prefix.
categoricalLogPmf :: [Double] -> Int -> Double
categoricalLogPmf probs k
  | k < 0 = m_neg_inf
  | otherwise = case drop k probs of
      [] -> m_neg_inf
      (p:_)
        | any (\q -> isNaN q || q < 0 || q > 1) (take (k + 1) probs) ->
            error "categoricalLogPmf: probabilities must be in [0,1]"
        | otherwise -> log p

uniformBoundedLogPdf :: Double -> Double -> Double -> Double
uniformBoundedLogPdf lower upper x
  | not (finite lower && finite upper && lower < upper) = error "uniformBoundedLogPdf: the bounds must be finite, with lower < upper"
  | x < lower || x > upper = m_neg_inf
  | isInfinite (upper - lower) = -(log (upper / 2 - lower / 2) + log 2)
  | otherwise = -log (upper - lower)

-- | Log density of the Dirichlet distribution, or negative infinity outside
-- the open simplex. The coordinates must sum to 1 within
-- gamma_n = n*u/(1-n*u), with n the dimension and u = 2^-53, which bounds the
-- rounding error of the coordinates and their sum. Concentrations that are not
-- positive, or whose sum is not finite, raise an error.
dirichletLogPdf :: [Double] -> [Double] -> Double
dirichletLogPdf alphas xs
  | null xs || length alphas /= length xs = m_neg_inf
  | not (all positiveFinite alphas && positiveFinite (sum alphas)) = error "dirichletLogPdf: concentrations must be positive, with a finite sum"
  | any (\x -> x <= 0 || isNaN x || isInfinite x) xs = m_neg_inf
  | abs (sum xs - 1) > roundingBound = m_neg_inf
  | otherwise = sum (zipWith5 factor alphas laters xs rests (drop 1 rests))
    where
      nu = fromIntegral (length xs) * encodeFloat 1 (negate (floatDigits (0 :: Double)))
      roundingBound = nu / (1 - nu)
      -- With r_i = x_i + ... + x_k, the share x_i/r_i of the remaining stick is
      -- Beta(a_i, a_(i+1) + ... + a_k), independently (the aggregation property),
      -- so the density is the product of those Beta densities over the Jacobian
      -- r_i of x_i = r_i * (x_i/r_i). Suffix sums form 1 - x_i/r_i without cancelling.
      rests = scanr1 (+) xs
      laters = drop 1 (scanr (+) 0 alphas)
      factor a later x r next =
        betaKernel a later (x / r) (next / r) (log x - log r) (log next - log r) (toRational x / toRational r) - log r

logNormalLogPdf :: Double -> Double -> Double -> Double
logNormalLogPdf mu sigma x
  | not (finite mu && positiveFinite sigma) = error "logNormalLogPdf: the mean must be finite and the standard deviation finite and positive"
  | x <= 0   = m_neg_inf
  | otherwise = normalLogPdf mu sigma (log x) - log x

binomialLogPmf :: Int -> Double -> Int -> Double
binomialLogPmf n p k
  | n < 0 || isNaN p || p < 0 || p > 1 = error "binomialLogPmf: n must be non-negative and probability in [0,1]"
  | k < 0 || k > n  = m_neg_inf
  | p == 0 = if k == 0 then 0 else m_neg_inf
  | p == 1 = if k == n then 0 else m_neg_inf
  | k == 0 = fromIntegral n * log1p (-p)
  | k == n = fromIntegral n * log p
  | otherwise = binomialKernel (fromIntegral k) (fromIntegral (n - k)) p (1 - p)
      (nearResidual (fromIntegral n) (fromIntegral n * p - fromIntegral k) (fromIntegral n * toRational p - fromIntegral k))

poissonLogPmf :: Double -> Integer -> Double
poissonLogPmf lambda k
  | lambda < 0 || isNaN lambda || isInfinite lambda = error "poissonLogPmf: rate must be finite and non-negative"
  | k < 0    = m_neg_inf
  -- At rate 0 the formula below gives NaN for k = 0 (0 * log 0), as does
  -- statistics-0.16.5.0.
  | lambda == 0 = if k == 0 then 0 else m_neg_inf
  | k == 0 = - lambda
  | isInfinite count = largeCountPoisson lambda k
  | otherwise = poissonKernel (fromIntegral k) lambda (countResidual lambda k)
  where count = fromInteger k :: Double

-- For a count above Double's finite range, keep one quarter of it in range.
-- The exact Integer residual retains central counts that round to the rate.
largeCountPoisson :: Double -> Integer -> Double
largeCountPoisson lambda k
  | isInfinite quarter = m_neg_inf
  | relative > -0.5 = quarter * (4 * log1pmx relative) - correction
  | otherwise = - quarter * (4 * (logRatio - 1) + lambda / quarter) - correction
  where
    quarter = fromInteger (k `div` 4)
    relative = fromRational ((toRational lambda - fromInteger k) / fromInteger k)
    logCount = log quarter + log 4
    ratio = (lambda / quarter) / 4
    logRatio | ratio >= m_tiny = - log ratio
             | otherwise = logCount - log lambda
    correction = stirlingError quarter / 4 + m_ln_sqrt_2_pi + 0.5 * logCount

-- | lambda - k, rounded once. A count above 2^53 can differ from the nearest
-- double by more than the spread of the distribution (10^32 and the double 1e32
-- differ by 5.4e15, half a standard deviation), so its difference from lambda is
-- formed exactly.
countResidual :: Double -> Integer -> Double
countResidual lambda k
  | k < 2 ^ (53 :: Int) = lambda - fromInteger k
  | otherwise = fromRational (toRational lambda - fromInteger k)

-- Loader's saddle-point forms (C. Loader, "Fast and accurate computation of
-- binomial probabilities", 2000). log(n!) is the Stirling formula plus
-- stirlingError n (from math-functions), and each power is a deviance bd0, so
-- no two large terms cancel, as they did in sums of logGamma values. Logarithms
-- of products are taken as sums of logarithms, so no product leaves the range.

-- | The logarithm of lambda^m e^(-lambda) / Gamma(m+1), for real m > 0 and
-- lambda > 0, given lambda - m as d.
poissonKernel :: Double -> Double -> Double -> Double
poissonKernel m lambda d = - stirlingError m - bd0 m lambda d - m_ln_sqrt_2_pi - 0.5 * log m

-- | The logarithm of Gamma(m+l+1) / (Gamma(m+1) Gamma(l+1)) p^m q^l, for real
-- m, l > 0 and q = 1 - p given separately, given np - m as d; nq - l is -d.
binomialKernel :: Double -> Double -> Double -> Double -> Double -> Double
binomialKernel m l p q d =
  stirlingError n - stirlingError m - stirlingError l
    - bd0 m (n * p) d - bd0 l (n * q) (- d)
    + 0.5 * (log n - log m - log l) - m_ln_sqrt_2_pi
  where n = m + l

-- | The residual np - x of bd0 near the centre, rounded once, for a count or
-- shape `size`. Rounding np, a product or quotient of the inputs, by a
-- relative 2^-53 moves the deviance by at most |np - x| 2^-53, which stays
-- below 1e-12 of the log density while size is below 2^25: so below, the
-- residual comes from Double arithmetic (`fast`), and above, from the exact
-- rational value of the same expression. At gammaLogPdf 1e300 1e-300 1, the
-- rounded x / theta is one ulp, 1.5e284, from 1e300, and the deviance 1.1e268
-- that it gives is 3.7 times the true 3.0e267.
nearResidual :: Double -> Double -> Rational -> Double
nearResidual size fast exact
  | size < 33554432 = fast
  | otherwise = fromRational exact

-- | The deviance x log(x/np) + np - x, for positive finite x and np, given
-- d = np - x. With r = np/x it is -x (log r - (r - 1)) = -x log1pmx (r - 1).
-- For r in [1/2, 2], r - 1 = d/x, and math-functions' log1pmx is accurate near
-- 0, where the deviance is small; between doubles, np - x is exact there. Elsewhere
-- |log(x/np)| >= log 2 and the deviance is at least 0.15 max(x, np): it is
-- x (log(x/np) - 1) + np, whose two terms sum in magnitude to at most 13.4
-- times its value, and neither exceeds it in magnitude by more than np, so
-- neither overflows where the deviance does not (x log(x/np) itself is 2.1e308
-- at x = 1e308, np = 1.25e307, where the deviance is 1.2e308). It takes
-- log x - log np when x/np is not a normal double. math-functions' own bd0
-- (0.3.4.4) forms x/np and x + np: it gives Infinity at x = 1, np = 1e-320, and
-- at x = np = 1e308 its series starts from Infinity * 0 and never ends.
bd0 :: Double -> Double -> Double -> Double
bd0 x np d
  | x / 2 <= np && np / 2 <= x = - x * log1pmx (d / x)
  | otherwise = x * (logRatio - 1) + np
  where
    ratio = x / np
    logRatio | ratio >= m_tiny && ratio <= m_huge = log ratio
             | otherwise = log x - log np

-- | The log density of Beta(a,b) at p, with q = 1 - p and both logarithms given,
-- so that none of the four is formed by cancellation, and p also exactly.
betaKernel :: Double -> Double -> Double -> Double -> Double -> Double -> Rational -> Double
betaKernel a b p q logP logQ exactP
  -- (a + b - 1) times the binomial mass of a - 1 successes in a + b - 2 trials.
  -- With both counts at least 1 the kernel's own Stirling terms do not cancel.
  | a >= 2 && b >= 2 = log (a + b - 1) + binomialKernel (a - 1) (b - 1) p q
      (nearResidual (a + b) ((a + b - 2) * p - (a - 1)) ((toRational a + toRational b - 2) * exactP - (toRational a - 1)))
  -- Below 2 the direct form has no large terms to cancel; logBeta corrects a
  -- large argument with Stirling terms.
  | otherwise = (a - 1) * logP + (b - 1) * logQ - logBeta a b

-- Parameter domains. A comparison with NaN is false, so each test rejects NaN.
positiveFinite :: Double -> Bool
positiveFinite x = x > 0 && x < m_pos_inf

finite :: Double -> Bool
finite x = x > m_neg_inf && x < m_pos_inf

probability :: Double -> Bool
probability p = p >= 0 && p <= 1
