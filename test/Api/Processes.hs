{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- The Wiener and memo checks need a fresh hidden table per call, which floating
-- a constant random function out of its lambda would share.
module Api.Processes where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import Numeric.SpecFunctions (erfc)
import LazyPPL.Distributions
import LazyPPL.Distributions.GP (gp, wiener)
import LazyPPL.Distributions.IBP (ibp)
import LazyPPL.Distributions.Memoization (memoize, memrec)
import qualified LazyPPL.Distributions.DirichletP as DP
import qualified LazyPPL.Distributions.Mondrian as Mondrian
import System.Random (RandomGen(..), SplitGen(..), mkStdGen, setStdGen)
import Test.QuickCheck (NonEmptyList(..), NonNegative(..), Positive(..), Property, ioProperty, isSuccess, quickCheckResult)

import Api.Support

memoizeMismatches :: Double
memoizeMismatches =
  let g = runProb (memoize (\n -> pure (n :: Int))) (randomTree (mkStdGen 1))
  in fromIntegral (length (filter (\n -> g n /= n) [0 .. 40]))

standardDeviation :: [Double] -> Double
standardDeviation xs =
  let n = fromIntegral (length xs)
      m = sum xs / n
  in sqrt (sum [(x - m) ^ (2 :: Int) | x <- xs] / (n - 1))

-- Since cov 0 0 = 1, f 0 has standard deviation 1 up to the nugget.
gpStandardDeviationAtZero :: Double
gpStandardDeviationAtZero =
  standardDeviation [runProb (gp (\x y -> exp (negate ((x - y) ^ (2 :: Int))))) (randomTree (mkStdGen s)) 0 | s <- [1 .. 400]]

-- With a constant kernel c, f 1 - f 0 is only nugget noise, of variance 2*c*2^-33.
gpConstantKernelGap :: Double -> IO Double
gpConstantKernelGap c = do
  let f = runProb (gp (\_ _ -> c)) (randomTree (mkStdGen 3))
  r <- try (evaluate (f 0 `seq` abs (f 1 - f 0) / sqrt c)) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) id r)

-- Scaling the kernel by s scales the draw by sqrt s, so even a huge s gives a finite draw.
gpScaledKernelFinite :: Double -> IO Double
gpScaledKernelFinite s = do
  let f = runProb (gp (\x y -> s * exp (negate ((x - y) ^ (2 :: Int))))) (randomTree (mkStdGen 4))
  r <- try (evaluate (sum [abs (f x) | x <- [0, 0.1 .. 3]] / sqrt s)) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) (\v -> if isNaN v || isInfinite v then 1 / 0 else 0) r)

-- The rank-two kernel gives a line plus nugget noise. At x = 5 the residual has nugget
-- variance 92*c*2^-33, the 92 in the bound.
gpRankTwoLineResidual :: Double -> IO Double
gpRankTwoLineResidual c = do
  let f = runProb (gp (\x y -> c * (1 + x * y))) (randomTree (mkStdGen 4))
      line x = f 0 + x * (f 1 - f 0)
  r <- try (evaluate (maximum [abs (f x - line x) | x <- [0 .. 5]] / sqrt c)) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) id r)

-- c cos (x - y) has rank two as well, and its draw must still be finite.
gpCosineKernelFinite :: Double -> IO Double
gpCosineKernelFinite c = do
  let f = runProb (gp (\x y -> c * cos (x - y))) (randomTree (mkStdGen 5))
  r <- try (evaluate (sum [abs (f x) | x <- [0 .. 5]] / sqrt c)) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) (\v -> if isNaN v || isInfinite v then 1 / 0 else 0) r)

-- On the same random tree, gp (s * k) is sqrt s times gp k, nugget included.
gpScaleEquivarianceError :: IO Double
gpScaleEquivarianceError = do
  let kernels = [\_ _ -> 1, \x y -> 1 + x * y, \x y -> cos (x - y), \x y -> exp (negate ((x - y) ^ (2 :: Int)))]
      draw s k = runProb (gp (\x y -> s * k x y)) (randomTree (mkStdGen 6))
      gap s k = let f1 = draw 1 k; fs = draw s k
                in maximum [abs (fs x / sqrt s - f1 x) / (1 + abs (f1 x)) | x <- [0 .. 5]]
  r <- try (evaluate (maximum [gap s k | k <- kernels,
                               s <- [1e-300,1e-100,1e8,1e16,1e100,1e300]])) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) id r)

-- Even a variance of 2^-1074, the smallest positive double, gives the first point a draw, as
-- there is no absolute cutoff. Later pivots may underflow.
gpSmallestVarianceSample :: IO Double
gpSmallestVarianceSample = do
  let s = encodeFloat 1 (-1074)
      f = runProb (gp (\_ _ -> s)) (randomTree (mkStdGen 4))
  r <- try (evaluate (abs (f 0) / sqrt s)) :: IO (Either SomeException Double)
  pure (either (const (1 / 0)) id r)

-- The first customer takes Poisson(1) dishes, so the first row is empty with probability exp(-1).
ibpFirstRowEmptyFraction :: Double
ibpFirstRowEmptyFraction =
  let firstRowEmpty s = case runProb (ibp 1) (randomTree (mkStdGen s)) of
        row : _ -> null row
        [] -> error "ibp returned no rows"
  in fromIntegral (length (filter firstRowEmpty [1 .. 2000 :: Int])) / 2000

smokeGpFinite :: IO Bool
smokeGpFinite = do
  let cov x y = exp (negate ((x - y) * (x - y)))
      f = runProb (gp cov) (randomTree (mkStdGen 12345))
      ys = [f 0, f 0.25, f 1.0]
  pure $ all (\y -> not (isNaN y) && not (isInfinite y)) ys

memoStochasticChecks :: IO [Bool]
memoStochasticChecks = do
  result <- quickCheckResult $ \seed (NonNegative key) -> ioProperty $ do
    let f = runProb (memoize (const uniform :: Int -> Prob Double)) (randomTree (mkStdGen seed))
        otherKey = if key == maxBound then 0 else key + 1
    values <- mapM (evaluate . f) [key, otherKey, key]
    pure $ case values of
      [first, other, repeated] -> first == repeated && first /= other
      _ -> False
  sequence [assertBool "memoize reuses sampled values and distinguishes keys" (isSuccess result)]

newtype OrdOnlyKey = OrdOnlyKey Int deriving (Eq, Ord)

recursiveMemoCheck :: IO [Bool]
recursiveMemoCheck = do
  let f = runProb (memrec (\self (OrdOnlyKey n) ->
        pure (if n == 0 then 1 else self (OrdOnlyKey (n-1)) + 1))) (randomTree (mkStdGen 1))
  sequence [assertBool "memrec accepts keys with Ord and no Show" (f (OrdOnlyKey 3) == (4 :: Int))]

dirichletProcessChecks :: IO [Bool]
dirichletProcessChecks = do
  result <- quickCheckResult $ \seed ->
    let tree = randomTree (mkStdGen seed)
        constantDraw = do distribution <- DP.dp 1 (pure (7 :: Int)); distribution
    in runProb constantDraw tree == 7 && runProb (categorical (0:1:undefined)) tree == 1
  sequence [assertBool "Dirichlet process preserves constant atoms and lazy categorical selection" (isSuccess result)]

-- x = 1/2 itself goes to the right child. The blocks have bias 0 and 1, so a wrong traversal
-- or likelihood shows.
mondrianChecks :: IO [Bool]
mondrianChecks = do
  let box = [(0,1),(0,1)]
      tree = Mondrian.Partition 0 0.5 box
        (Mondrian.Block 0 [(0,0.5),(0,1)]) (Mondrian.Block 1 [(0.5,1),(0,1)])
      draw seed p = runProb p (randomTree (mkStdGen seed))
      expected = [(0.25,False),(0.5,True),(0.75,True)]
      halfTree = let t = Tree 0.5 (repeat t) in t
      -- With every label 1/2, the root (cost log(2)/2 < 0.4) is cut along dimension 1 at 1/2,
      -- and each child (cost log(2)/1.5) exceeds the budget left.
      expectedPrior = Mondrian.Partition 1 0.5 box
        (Mondrian.Block 0.25 [(0,1),(0,0.5)]) (Mondrian.Block 0.25 [(0,1),(0.5,1)])
  result <- quickCheckResult $ \seed -> and
        [ Mondrian.likelihoodFromMondrian2D tree x y value == 1 &&
          Mondrian.likelihoodFromMondrian2D tree x y (not value) == 0 &&
          draw seed (Mondrian.sampleFromMondrian2D tree x y) == value
        | (x,value) <- expected, y <- [0,1] ]
  prior <- quickCheckResult $ \seed ->
    draw seed (Mondrian.randomMondrian (pure (7 :: Int)) 0 box) == Mondrian.Block 7 box
  sequence
    [ assertBool "Mondrian likelihood and samples follow the cut" (isSuccess result)
    , assertBool "zero-budget Mondrian draws its base without cuts" (isSuccess prior)
    , assertBool "Mondrian samples the independently specified first cut and child boxes"
        (runProb (Mondrian.randomMondrian (pure 0.25) 0.4 box) halfTree == expectedPrior)
    ]

-- The Wiener bridge at unequal gaps. Given W(0) = 0 and W(T) = w, W(s) for
-- 0 < s < T is normal with mean (s/T) w and standard deviation
-- sqrt(s (T - s) / T), representable here even where s/T or s (T - s) / T is
-- not. On the tree whose every label is 1/4, every standard normal draw is the
-- lower quartile z = -0.6744897501960817, so w = sqrt T z and W(s) has an exact
-- value. Over seeds, the standardized W(s) is standard normal; that law is
-- checked with the Kolmogorov-Smirnov threshold of lawChecks.
wienerBridgeChecks :: IO [Bool]
wienerBridgeChecks = do
  let z = -0.6744897501960817
      leastPositive = encodeFloat 1 (-1074) :: Double
      afterT s t tree = let f = runProb wiener tree in f t `seq` (f t, f s)
      exactAt s t = do
        let expected = z * sqrt s * sqrt ((t - s) / t) + s * (z / sqrt t)
        r <- within 500000 (snd (afterT s t (constantLabel 0.25)))
        assertBool ("wiener at " ++ show s ++ " after " ++ show t ++ " on labels 1/4 is " ++ show expected
                      ++ ", got " ++ maybe "an error or no answer" show r)
          (maybe False (\v -> abs (v - expected) <= 1e-12 * abs expected) r)
      n = 10000 :: Int
      threshold = sqrt (log (2 / 1e-6) / (2 * fromIntegral n))
      standardNormalCdf x = erfc (- x / sqrt 2) / 2
      lawAt s t = do
        let residual seed = let (w, v) = afterT s t (randomTree (mkStdGen seed))
                            in (v - s * (w / t)) / (sqrt s * sqrt ((t - s) / t))
            d = ksDistance standardNormalCdf (map residual [1 .. n])
        assertBool ("wiener at " ++ show s ++ " after " ++ show t ++ ": standardized draws at distance " ++ show d
                      ++ " from the standard normal (threshold " ++ show threshold ++ ")")
          (d <= threshold)
  sequence
    [ exactAt 1e-200 1e200
    , exactAt leastPositive (2 * leastPositive)
    , exactAt 1 4
    , lawAt 1e-200 1e200
    , lawAt leastPositive (2 * leastPositive)
    , lawAt 1 4
    ]

-- Given f(2s), the Wiener bridge at s has mean f(2s)/2 and variance s/2, so on
-- one tree f(s)/sqrt(s) does not depend on s. A query at an invalid time is an
-- error and leaves the process unchanged.
processChecks :: IO [Bool]
processChecks = do
  let scaled s = let f = runProb wiener (randomTree (mkStdGen 13)) in f (2 * s) `seq` f s / sqrt s
      reference = scaled 1
      f = runProb wiener (randomTree (mkStdGen 14))
  scales <- sequence
    [ do value <- within 500000 (scaled s)
         assertBool ("wiener f(s)/sqrt(s) at s = " ++ show s ++ " is " ++ show reference ++ ", got " ++ show value)
           (maybe False (\v -> abs (v - reference) <= 1e-12 * abs reference) value)
    | s <- [1e-200, 1e-100, 1e100, 1e200] ]
  invalid <- try (evaluate (f (0/0)))
  after <- within 500000 (f 1)
  atomic <- sequence
    [ assertBool ("wiener rejects time NaN" ++ either (const "") (\v -> ", got " ++ show (v :: Double)) invalid)
        (either (\e -> const True (e :: ErrorCall)) (const False) invalid)
    , assertBool ("wiener after a rejected query gives a finite f 1, got " ++ show after)
        (maybe False (\v -> not (isNaN v || isInfinite v)) after)
    ]
  pure (scales ++ atomic)
