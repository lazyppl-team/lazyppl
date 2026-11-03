{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Api.Distributions where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import Numeric.Log (Log(..), ln)
import Numeric (expm1, log1p)
import System.Timeout (timeout)
import LazyPPL.Distributions
import qualified LazyPPL.Distributions.IBP as IBP
import System.Environment (getArgs, getExecutablePath)
import System.Process.Typed (proc, readProcessStdout)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Random (RandomGen(..), SplitGen(..), mkStdGen, setStdGen)
import Control.Monad (replicateM)

import Api.Support

-- 5000 draws of normal 3 2 on a fixed seed, so the sampler is checked, not just
-- the density.
normalSampleMoments :: (Double, Double)
normalSampleMoments =
  let draws = runProb (replicateM 5000 (normal 3 2)) (randomTree (mkStdGen 7))
      n = fromIntegral (length draws) :: Double
      mean = sum draws / n
      variance = sum [(x - mean) ^ (2 :: Int) | x <- draws] / (n - 1)
  in (mean, variance)

-- Draws on the simplex. With every label 1/4, normalised Gamma draws underflow
-- to 0 for small concentrations; a draw must still be a point of the simplex.
dirichletSamplingChecks :: IO [Bool]
dirichletSamplingChecks = do
  let quarter = let t = Tree 0.25 (repeat t) in t
      sparse = runProb (dirichlet [0.001, 0.001]) quarter
      single = runProb (dirichlet [0.001]) quarter
      onSimplex xs = all (\x -> x >= 0 && not (isNaN x || isInfinite x)) xs
        && abs (sum xs - 1) <= fromIntegral (length xs) * 2.220446049250313e-16
  points <- sequence
    [ assertBool ("dirichlet [0.001,0.001] at labels 1/4 is on the simplex, got " ++ show sparse) (onSimplex sparse)
    , assertBool ("dirichlet [0.001] is exactly [1], got " ++ show single) (single == [1])
    ]
  moments <- concat <$> mapM ensemble [[0.5, 1.5, 2], [2, 1.5, 0.5], [0.05, 0.05, 0.1], [0.001, 0.001, 0.002]]
  pure (points ++ moments)
  where
    -- 20000 seeded draws. The means, the variances and the variance of x0 + x1,
    -- whose law is Beta(a0 + a1, a2), lie within six standard errors of their
    -- exact values. For a variable in [0,1], E[X^2] bounds n times the variance
    -- of its sample variance.
    ensemble alphas = do
      let n = 20000 :: Int
          draws = runProb (replicateM n (dirichlet alphas)) (randomTree (mkStdGen 11))
          total = sum alphas
          mean xs = sum xs / fromIntegral n
          variance xs = let m = mean xs in sum [(x - m) ^ (2 :: Int) | x <- xs] / fromIntegral (n - 1)
          betaVariance a b = a * b / ((a + b) ^ (2 :: Int) * (a + b + 1))
          coordinate i = map (!! i) draws
          pairSum = map (\d -> d !! 0 + d !! 1) draws
          varianceError xs = 6 * sqrt (mean (map (^ (2 :: Int)) xs) / fromIntegral n)
          name = "dirichlet " ++ show alphas
      sequence $
        [ approxEq (name ++ " mean of x" ++ show i) (a / total) (mean (coordinate i))
            (6 * sqrt (betaVariance a (total - a) / fromIntegral n))
        | (i, a) <- zip [0 :: Int ..] alphas ] ++
        [ approxEq (name ++ " variance of x" ++ show i) (betaVariance a (total - a)) (variance (coordinate i))
            (varianceError (coordinate i))
        | (i, a) <- zip [0 :: Int ..] alphas ] ++
        [ approxEq (name ++ " variance of x0 + x1") (betaVariance (alphas !! 0 + alphas !! 1) (alphas !! 2))
            (variance pairSum) (varianceError pairSum) ]

-- A distribution function on [0,1], extended by 0 below and 1 above.
onUnit :: (Double -> Double) -> Double -> Double
onUnit f x = f (max 0 (min 1 x))

-- Beta(1,b) has the distribution function 1 - (1-x)^b and the quantile
-- 1 - (1-u)^(1/b); Beta(a,1) has x^a and u^(1/a).
beta1Cdf, betaA1Cdf :: Double -> Double -> Double
beta1Cdf b = onUnit (\x -> - expm1 (b * log1p (- x)))
betaA1Cdf a = onUnit (\x -> exp (a * log x))

-- Laws of the Beta and Dirichlet samplers. Each check draws n = 10000 values on
-- one seeded tree and computes ksDistance from an exact distribution function F.
-- For n independent draws from F, the Dvoretzky-Kiefer-Wolfowitz inequality with
-- Massart's constant gives P(sup |F_n - F| > e) <= 2 exp(-2 n e^2): at
-- e = 0.0269 below 1e-6, and for draws from a law whose distribution function
-- differs from F by at least 0.0269 + 0.0195 = 0.0464 at a point, a distance
-- above e with probability at least 0.999. These figures hold for ideal
-- independent draws; the draws here come from one fixed seed of a pseudorandom
-- generator. ksDistance compares each draw with F at the draw and at the double
-- before it, so a difference between two laws inside that interval is not
-- measured.
lawChecks :: IO [Bool]
lawChecks = do
  let n = 10000 :: Int
      threshold = sqrt (log (2 / 1e-6) / (2 * fromIntegral n))
      law name seed cdf p = do
        let d = ksDistance cdf (runProb (replicateM n p) (randomTree (mkStdGen seed)))
        assertBool (name ++ ": distance " ++ show d ++ " from the exact distribution function (threshold " ++ show threshold ++ ")")
          (d <= threshold)
      coordinate i alphas = (!! i) <$> dirichlet alphas
  sequence
    [ law "beta 1 0.001" 21 (beta1Cdf 0.001) (beta 1 0.001)
    , law "beta 1 0.1" 22 (beta1Cdf 0.1) (beta 1 0.1)
    , law "beta 0.001 1" 23 (betaA1Cdf 0.001) (beta 0.001 1)
    , law "beta 0.5 0.5" 24 (onUnit (\x -> 2 / pi * asin (sqrt x))) (beta 0.5 0.5)
    , law "beta 2 3" 25 (onUnit (\x -> x * x * (6 - 8 * x + 3 * x * x))) (beta 2 3)
      -- The coordinate of concentration 1 next to 0.001 is Beta(1, 0.001), in either order.
    , law "dirichlet [1,0.001] x0" 26 (beta1Cdf 0.001) (coordinate 0 [1, 0.001])
    , law "dirichlet [0.001,1] x1" 27 (beta1Cdf 0.001) (coordinate 1 [0.001, 1])
      -- x_i is Beta(a_i, a - a_i) with a the total; Beta(2,2) has 3x^2 - 2x^3.
    , law "dirichlet [0.5,1.5,2] x2" 28 (onUnit (\x -> x * x * (3 - 2 * x))) (coordinate 2 [0.5, 1.5, 2])
    , law "dirichlet [1,0.01,0.01] x0" 29 (beta1Cdf 0.02) (coordinate 0 [1, 0.01, 0.01])
    ]

-- The documented domains (LazyPPL.Distributions): dirichlet takes concentrations
-- of at least 0.001 each with a sum of at most 2000; beta takes the same
-- parameters, and Beta(a,1) and Beta(1,b) for every finite positive a and b.
dirichletInDomain, betaInDomain :: Double -> Double -> Bool
dirichletInDomain a b = a >= 0.001 && b >= 0.001 && a + b <= 2000
betaInDomain a b = dirichletInDomain a b || (a == 1 && positive b) || (b == 1 && positive a)
  where positive c = c > 0 && not (isInfinite c)

-- The Beta sampler on constant trees against test/reference/beta-quantiles.tsv.
-- Inside the documented domain of each, beta a b and the two coordinates of
-- dirichlet [a,b] at a supported label u lie in the row's intervals: the doubles
-- whose distribution function differs from u by at most 1e-9 min(u, 1 - u),
-- joined with the two doubles next to the exact quantile. A row marked reject
-- has no intervals and checks both samplers' errors instead. Outside the shape
-- domain or below 2^-53 for general shapes, each raises an error. With
-- insideOnly, only the rows inside beta's shape domain are read; the
-- samplers of an earlier version, which have no domain, can take minutes
-- outside it.
betaQuantileChecks :: Bool -> IO [Bool]
betaQuantileChecks insideOnly = do
  rows <- referenceTable "beta-quantiles.tsv"
  results <- mapM check [r | r@(a : b : _) <- rows, not insideOnly || betaInDomain (read a) (read b)]
  tableReport "beta and dirichlet [a,b] in the quantile intervals of beta-quantiles.tsv" results
  where
    check [a', b', u', "reject"] = do
      let (a, b, u) = (read a', read b', read u') :: (Double, Double, Double)
          tree = constantLabel u
          unsupportedLabel = u < encodeFloat 1 (-53) && a /= 1 && b /= 1
          betaSupportedHere = betaInDomain a b && not unsupportedLabel
          dirichletSupportedHere = dirichletInDomain a b && not unsupportedLabel
      single <- if betaInDomain a b
        then raisesBetaLabel (runProb (beta a b) tree)
        else raisesError (runProb (beta a b) tree)
      pair <- if dirichletInDomain a b
        then raisesBetaLabel (sum (runProb (dirichlet [a,b]) tree))
        else raisesError (sum (runProb (dirichlet [a,b]) tree))
      pure (not betaSupportedHere && not dirichletSupportedHere && single && pair,
            "beta and dirichlet must reject " ++ show (a,b,u))
    check (a' : b' : u' : xlo' : xhi' : ylo' : yhi' : _) = do
      let (a, b, u, xlo, xhi, ylo, yhi) = (read a', read b', read u', read xlo', read xhi', read ylo', read yhi') :: (Double, Double, Double, Double, Double, Double, Double)
          tree = constantLabel u
          name = "beta " ++ show a ++ " " ++ show b ++ " at label " ++ show u
          inside lo hi v = lo <= v && v <= hi
          answer v = maybe "an error or no answer in 2 s" show v
          intervals = "x in [" ++ show xlo ++ "," ++ show xhi ++ "] and 1-x in [" ++ show ylo ++ "," ++ show yhi ++ "]"
      let unsupportedLabel = u < encodeFloat 1 (-53) && a /= 1 && b /= 1
      single <- if betaInDomain a b && unsupportedLabel
        then (\e -> (e, "general beta rejects the custom label below 2^-53")) <$> raisesBetaLabel (runProb (beta a b) tree)
        else if betaInDomain a b
        then do
          x <- within 2000000 (runProb (beta a b) tree)
          pure (maybe False (inside xlo xhi) x, "beta: " ++ intervals ++ ", got " ++ answer x)
        else (\e -> (e, "beta is outside its domain and must raise an error")) <$> raisesError (runProb (beta a b) tree)
      pair <- if dirichletInDomain a b && unsupportedLabel
        then (\e -> (e, "dirichlet rejects the custom label below 2^-53")) <$> raisesBetaLabel (sum (runProb (dirichlet [a, b]) tree))
        else if dirichletInDomain a b
        then do
          xs <- within 2000000 (let xs = runProb (dirichlet [a, b]) tree in sum xs `seq` xs)
          pure (case xs of
                  Just [v, w] -> inside xlo xhi v && inside ylo yhi w
                  _ -> False, "dirichlet: " ++ intervals ++ ", got " ++ answer xs)
        else (\e -> (e, "dirichlet is outside its domain and must raise an error")) <$> raisesError (sum (runProb (dirichlet [a, b]) tree))
      pure (fst single && fst pair, name ++ ": " ++ snd single ++ "; " ++ snd pair)
    check row = pure (False, "malformed row " ++ show row)

-- dirichlet as at the constant label u against the exact stick-breaking
-- coordinates of test/reference/dirichlet-sticks.tsv. Coordinate i is the
-- remaining stick times the u-quantile of Beta(a_i, a_(i+1) + ... + a_k), and
-- its interval adds the relative tolerances of the shares that form it.
dirichletStickChecks :: IO [Bool]
dirichletStickChecks = do
  rows <- referenceTable "dirichlet-sticks.tsv"
  results <- mapM check rows
  tableReport "dirichlet at the exact stick-breaking coordinates of dirichlet-sticks.tsv" results
  where
    check (alphas' : u' : bounds') = do
      let alphas = read ("[" ++ alphas' ++ "]") :: [Double]
          u = read u' :: Double
          bounds = intervals (map read bounds')
          unsupportedLabel = u < encodeFloat 1 (-53) && any (\(a, rest) -> a /= 1 && foldr (+) 0 rest /= 1)
            [(a, rest) | (a : rest) <- tails alphas, not (null rest)]
      if unsupportedLabel
        then do
          rejected <- raisesBetaLabel (sum (runProb (dirichlet alphas) (constantLabel u)))
          pure (rejected, "dirichlet " ++ show alphas ++ " must reject label " ++ show u)
        else do
          xs <- within 2000000 (let v = runProb (dirichlet alphas) (constantLabel u) in sum v `seq` v)
          let ok = maybe False (\v -> length v == length bounds && and (zipWith (\c (lo, hi) -> lo <= c && c <= hi) v bounds)) xs
          pure (ok, "dirichlet " ++ show alphas ++ " at label " ++ show u ++ ": coordinates in " ++ show bounds
                      ++ ", got " ++ maybe "an error or no answer in 2 s" show xs)
    check row = pure (False, "malformed row " ++ show row)
    intervals (lo : hi : rest) = (lo, hi) : intervals rest
    intervals _ = [] :: [(Double, Double)]

-- For sorted xs and sorted points, how many xs are at most each point.
atOrBelow :: [Double] -> [Double] -> [Int]
atOrBelow = go 0
  where
    go _ _ [] = []
    go k xs (p : ps) = let (le, rest) = span (<= p) xs; k' = k + length le in k' : go k' rest ps

-- Laws of the Beta sampler: 10000 seeded draws against the exact distribution
-- function of test/reference/beta-laws.tsv at its grid of points, for two shapes
-- without a closed form and for Beta(1,b) and Beta(a,1) at 1e-4 and 1e4, the
-- sticks of the Dirichlet process and of IBP's newRestaurantS. The largest distance over the grid is at most the Kolmogorov-Smirnov
-- statistic, so the threshold of lawChecks keeps its false-alarm bound; the
-- grid's points are close enough that the distribution function rises by less
-- than 0.005 between neighbours.
gridLawChecks :: IO [Bool]
gridLawChecks = do
  rows <- referenceTable "beta-laws.tsv"
  let grid = [((read a, read b), (read x, read f)) | [a, b, x, f] <- rows] :: [((Double, Double), (Double, Double))]
      n = 10000 :: Int
      threshold = sqrt (log (2 / 1e-6) / (2 * fromIntegral n))
      shapes = [(2.5, 3.5), (1000, 1000), (1, 1e-4), (1, 1e4), (1e-4, 1), (1e4, 1)]
  coverage <- assertBool "beta-laws.tsv covers both general shapes and all four nonparametric stick laws"
                (sort (nub (map fst grid)) == sort shapes)
  results <- sequence
    [ do let points = [p | (s, p) <- grid, s == (a, b)]
             draws = sort (runProb (replicateM n (beta a b)) (randomTree (mkStdGen seed)))
             d = maximum (zipWith (\k (_, f) -> abs (fromIntegral k / fromIntegral n - f)) (atOrBelow draws (map fst points)) points)
         result <- try (evaluate d)
         case result of
           Right distance -> assertBool ("beta " ++ show a ++ " " ++ show b ++ ": distance " ++ show distance ++ " from the exact distribution function at "
                                            ++ show (length points) ++ " points (threshold " ++ show threshold ++ ")") (distance <= threshold)
           Left err -> assertBool ("beta " ++ show a ++ " " ++ show b ++ ": " ++ takeWhile (/= '\n') (show (err :: ErrorCall))) False
    | (seed, (a, b)) <- zip [41 ..] (nub (map fst grid)) ]
  pure (coverage : results)

-- One row of test/reference/log-densities.tsv: the function at the arguments is
-- within 1e-12 max(1, |exact|) of the exact value, or is the same infinity where
-- the exact value leaves the double range.
densityRow :: [String] -> (Bool, String)
densityRow [function, arguments, exact'] =
  let expected = read exact' :: Double
      actual = case (function, words arguments) of
        ("poissonLogPmf", [l, k]) -> poissonLogPmf (read l) (read k)
        ("gammaLogPdf", [k, t, x]) -> gammaLogPdf (read k) (read t) (read x)
        ("betaLogPdf", [a, b, x]) -> betaLogPdf (read a) (read b) (read x)
        ("binomialLogPmf", [n, p, k]) -> binomialLogPmf (read n) (read p) (read k)
        ("dirichletLogPdf", [as, xs]) -> dirichletLogPdf (read as) (read xs)
        _ -> error ("log-densities.tsv: unknown row " ++ function ++ " " ++ arguments)
      ok | isInfinite expected = actual == expected
         | otherwise = abs (actual - expected) <= 1e-12 * max 1 (abs expected)
  in (ok, function ++ " " ++ arguments ++ " is " ++ show expected ++ ", got " ++ show actual)
densityRow row = (False, "malformed row " ++ show row)

-- The rows of test/reference/log-densities.tsv run in a child process, this
-- program with the argument "density-rows", which prints one verdict per row.
-- A computation that loops without allocating cannot be interrupted inside its
-- own process (poissonLogPmf 1e308 (10^308) once did), so the deadline is kept
-- from outside: a child that has not exited within 120 s is terminated, and
-- every row without a verdict fails.
densityChecks :: IO [Bool]
densityChecks = do
  rows <- referenceTable "log-densities.tsv"
  out <- externally 120000000 ["density-rows"]
  let reported = [l | l <- maybe [] lines out, take 4 l `elem` ["PASS", "FAIL"]]
      missing = length rows - length reported
  mapM_ putStrLn [l | l <- reported, take 4 l == "FAIL"]
  putStrLn ((if missing == 0 && all ((== "PASS") . take 4) reported then "PASS " else "FAIL ")
            ++ "log densities of log-densities.tsv: " ++ show (length (filter ((== "PASS") . take 4) reported))
            ++ " of " ++ show (length rows) ++ " rows"
            ++ (if missing > 0 then ", " ++ show missing ++ " without a verdict" ++ maybe " (no exit within 120 s)" (const "") out else ""))
  pure (map ((== "PASS") . take 4) reported ++ replicate missing False)

densityRowsChild :: IO ()
densityRowsChild = do
  rows <- referenceTable "log-densities.tsv"
  mapM_ (\row -> let (ok, detail) = densityRow row in putStrLn ((if ok then "PASS " else "FAIL ") ++ detail)) rows

-- Runs this test program with the given arguments and returns its standard
-- output, or Nothing when it has not exited within the given microseconds, in
-- which case it is terminated. typed-process reads the output while the child
-- runs, so a full pipe cannot stop it.
externally :: Int -> [String] -> IO (Maybe String)
externally micros args = do
  self <- getExecutablePath
  fmap (BL.unpack . snd) <$> timeout micros (readProcessStdout (proc self args))

-- Log densities and draws at extreme but valid parameters. Each expected value
-- comes from the closed form or asymptotic expansion named beside it.
numericChecks :: IO [Bool]
numericChecks = do
  let half = let t = Tree 0.5 (repeat t) in t
      relative name expected actual = approxEq name expected actual (1e-12 * max 1 (abs expected))
      exactly name expected actual = assertBool (name ++ " is " ++ show expected ++ ", got " ++ show actual) (actual == expected)
  sequence
    [ -- z = 1: -log(2 pi)/2 - log 1e200 - 1/2
      relative "normalLogPdf 0 1e200 1e200" (-461.9359571320138) (normalLogPdf 0 1e200 1e200)
      -- z = 0: -log(2 pi)/2 + log 1e200
    , relative "normalLogPdf 0 1e-200 0" 459.5980800656045 (normalLogPdf 0 1e-200 0)
    , relative "logNormalLogPdf 0 1e-200 1" 459.5980800656045 (logNormalLogPdf 0 1e-200 1)
      -- z = 2 while x - mu exceeds the largest double: -log(2 pi)/2 - log 1e308 - 2
    , relative "normalLogPdf -1e308 1e308 1e308" (-712.1151471753708) (normalLogPdf (-1e308) 1e308 1e308)
      -- the median of a normal is its mean, up to the quantile's absolute accuracy near 0
    , let v = runProb (normal 0 1.5e308) half
      in assertBool ("normal 0 1.5e308 at label 1/2 is within 1e-15 sigma of 0, got " ++ show v) (abs v <= 1e-15 * 1.5e308)
      -- width 2e308: -log 2 - log 1e308
    , approxEq "uniformBoundedLogPdf -1e308 1e308 0" (-709.8893558227261) (uniformBoundedLogPdf (-1e308) 1e308 0) 1e-9
    , exactly "uniformbounded -1e308 1e308 at label 1/2" 0 (runProb (uniformbounded (-1e308) 1e308) half)
    ]
