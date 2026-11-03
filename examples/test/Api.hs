{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Array (Array, Ix, listArray)
import Data.List (sort)
import Helpers (featureMembership, featureRowTicks, featureShade, maxWeightElement, takeProgressEveryDrop, takeWithProgress, hardTakeWithProgress)
import Control.Monad.Trans.Writer (runWriterT)
import Data.Monoid (getProduct)
import qualified AdditiveClustering as IBP
import qualified ClusteringDemo
import qualified GaussianProcessDemo
import qualified GraphDemo
import qualified ProgramInductionDemo
import qualified RegressionDemo
import qualified WienerDemo
import LazyPPL (Meas(..), Tree(..), randomTree, runProb, sample, scoreLog)
import LazyPPL.Distributions (normal)
import LazyPPL.Distributions.IBP (Dish(..), newCustomerS, newRestaurantS)
import Numeric.Log (Log(..))
import System.Random (mkStdGen)
import System.Environment (getArgs)
import Test.QuickCheck (isSuccess, quickCheckResult)

ibpInitialCustomers :: IO Bool
ibpInitialCustomers = do
  let Meas action = IBP.additive_clustering 2 6 0.3 IBP.countries_dataset
      ((features, _), weight) = runProb (runWriterT action) (randomTree (mkStdGen 12345))
      expected = map (map D)
        [[0,1], [0,1,2,3], [], [0], [], [0], [0], [0,3],
         [0], [0], [], [0], [0], [], [0], []]
  -- Customers get the matrix rows in the order they are first read, and the likelihood reads them
  -- before the features do.
  _ <- evaluate (ln (getProduct weight))
  pure (features == expected)

featureShownWhereHeld :: Bool
featureShownWhereHeld =
  let first = featureMembership (listArray (False, True) [["dish"], []])
      last' = featureMembership (listArray (False, True) [[], ["dish"]])
  in first == [[1], [0]] && last' == [[0], [1]]

featureShadesAvoidTheEnds :: Bool
featureShadesAvoidTheEnds = and
  [ featureShade columns 0 == 0 && all (\v -> v >= 0.25 && v <= 0.75) held && and (zipWith (<) held (tail held))
  | columns <- [1, 2, 255], let held = map (featureShade columns) [1 .. columns] ]

data TinyCountry = China | Cuba deriving (Eq, Ord, Show, Enum, Bounded, Ix)

-- The plot's rows are the array's own indices, at positions counted from the
-- first row, so a subrange array is labelled by the countries it contains and
-- not by the full enum.
featureRowTicksFollowArrayBounds :: Bool
featureRowTicksFollowArrayBounds =
  let chinaToCuba = listArray (China, Cuba) [[()], []] :: Array TinyCountry [()]
      cubaOnly = listArray (Cuba, Cuba) [[()]] :: Array TinyCountry [()]
  in featureRowTicks chinaToCuba == [(0, China), (1, Cuba)]
     && featureRowTicks cubaOnly == [(0, Cuba)]

-- | The tree whose every label is u.
constantTree :: Double -> Tree
constantTree u = let tree = Tree u (repeat tree) in tree

-- | The theta that GraphDemo's model draws from its prior on a given tree.
graphTheta :: Tree -> Double
graphTheta = snd . fst . fst . runProb (runWriterT action)
  where Meas action = GraphDemo.exampleA

-- | The value of a measure on the tree whose every label is u, and the log of its weight.
atConstantTree :: Double -> Meas a -> (a, Double)
atConstantTree u (Meas action) =
  let (value, weight) = runProb (runWriterT action) (constantTree u)
  in (value, ln (getProduct weight))

-- | The log density of the normal distribution with the given mean and standard deviation, at x.
logDensity :: Double -> Double -> Double -> Double
logDensity mean sigma x = negate (log (sigma * sqrt (2 * pi))) - ((x - mean) / sigma) ^ (2 :: Int) / 2

-- | The log weight of a regression model that scores each y by a normal density around f x.
regressionLogWeight :: Double -> (Double -> Double) -> [(Double, Double)] -> Double
regressionLogWeight sigma f dataset = sum [logDensity (f x) sigma y | (x, y) <- dataset]

-- | A regression model that returns its function, run on the constant tree u: its name, its log weight, and the log weight it must have.
regressionModelAt :: String -> Double -> Meas (Double -> Double) -> [(Double, Double)] -> (String, Double, Double)
regressionModelAt name u model dataset =
  let (f, observed) = atConstantTree u model
  in (name ++ " on the constant tree " ++ show u, observed, regressionLogWeight 0.3 f dataset)

-- | The clustering model, run on the constant tree u, with the log weight that its returned cluster parameters imply.
clusteringModelAt :: Double -> (String, Double, Double)
clusteringModelAt u =
  let (points, observed) = atConstantTree u ClusteringDemo.example
  in ( "ClusteringDemo.example on the constant tree " ++ show u, observed
     , sum [logDensity x s x' + logDensity y s y' | ((x', y'), _, (x, y, s)) <- points] )

-- | Matrix b differs from GraphDemo.test in three cells below the diagonal.
testWithThreeCellsFlipped :: [[Bool]]
testWithThreeCellsFlipped =
  [ [if (i, j) `elem` [(1, 0), (2, 1), (5, 3)] then not cell else cell | (j, cell) <- zip [0 :: Int ..] row]
  | (i, row) <- zip [0 :: Int ..] GraphDemo.test ]

-- | Every likelihood that the examples score must stay the log density, however far in the tail: (name, observed, expected).
logWeightChecks :: [(String, Double, Double)]
logWeightChecks =
  [ ("RegressionDemo.regress at 40 standard deviations", farTail (RegressionDemo.regress 1 (return (const 0)) farTailData), -800.9189385332047)
  , ("WienerDemo.regress at 40 standard deviations", farTail (WienerDemo.regress 1 (return (const 0)) farTailData), -800.9189385332047)
  , ("ProgramInductionDemo.regress at 40 standard deviations", farTail (ProgramInductionDemo.regress 1 (return (const 0, "0")) farTailData), -800.9189385332047)
  , ("GraphDemo.like 1e-10 when all 105 cells match", farTail (GraphDemo.like 1e-10 GraphDemo.test GraphDemo.test), 105 * log 1e-10)
  , ("GraphDemo.like 0.999 when 102 of 105 cells match", farTail (GraphDemo.like 0.999 GraphDemo.test testWithThreeCellsFlipped), 102 * log 0.999 + 3 * log 0.001)
  , ("GraphDemo.like 1 when all 105 cells match", farTail (GraphDemo.like 1 GraphDemo.test GraphDemo.test), 0)
  , ("GraphDemo.like 0 when none of the 105 cells matches", farTail (GraphDemo.like 0 GraphDemo.test (map (map not) GraphDemo.test)), 0)
  ]
  ++ [ regressionModelAt "WienerDemo.example" u WienerDemo.example WienerDemo.dataset | u <- [0.5, 1e-300] ]
  ++ [ regressionModelAt "GaussianProcessDemo.example" u GaussianProcessDemo.example GaussianProcessDemo.dataset | u <- [0.5, 1e-300] ]
  ++ [ clusteringModelAt u | u <- [0.5, 1 - 2 ^^ (-53 :: Int)] ]
  where farTail = snd . atConstantTree 0.5
        farTailData = [(0 :: Double, 40)]

examplesScoreLogDensities :: IO ()
examplesScoreLogDensities = do
  let agree observed expected = not (isNaN observed || isInfinite observed) && abs (observed - expected) <= 1e-9 * max 1 (abs expected)
      failures = [ name ++ ": log weight " ++ show observed ++ ", expected " ++ show expected
                 | (name, observed, expected) <- logWeightChecks, not (agree observed expected) ]
  unless (null failures) (fail (unlines failures))

-- GraphDemo says that theta is uniformly distributed in [0,pi].
graphThetaIsUniformOnZeroToPi :: IO ()
graphThetaIsUniformOnZeroToPi = do
  let half = graphTheta (constantTree 0.5)
      draws = 20000 :: Int
      thetas = [graphTheta (randomTree (mkStdGen seed)) | seed <- [1 .. draws]]
      quartiles = [0.25, 0.5, 0.75]
      shareBelow p = fromIntegral (length (filter (<= p * pi) thetas)) / fromIntegral draws :: Double
      shares = map shareBelow quartiles
      withinFourErrors p share = abs (share - p) < 4 * sqrt (p * (1 - p) / fromIntegral draws)
  let failures =
        [ "GraphDemo's prior puts theta at " ++ show half ++ " on the constant tree 0.5; Uniform[0,pi] puts it at " ++ show (pi / 2)
        | abs (half - pi / 2) >= 1e-12 ]
        ++ [ "GraphDemo's prior leaves [0,pi]: theta ranges over [" ++ show (minimum thetas) ++ "," ++ show (maximum thetas) ++ "]"
           | any (\theta -> theta < 0 || theta > pi) thetas ]
        ++ [ "GraphDemo's prior is not uniform on [0,pi]: the shares of theta below pi/4, pi/2 and 3pi/4 are " ++ show shares ++ ", not " ++ show quartiles
           | not (and (zipWith withinFourErrors quartiles shares)) ]
  unless (null failures) (fail (unlines failures))

-- The DKW bound gives a 1e-6 false-alarm probability for ideal independent draws;
-- these draws use fixed pseudorandom seeds.
graphOptimizedUniform :: IO ()
graphOptimizedUniform = do
  let count = 20000 :: Int
      values = sort [runProb (GraphDemo.superuniformbounded 3 0 pi) (randomTree (mkStdGen seed)) / pi | seed <- [1 .. count]]
      n = fromIntegral count
      cdf x = max 0 (min 1 x)
      distance = maximum [max (abs (cdf x - fromIntegral (i - 1) / n)) (abs (cdf x - fromIntegral i / n)) | (i, x) <- zip [1 :: Int ..] values]
      threshold = sqrt (log (2 / 1e-6) / (2 * n))
  unless (all (\x -> x >= 0 && x <= 1) values && distance <= threshold)
    (fail ("optimized uniform sampler: distance " ++ show distance ++ ", threshold " ++ show threshold ++ ", range " ++ show (minimum values, maximum values)))
  putStrLn ("optimized uniform sampler: distance " ++ show distance ++ ", threshold " ++ show threshold)

-- GraphDemo's pen-and-paper calculation: a triangle has chance r^3 in the Erdős-Rényi graph and 3/16 in the geometric graph on the circle
-- with theta = pi/2, so that, given a triangle, the geometric graph has chance 3/5.
graphTriangleChances :: IO ()
graphTriangleChances = do
  let draws = 40000 :: Int
      trees = [randomTree (mkStdGen seed) | seed <- [1 .. draws]]
      chance model = fromIntegral (length (filter (runProb model) trees)) / fromIntegral draws :: Double
      withinFourErrors expected observed = abs (observed - expected) < 4 * sqrt (expected * (1 - expected) / fromIntegral draws)
      Meas triangleModel = GraphDemo.exampleC
      withTriangle = [b | (b, weight) <- map (runProb (runWriterT triangleModel)) trees, not (isInfinite (ln (getProduct weight)))]
      geometricGivenTriangle = fromIntegral (length (filter not withTriangle)) / fromIntegral (length withTriangle) :: Double
      cases = [ ("an Erdős-Rényi graph with r = 0.5 has a triangle", 1 / 8, chance (GraphDemo.isTriangle (GraphDemo.erdosRenyi 0.5)))
              , ("an Erdős-Rényi graph with r = 0.8 has a triangle", 0.8 ^ (3 :: Int), chance (GraphDemo.isTriangle (GraphDemo.erdosRenyi 0.8)))
              , ("a geometric graph on the circle with theta = pi/2 has a triangle", 3 / 16, chance (GraphDemo.isTriangle (GraphDemo.SG 2 (pi / 2)))) ]
      failures = [ what ++ " with chance " ++ show observed ++ ", expected " ++ show expected
                 | (what, expected, observed) <- cases, not (withinFourErrors expected observed) ]
                 ++ [ "the geometric graph has chance " ++ show geometricGivenTriangle ++ " given a triangle, expected 0.6"
                    | abs (geometricGivenTriangle - 0.6) >= 4 * sqrt (0.24 / fromIntegral (length withTriangle)) ]
  unless (null failures) (fail (unlines failures))

-- The sample of highest weight is the one of highest likelihood, not the posterior mode: for the prior N(0,1) and one observation 2 with noise 1,
-- the likelihood peaks at 2 and the posterior N(1,1/2) peaks at 1.
highestWeightIsHighestLikelihood :: IO ()
highestWeightIsHighestLikelihood = do
  let Meas model = do { theta <- sample (normal 0 1); scoreLog (Exp (logDensity theta 1 2)); return theta }
      chosen = maxWeightElement [runProb (runWriterT model) (randomTree (mkStdGen seed)) | seed <- [1 .. 20000 :: Int]]
  unless (abs (chosen - 2) < 0.05)
    (fail ("The sample of highest weight is " ++ show chosen ++ "; the likelihood peaks at 2 and the posterior mode is 1"))

-- newRestaurantS generates sticks until the first one below 0.01 and keeps that one as the last. With every ratio equal to nu the sticks are
-- nu, nu^2, ..., so a customer who takes every stick takes as many dishes as the smallest k with nu^k < 0.01.
ibpSticksStopAtTheFirstBelowAHundredth :: IO ()
ibpSticksStopAtTheFirstBelowAHundredth = do
  let alpha = 2
      -- The Beta(alpha, 1) distribution has the distribution function x^alpha, so on the constant tree nu^alpha it draws nu.
      dishesOfCustomerWhoTakesEveryStick nu =
        length (runProb (newCustomerS (runProb (newRestaurantS alpha) (constantTree (nu ** alpha)))) (constantTree 1e-300))
      smallestKWithNuToTheKBelowAHundredth nu = head [k | k <- [1 :: Int ..], nu ^ k < 0.01]
      failures = [ "nu = " ++ show nu ++ ": the customer takes " ++ show observed ++ " dishes, expected " ++ show expected
                 | nu <- [0.5, 0.9, 0.99], let observed = dishesOfCustomerWhoTakesEveryStick nu
                 , let expected = smallestKWithNuToTheKBelowAHundredth nu, observed /= expected ]
  unless (null failures) (fail (unlines failures))

-- Dropping the sticks after the first one below 0.01 loses alpha * r expected dishes per customer, r being the last kept stick, so less than 0.01 * alpha.
-- A customer of the exact Indian buffet process takes alpha dishes in expectation.
ibpTruncationLosesLessThanAHundredthOfAlpha :: IO ()
ibpTruncationLosesLessThanAHundredthOfAlpha = do
  let alpha = 2 :: Double
      customers = 20000 :: Int
      dishes seed = fromIntegral (length (runProb (newCustomerS (runProb (newRestaurantS alpha) (randomTree (mkStdGen (2 * seed)))))
                                                  (randomTree (mkStdGen (2 * seed + 1))))) :: Double
      counts = map dishes [1 .. customers]
      n = fromIntegral customers
      mean = sum counts / n
      standardError = sqrt (sum [(c - mean) ^ (2 :: Int) | c <- counts] / (n - 1) / n)
  unless (mean <= alpha + 4 * standardError && mean >= alpha - 0.01 * alpha - 4 * standardError)
    (fail ("A customer takes " ++ show mean ++ " dishes on average (standard error " ++ show standardError ++ "); the exact process gives " ++ show alpha
           ++ " and the truncation may lose up to " ++ show (0.01 * alpha)))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["graph-uniform"] -> graphOptimizedUniform
    ["progress"] -> do
      soft <- takeWithProgress 1 [1 :: Int]
      hard <- hardTakeWithProgress 1 [1 :: Int]
      short <- takeWithProgress 2 [1 :: Int]
      unless (soft == [1] && hard == [1] && short == [1]) (fail "progress changed the values")
    _ -> checks

checks :: IO ()
checks = do
  subrange <- quickCheckResult $ \(x :: Int) ->
    featureMembership (listArray (False, False) [[x]]) == [[1]]
  unless (isSuccess subrange) (fail "featureMembership indexes outside its array")
  unless (maxWeightElement [("first",2 :: Int),("second",2),("last",1)] == "first")
    (fail "maxWeightElement must keep the first tie")
  thinned <- takeProgressEveryDrop 3 2 1 [0 :: Int ..]
  -- Dropping 0 leaves 1,2,3,..., and every second element of that is 2,4,6.
  unless (thinned == [2,4,6]) (fail "progress/thinning changes selected values")
  unless featureShadesAvoidTheEnds (fail ("Feature shades reach the colormap's light ends: " ++
    show [map (featureShade 255) [1, 255], map (featureShade 1) [1]]))
  putStrLn "PASS feature shades avoid the colormap's light ends"
  unless featureShownWhereHeld (fail ("Feature membership shows a held feature: " ++
    show (featureMembership (listArray (False, True) [["dish"], []] :: Array Bool [String]))))
  putStrLn "PASS feature membership shows a held feature"
  unless featureRowTicksFollowArrayBounds
    (fail ("Feature row ticks must follow the array's own bounds: " ++
      show (featureRowTicks (listArray (Cuba, Cuba) [[()]] :: Array TinyCountry [()]))))
  putStrLn "PASS feature row ticks follow the array's own bounds"
  same <- ibpInitialCustomers
  unless same (fail "IBP initial customers match seed 12345")
  putStrLn "PASS IBP initial customers match seed 12345"
  graphThetaIsUniformOnZeroToPi
  putStrLn "PASS GraphDemo's theta is uniform on [0,pi]"
  graphOptimizedUniform
  putStrLn "PASS GraphDemo's optimized sampler is uniform on [0,pi]"
  examplesScoreLogDensities
  putStrLn "PASS the examples score log densities, finite in the far tail"
  graphTriangleChances
  putStrLn "PASS GraphDemo's triangle chances match its pen-and-paper calculation"
  highestWeightIsHighestLikelihood
  putStrLn "PASS the sample of highest weight is the one of highest likelihood"
  ibpSticksStopAtTheFirstBelowAHundredth
  putStrLn "PASS newRestaurantS keeps sticks up to the first one below 0.01"
  ibpTruncationLosesLessThanAHundredthOfAlpha
  putStrLn "PASS newRestaurantS loses less than a hundredth of alpha dishes per customer"
