{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Api.Domains where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Data.List (foldl', isInfixOf, isPrefixOf, nub, sort, tails)
import Data.Word (Word64)
import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import Control.Monad.Trans.Writer (runWriterT)
import Data.Monoid (Product(..), getProduct)
import Numeric.Log (Log(..), ln)
import qualified Numeric.Sum as Sum
import System.Timeout (timeout)
import LazyPPL.Distributions
import qualified LazyPPL.Distributions.IBP as IBP
import qualified LazyPPL.Distributions.DirichletP as DP
import System.Random (RandomGen(..), SplitGen(..), mkStdGen, setStdGen)
import Control.Monad (replicateM)
import Test.QuickCheck (NonEmptyList(..), NonNegative(..), Positive(..), Property, ioProperty, isSuccess, quickCheckResult)

import Api.Support

-- An impossible event has mass 0, so its log is negative infinity.
densityBoundaryChecks :: IO [Bool]
densityBoundaryChecks = sequence
  [ assertBool name (isInfinite actual && actual < 0)
  | (name, actual) <-
      [ ("Bernoulli impossible success", bernoulliLogPmf 0 True)
      , ("Bernoulli impossible failure", bernoulliLogPmf 1 False)
      , ("categorical zero mass", categoricalLogPmf [1,0] 1)
      , ("categorical index outside support", categoricalLogPmf [1] 1)
      , ("Poisson positive count at rate zero", poissonLogPmf 0 1)
      , ("Dirichlet outside simplex", dirichletLogPdf [1,1,1] [0.2,0.2,0.2])
      , ("Dirichlet empty simplex", dirichletLogPdf [] [])
      , ("exponential log density outside support", exponentialLogPdf 1 (-1))
      ]
  ] >>= \results -> (results ++) <$> sequence
  [ approxEq "Poisson zero rate and zero count" 0 (poissonLogPmf 0 0) 1e-15
  , approxEq "Poisson density zero rate and zero count" 1 (poissonPdf 0 0) 1e-15
  , do
      result <- try (evaluate (poissonPdf 1 (-1))) :: IO (Either ErrorCall Double)
      case result of
        Left err -> putStrLn ("FAIL Poisson negative count: " ++ show err) >> pure False
        Right value -> approxEq "Poisson negative count" 0 value 1e-15
  , approxEq "exponential density outside support" 0 (expPdf 1 (-1)) 1e-15
  , approxEq "Dirichlet(1,1,1) simplex density" (log 2) (dirichletLogPdf [1,1,1] [0.25,0.25,0.5]) 1e-12
  , approxEq "binomial certain failure" 0 (binomialLogPmf 5 0 0) 1e-15
  , approxEq "binomial certain success" 0 (binomialLogPmf 5 1 5) 1e-15
  , let Meas action = score 0
        (_, weight) = runProb (runWriterT action) (randomTree (mkStdGen 1))
    in assertBool "score zero stays impossible" (ln (getProduct weight) == -1/0)
  , and <$> mapM (\x -> do
      let Meas action = score x
          (_, weight) = runProb (runWriterT action) (randomTree (mkStdGen 1))
      result <- try (evaluate (ln (getProduct weight))) :: IO (Either ErrorCall Double)
      assertBool "invalid score is rejected" (either (const True) (const False) result)) [-1, 0/0, 1/0]
  , isSuccess <$> quickCheckResult dirichletNormalizedInput
  ]

dirichletNormalizedInput :: NonEmptyList (Positive Int) -> Bool
dirichletNormalizedInput (NonEmpty positive) =
  let values = map (fromIntegral . getPositive) positive :: [Double]
      xs = map (/ sum values) values
      density = dirichletLogPdf (replicate (length xs) 1) xs
  in not (isNaN density || isInfinite density)

-- The Dirichlet process scores atoms on an infinite stick-breaking probability
-- stream, so `categoricalLogPmf` must inspect only the prefix up to the selected
-- element and return on an infinite stream.
lazyCategoricalChecks :: IO [Bool]
lazyCategoricalChecks = do
  let stream = 0 : 1 : repeat 0
  selected <- evaluate (categoricalLogPmf stream 1)
  zeroMass <- evaluate (categoricalLogPmf stream 0)
  invalid <- try (evaluate (categoricalLogPmf (1.5 : repeat 0) 0)) :: IO (Either ErrorCall Double)
  sequence
    [ assertBool "categoricalLogPmf scores an infinite probability stream" (selected == log 1)
    , assertBool "categoricalLogPmf gives negative infinity for zero mass on an infinite stream"
        (isInfinite zeroMass && zeroMass < 0)
    , assertBool "categoricalLogPmf rejects an invalid inspected probability"
        (either (const True) (const False) invalid)
    ]

-- A generator that returns one word forever, so every label of the tree is the
-- label of that word.
newtype ConstWord = ConstWord Word64

instance RandomGen ConstWord where
  genWord64 g@(ConstWord w) = (w, g)

instance SplitGen ConstWord where
  splitGen g = (g, g)

categoricalBoundaryChecks :: IO [Bool]
categoricalBoundaryChecks = do
  let e = encodeFloat 1 (-54)
      weights = [0.5, e, e, e, e, 0.5 - 4 * e]
      tree = randomTree (ConstWord 0)
  result <- try (timeout 500000 (evaluate (runProb (categorical weights) tree))) :: IO (Either SomeException (Maybe Int))
  sequence
    [ assertBool ("categorical selects the final positive weight at the maximum label, got " ++ show result)
        (case result of Right (Just 5) -> True; _ -> False)
    ]

categoricalFiniteProperty :: NonEmptyList Word64 -> Word64 -> Property
categoricalFiniteProperty (NonEmpty values) word = ioProperty $ do
  let masses = 1 : map (fromIntegral . (`mod` 1024)) values
      weights = map (/ sum masses) masses
      labels = [0, 1 - encodeFloat 1 (-53), runProb uniform (randomTree (ConstWord word))]
      valid (Just i) = i >= 0 && i < length weights && weights !! i > 0
      valid Nothing = False
  draws <- mapM (\label -> within 500000 (runProb (categorical weights) (constantLabel label))) labels
  pure (all valid draws)

categoricalInfiniteProperty :: NonEmptyList Word64 -> NonNegative Int -> Word64 -> Property
categoricalInfiniteProperty (NonEmpty values) (NonNegative prefix) word = ioProperty $ do
  let splitMass n value =
        let mass = encodeFloat 1 (-n)
            fraction = fromIntegral (value `mod` 17) / 16
        in [mass * fraction, mass * (1 - fraction)]
      weights = replicate (prefix `mod` 100) 0 ++ concat (zipWith splitMass [1..] (cycle values))
      labels = [0, 1 - encodeFloat 1 (-53), runProb uniform (randomTree (ConstWord word))]
      valid (Just i) = i >= 0 && weights !! i > 0
      valid Nothing = False
  draws <- mapM (\label -> within 500000 (runProb (categorical weights) (constantLabel label))) labels
  pure (all valid draws)

categoricalPropertyChecks :: IO [Bool]
categoricalPropertyChecks = do
  finite <- quickCheckResult categoricalFiniteProperty
  infinite <- quickCheckResult categoricalInfiniteProperty
  shortDraw <- try (evaluate (runProb (categorical [0.5, 0.3]) (constantLabel 0.9))) :: IO (Either ErrorCall Int)
  quarterShort <- try (evaluate (runProb (categorical [0.5, 0.25]) (constantLabel 0.9999999999999999))) :: IO (Either ErrorCall Int)
  tailDraw <- try (evaluate (runProb (categorical [0.2, 0, 0]) (constantLabel 0.9))) :: IO (Either ErrorCall Int)
  let nearOne = [0.5, 0.5 - encodeFloat 1 (-40)]
      maxLabel = constantLabel (1 - encodeFloat 1 (-53))
  oneUlpDraw <- try (evaluate (runProb (categorical [1 - encodeFloat 1 (-53)]) maxLabel)) :: IO (Either ErrorCall Int)
  paddedDraw <- try (evaluate (runProb (categorical (nearOne ++ replicate 8190 0)) maxLabel)) :: IO (Either ErrorCall Int)
  tinyDraw <- try (evaluate (runProb (categorical (nearOne ++ replicate 8190 (encodeFloat 1 (-1074)))) maxLabel)) :: IO (Either ErrorCall Int)
  earlyDraw <- within 500000 (runProb (categorical [0.5, 0.3]) (constantLabel 0.4))
  lazyDraw <- within 500000 (runProb (categorical (0.8 : error "categorical forced the unchosen tail")) (constantLabel 0.4))
  roundedDraw <- within 500000 (runProb (categorical [1/590, 459/590, 130/590]) (constantLabel (1 - encodeFloat 1 (-53))))
  decimalDraw <- within 500000 (runProb (categorical [0.1, 0.2, 0.7]) (constantLabel (1 - encodeFloat 1 (-53))))
  decimalFallback <- within 500000 (runProb (categorical [0.50000000000000005, 0.250000000000000025, 0.249999999999999925]) maxLabel)
  let equalWeights = replicate 49 (1/49) :: [Double]
      equalTotal = Sum.kbn (foldl' Sum.add Sum.zero equalWeights)
  equalDraw <- within 500000 (runProb (categorical equalWeights) maxLabel)
  negativeZeroDraw <- within 500000 (runProb (categorical ((-0.0) : replicate 49 (1/49))) maxLabel)
  allZero <- rejects "categorical [0,0]" (runProb (categorical [0, 0]) (constantLabel 0.9))
  sequence
    [ assertBool "categorical chooses positive finite weights" (isSuccess finite)
    , assertBool "categorical chooses positive weights from an infinite stream" (isSuccess infinite)
    , assertBool ("categorical rejects missing mass in [0.5,0.3], got " ++ show shortDraw)
        (case shortDraw of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool "categorical rejects [0.5,0.25] at the largest label"
        (case quarterShort of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool ("categorical rejects missing mass before trailing zeros, got " ++ show tailDraw)
        (case tailDraw of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool "categorical rejects a singleton one ulp below one"
        (case oneUlpDraw of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool "categorical rejects a clear deficit despite many trailing zeros"
        (case paddedDraw of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool "categorical rejects a clear deficit despite many tiny weights"
        (case tinyDraw of Left err -> "probabilities do not sum to 1" `isInfixOf` show err; _ -> False)
    , assertBool "categorical stops before a later deficit" (earlyDraw == Just 0)
    , assertBool "categorical does not force an unchosen tail" (lazyDraw == Just 0)
    , assertBool "categorical draws from a rounded normalized vector" (roundedDraw == Just 2)
    , assertBool "categorical accepts rounded decimal probabilities" (decimalDraw == Just 2)
    , assertBool "categorical accepts rounded decimal probabilities at exhaustion" (decimalFallback == Just 2)
    , assertBool ("categorical 49-weight compensated sum is 0.9999999999999999, got " ++ show equalTotal)
        (equalTotal == 0.9999999999999999)
    , assertBool "categorical selects index 48 for 49 rounded equal probabilities at 0.9999999999999999" (equalDraw == Just 48)
    , assertBool "categorical accepts negative zero before rounded probabilities" (negativeZeroDraw == Just 49)
    , pure allZero
    ]

dirichletStickRoundingChecks :: IO [Bool]
dirichletStickRoundingChecks = do
  let e = encodeFloat 1 (-54)
      tree = Tree 0.5 (branch (0.5 : replicate 8 e ++ repeat 0.5) : repeat (constantLabel 0.5))
      branch (u:us) = Tree 0.5 (stickTree u : branch us : repeat (constantLabel 0.5))
      branch [] = constantLabel 0.5
      stickTree u = Tree 0.5 (constantLabel u : repeat (constantLabel 0.5))
      restaurant = runProb (DP.newRestaurant 1) tree
      draw u = runProb (DP.newCustomer restaurant) (constantLabel u)
  atBoundary <- within 500000 (draw (0.75 + 2 * e))
  insideNext <- within 500000 (draw 0.8)
  beforeBoundary <- within 500000 (draw 0.75)
  sequence [assertBool ("Dirichlet process retains the small sticks before the boundary: " ++ show beforeBoundary ++ ", " ++ show atBoundary ++ ", " ++ show insideNext) (atBoundary == insideNext && atBoundary /= Nothing && atBoundary /= beforeBoundary && beforeBoundary /= Nothing)]

poissonBoundaryChecks :: IO [Bool]
poissonBoundaryChecks = do
  result <- within 500000 (runProb (poisson 1) (constantLabel (1 - encodeFloat 1 (-53))))
  low <- within 500000 (runProb (poisson 36.5) (constantLabel (encodeFloat 1 (-53))))
  sequence
    [ assertBool ("Poisson(1) at the largest label gives 17, got " ++ show result) (result == Just 17)
    , assertBool ("Poisson(36.5) at the smallest label gives 0, got " ++ show low) (low == Just 0)
    ]

-- Words 0 and 2^64-1 are the two ends of the generator's range. Every label of
-- the tree lies strictly between 0 and 1, so each inverse transform returns a
-- value in its support.
endpointChecks :: IO [Bool]
endpointChecks = concat <$> mapM check [("word 0", 0), ("word 2^64-1", maxBound)]
  where
    check (name, word) = do
      let tree = randomTree (ConstWord word)
          draw p = within 500000 (runProb p tree)
          label = runProb uniform tree
          finite x = not (isNaN x || isInfinite x)
      discrete <- draw (uniformdiscrete 1)
      sure <- draw (bernoulli 1)
      never <- draw (bernoulli 0)
      single <- draw (categorical [1])
      count <- draw (poisson 0)
      waiting <- draw (exponential 1)
      gaussian <- draw (normal 0 1)
      sequence
        [ assertBool (name ++ ": label " ++ show label ++ " lies in (0,1)") (label > 0 && label < 1)
        , assertBool (name ++ ": uniformdiscrete 1 gives 0, got " ++ show discrete) (discrete == Just 0)
        , assertBool (name ++ ": bernoulli 1 gives True, got " ++ show sure) (sure == Just True)
        , assertBool (name ++ ": bernoulli 0 gives False, got " ++ show never) (never == Just False)
        , assertBool (name ++ ": categorical [1] gives 0, got " ++ show single) (single == Just 0)
        , assertBool (name ++ ": poisson 0 gives 0, got " ++ show count) (count == Just 0)
        , assertBool (name ++ ": exponential 1 is finite, got " ++ show waiting) (maybe False finite waiting)
        , assertBool (name ++ ": normal 0 1 is finite, got " ++ show gaussian) (maybe False finite gaussian)
        ]

-- A log weight is finite, or negative infinity for weight 0. NaN and positive
-- infinity are not weights, and a sampler must not run on them.
logWeightChecks :: IO [Bool]
logWeightChecks = do
  let firstLogWeight model = do
        setStdGen (mkStdGen 1)
        draws <- mh 1 model
        evaluate (ln (getProduct (snd (head draws))))
      scoredBy scorer = do x <- sample uniform; scorer; pure x
      invalid = [("NaN", Exp (0/0)), ("+Infinity", Exp (1/0))]
  single <- sequence
    [ rejectsIO ("mh rejects scoreLog " ++ name) (firstLogWeight (scoredBy (scoreLog w))) | (name, w) <- invalid ]
  products <- sequence
    [ rejectsIO ("mh rejects scoreProductLog " ++ name) (firstLogWeight (scoredBy (scoreProductLog (Product w))))
    | (name, w) <- invalid ]
  overflow <- rejectsIO "mh rejects a total log weight that overflows to +Infinity"
    (firstLogWeight (scoredBy (scoreLog (Exp 1e308) >> scoreLog (Exp 1e308))))
  large <- firstLogWeight (scoredBy (scoreLog (Exp 1000)))
  zero <- firstLogWeight (scoredBy (scoreLog (Exp (-1/0))))
  valid <- sequence
    [ assertBool ("mh keeps a log weight of 1000, got " ++ show large) (large == 1000)
    , assertBool ("mh keeps weight 0 as log weight -Infinity, got " ++ show zero) (isInfinite zero && zero < 0)
    ]
  pure (single ++ products ++ [overflow] ++ valid)

-- The chances given to mh and mhirreducible are probabilities, and the
-- irreducible kernel needs a positive chance q of an all-sites change.
chanceChecks :: IO [Bool]
chanceChecks = do
  let firstValue sampler = do
        setStdGen (mkStdGen 1)
        draws <- sampler (sample uniform)
        evaluate (fst (head draws))
  invalid <- sequence
    [ rejectsIO (name ++ " is rejected") (firstValue sampler)
    | (name, sampler) <-
        [ ("mhirreducible 0 0", mhirreducible 0 0)
        , ("mhirreducible 0.5 NaN", mhirreducible 0.5 (0/0))
        , ("mhirreducible 0.5 1.5", mhirreducible 0.5 1.5)
        , ("mh NaN", mh (0/0))
        , ("mh -0.5", mh (-0.5))
        , ("mh 1.5", mh 1.5) ] ]
  setStdGen (mkStdGen 1)
  moving <- take 16 <$> mhirreducible 0 0.5 (sample uniform)
  valid <- assertBool "mhirreducible 0 0.5 moves through its all-sites changes"
    (length (nub (map fst moving)) > 1)
  pure (invalid ++ [valid])

betaLabelChecks :: IO [Bool]
betaLabelChecks = do
  let least = encodeFloat 1 (-1074)
      at u p = runProb p (constantLabel u)
      rejected name value = raisesBetaLabel value >>= assertBool (name ++ " raises BetaLabelError")
  sequence
    [ rejected "beta 2 2 at least subnormal" (at least (beta 2 2))
    , rejected "beta 1000 1000 at least subnormal" (at least (beta 1000 1000))
    , rejected "beta 2 2 at 1e-320" (at 1e-320 (beta 2 2))
    , rejected "dirichlet [2,2] at 1e-320" (sum (at 1e-320 (dirichlet [2,2])))
    , assertBool "beta 1 2 rounds the least-label quantile to zero" (at least (beta 1 2) == 0)
    , approxEq "beta 2 1 at the least label" (encodeFloat 1 (-537)) (at least (beta 2 1)) (encodeFloat 1 (-537) * 1e-12)
    ]

-- The Dirichlet process and IBP's newRestaurantS draw their sticks from
-- beta 1 alpha and beta alpha 1. They sample at every finite positive
-- concentration, 1e-4 and 1e4 among them, and raise an error that names the
-- function and alpha for any other.
concentrationChecks :: IO [Bool]
concentrationChecks = do
  let tree = randomTree (mkStdGen 7)
      samples name p = do
        r <- within 2000000 (let xs = runProb p tree in length (show xs) `seq` xs)
        assertBool (name ++ " samples") (maybe False (const True) r)
      namesItself function name x = do
        result <- try (timeout 2000000 (evaluate x))
        let message = either (\err -> takeWhile (/= '\n') (show (err :: ErrorCall))) (const "") result
        assertBool (name ++ " raises an error naming " ++ function ++ " and alpha"
                      ++ either (const (": " ++ message)) (const " (it returned a value or no answer)") result)
          (function `isPrefixOf` message && "alpha" `isInfixOf` message)
  sequence $
    concat [ [ samples ("DirichletP.newRestaurant " ++ show alpha ++ " with 20 customers")
                 (DP.newRestaurant alpha >>= \r -> replicateM 20 (DP.newCustomer r))
             , samples ("dp " ++ show alpha ++ " (normal 0 1), 20 draws")
                 (DP.dp alpha (normal 0 1) >>= \d -> replicateM 20 d)
             , samples ("IBP.newRestaurantS " ++ show alpha ++ ", the first 10 dishes of 3 customers")
                 (IBP.newRestaurantS alpha >>= \r -> replicateM 3 (take 10 <$> IBP.newCustomerS r)) ]
           | alpha <- [1e-4, 1e4] ] ++
    [ namesItself "newRestaurant" "DirichletP.newRestaurant 0" (runProb (DP.newRestaurant 0 >>= DP.newCustomer) tree)
    , namesItself "dp" "dp (-1) (normal 0 1)" (runProb (DP.dp (-1) (normal 0 1) >>= id) tree)
    , namesItself "newRestaurantS" "IBP.newRestaurantS NaN" (runProb (IBP.newRestaurantS (0 / 0) >>= IBP.newCustomerS) tree)
    , namesItself "dp" "dp Infinity (normal 0 1)" (runProb (DP.dp (1 / 0) (normal 0 1) >>= id) tree) ]

-- Each distribution checks its parameters, so an invalid parameter raises an
-- error instead of silently changing the law; samplers and densities agree.
parameterChecks :: IO [Bool]
parameterChecks = do
  let tree = randomTree (mkStdGen 1)
      sampled :: Prob a -> a
      sampled p = runProb p tree
  sequence
    [ rejects "bernoulli 2" (sampled (bernoulli 2))
    , rejects "bernoulli -0.5" (sampled (bernoulli (-0.5)))
    , rejects "exponential -1" (sampled (exponential (-1)))
    , rejects "exponential 0" (sampled (exponential 0))
    , rejects "gamma 1 -1" (sampled (gamma 1 (-1)))
    , rejects "gamma -1 1" (sampled (gamma (-1) 1))
    , rejects "beta 0 1" (sampled (beta 0 1))
      -- Outside the supported domain of beta and dirichlet: large shapes, and
      -- concentrations whose sum overflows.
    , rejects "beta 1e16 1e16" (sampled (beta 1e16 1e16))
    , rejects "dirichlet [1e308,1e308,1e308]" (sampled (dirichlet [1e308, 1e308, 1e308]))
    , rejects "normal 0 -1" (sampled (normal 0 (-1)))
    , rejects "normal 0 0" (sampled (normal 0 0))
    , rejects "poisson -1" (sampled (poisson (-1)))
    , rejects "uniformdiscrete 0" (sampled (uniformdiscrete 0))
    , rejects "uniformbounded 1 0" (sampled (uniformbounded 1 0))
    , rejects "categorical [-1,2]" (sampled (categorical [-1, 2]))
    , rejects "categorical [2,-1]" (sampled (categorical [2, -1]))
    , rejects "normalLogPdf 0 -1 0" (normalLogPdf 0 (-1) 0)
    , rejects "logNormalLogPdf 0 0 1" (logNormalLogPdf 0 0 1)
    , rejects "gammaLogPdf -1 1 1" (gammaLogPdf (-1) 1 1)
    , rejects "betaLogPdf 0 1 0.5" (betaLogPdf 0 1 0.5)
    , rejects "betaLogPdf 1e308 1e308 0.5" (betaLogPdf 1e308 1e308 0.5)
    , rejects "dirichletLogPdf [1e308,1e308] [0.5,0.5]" (dirichletLogPdf [1e308, 1e308] [0.5, 0.5])
    , rejects "uniformBoundedLogPdf 1 0 0.5" (uniformBoundedLogPdf 1 0 0.5)
    ]
