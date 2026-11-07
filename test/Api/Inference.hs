{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Api.Inference where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import Data.Monoid (Product(..), getProduct)
import Numeric.Log (Log(..), ln)
import LazyPPL.Distributions
import System.Random (RandomGen(..), SplitGen(..), mkStdGen, setStdGen)
import Control.Monad (replicateM)

import Api.Support

smokeMhWithinUnitInterval :: IO Bool
smokeMhWithinUnitInterval = do
  draws <- take 25 <$> mh 0.5 (sample uniform)
  pure $ all (\(x, _) -> x >= 0 && x <= 1) draws

smokeMhIrreducibleWithinUnitInterval :: IO Bool
smokeMhIrreducibleWithinUnitInterval = do
  draws <- take 25 <$> mhirreducible 0.5 0.1 (sample uniform)
  pure $ all (\(x, _) -> x >= 0 && x <= 1) draws

wisDistinctResamples :: IO Double
wisDistinctResamples = do
  draws <- take 2000 <$> wis 4 (sample uniform)
  pure $ fromIntegral (length (nub draws))

smokeWisWithinUnitInterval :: IO Bool
smokeWisWithinUnitInterval = do
  draws <- take 25 <$> wis 50 (sample uniform)
  pure $ all (\x -> x >= 0 && x <= 1) draws

-- Only the all-true sequence has weight (prior mass 1/1024). From weight 0 the chain accepts
-- every move until it lands there, at sample index 408 with seed 1.
zeroAcceptanceChecks :: IO [Bool]
zeroAcceptanceChecks = do
  let constrainedCoins = do
        coins <- sample (replicateM 10 (bernoulli 0.5))
        score (if and coins then 1 else 0)
        pure coins
      impossible = do x <- sample uniform; score 0; pure x
  setStdGen (mkStdGen 1)
  draws <- mh 0.5 constrainedCoins
  escaped <- assertBool "mh reaches the support from a zero-weight initialization within 1024 transitions"
    (any (and . fst) (take 1024 draws))
  retained <- assertBool "mh rejects impossible proposals after entering the support"
    (all (and . fst) (dropWhile (not . and . fst) (take 1024 draws)))
  moving <- mapM (\(name, sampler) -> do
      setStdGen (mkStdGen 1)
      states <- take 16 <$> sampler impossible
      assertBool (name ++ " accepts zero-to-zero initialization moves")
        (length (nub (map fst states)) > 1 && all ((== 0) . getProduct . snd) states))
    [("mh", mh 0.5), ("mhirreducible", mhirreducible 0.5 0.1)]
  setStdGen (mkStdGen 1)
  zeroBatch <- try (wis 16 impossible >>= evaluate . head) :: IO (Either ErrorCall Double)
  noZeroNormalization <- assertBool "wis rejects an all-zero empirical measure"
    (either (const True) (const False) zeroBatch)
  setStdGen (mkStdGen 1)
  positive <- take 32 <$> wis 256 (do
    x <- sample uniform
    score (if x > 0.5 then 1 else 0)
    pure x)
  positiveSupport <- assertBool "wis returns supported samples from a batch containing zero weights"
    (all (>0.5) positive)
  emptyBatch <- try (wis 0 (pure ()) >>= evaluate . take 1) :: IO (Either ErrorCall [()])
  emptyRejected <- assertBool "wis rejects an empty empirical measure"
    (either (const True) (const False) emptyBatch)
  let base = do x <- sample uniform; scoreLog (Exp 0); pure x
  setStdGen (mkStdGen 29)
  [(target, _)] <- take 1 <$> weightedsamples base
  let oneModel = do
        x <- sample uniform
        scoreLog (Exp (if x == target then 0 else -1 / 0))
        pure x
  setStdGen (mkStdGen 29)
  oneParticles <- take 64 <$> weightedsamples oneModel
  setStdGen (mkStdGen 29)
  oneResamples <- take 2000 <$> wis 64 oneModel
  onePositive <- assertBool "the batch has one positive particle and only it is resampled"
    (length (filter ((> 0) . snd) oneParticles) == 1 && all (== target) oneResamples)
  let sepModel = do
        x <- sample (bernoulli 0.5)
        scoreLog (Exp (if x then -1000 else 0))
        pure x
  setStdGen (mkStdGen 24)
  sepResamples <- take 2000 <$> wis 64 sepModel
  separated <- assertBool "negligible-weight particles are not sampled" (all not sepResamples)
  pure (escaped : retained : moving ++ [noZeroNormalization, positiveSupport, emptyRejected, onePositive, separated])

-- A fair coin b. On true, a uniform u is read and scored 6u, which integrates
-- to 3; on false, the score is 1. So P(b) = 3/4 exactly, and the site u exists
-- only on the true branch.
coinModel :: Meas Bool
coinModel = do
  b <- sample (bernoulli 0.5)
  if b then sample uniform >>= \u -> score (6 * u) else score 1
  pure b

-- The fraction of true draws is within five standard errors of 3/4. The
-- standard error comes from the means of 40 consecutive batches, and it must be
-- below 0.01 for the check to say anything.
posteriorFraction :: String -> [Bool] -> IO Bool
posteriorFraction name draws = do
  let n = length draws
      batches = 40
      size = n `div` batches
      batchMean k = fromIntegral (length (filter id (take size (drop (k * size) draws)))) / fromIntegral size :: Double
      means = map batchMean [0 .. batches - 1]
      p = sum means / fromIntegral batches
      se = sqrt (sum [(m - p) ^ (2 :: Int) | m <- means] / fromIntegral (batches - 1) / fromIntegral batches)
      ok = se < 0.01 && abs (p - 0.75) < 5 * se
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ name ++ ": P(true) = " ++ show p ++ ", standard error " ++ show se ++ ", exact 3/4"
  pure ok

-- wis n resamples from its n weighted particles, so each particle's share of
-- the resamples is its weight over the total. With the same global seed,
-- weightedsamples gives wis's particles, and the weights here are 1 and 3.
wisResamplingCheck :: IO Bool
wisResamplingCheck = do
  let model = do x <- sample uniform; score (if x < 0.5 then 1 else 3); pure x
      n = 5
      draws = 20000 :: Int
  setStdGen (mkStdGen 24)
  particles <- take n <$> weightedsamples model
  setStdGen (mkStdGen 24)
  resamples <- take draws <$> wis n model
  let total = sum [exp (ln w) | (_, w) <- particles]
      shares = [ (fromIntegral (length (filter (== x) resamples)) / fromIntegral draws, exp (ln w) / total)
               | (x, w) <- particles ]
      within5 (observed, expected) = abs (observed - expected) <= 5 * sqrt (expected * (1 - expected) / fromIntegral draws)
  assertBool ("wis 5 resamples each particle in proportion to its weight, (observed, expected) = " ++ show shares)
    (all within5 shares && length (filter (`elem` map fst particles) resamples) == draws)

-- Resampling in log space: equal weights share the resamples equally no
-- matter how large the log weights are. With log weight 1e308 the cumulative
-- sums of the weights all round to the maximum, so a resampler that adds the
-- weights always picks the first particle; this regression check observes the
-- share of one coin instead.
wisLogSpaceChecks :: IO [Bool]
wisLogSpaceChecks = do
  let equalModel = do
        x <- sample (bernoulli 0.5)
        scoreLog (Exp 1e308)
        pure x
      n = 64
      draws = 4000
  setStdGen (mkStdGen 29)
  equalParticles <- take n <$> weightedsamples equalModel
  setStdGen (mkStdGen 29)
  equalResamples <- take draws <$> wis n equalModel
  let expected = fromIntegral (length (filter fst equalParticles)) / fromIntegral n :: Double
      observed = fromIntegral (length (filter id equalResamples)) / fromIntegral draws
  equalLarge <- assertBool ("equal-weight wis share, expected " ++ show expected ++ ", got " ++ show observed)
    (expected > 0 && expected < 1 && abs (observed - expected) < 0.05)
  pure [equalLarge]

posteriorChecks :: IO [Bool]
posteriorChecks = do
  setStdGen (mkStdGen 21)
  chain <- map fst . take 40000 . drop 1000 <$> mh 0.5 coinModel
  setStdGen (mkStdGen 22)
  irreducible <- map fst . take 40000 . drop 1000 <$> mhirreducible 0.5 0.1 coinModel
  logSpace <- wisLogSpaceChecks
  rest <- sequence [posteriorFraction "mh 0.5 coin posterior" chain, posteriorFraction "mhirreducible 0.5 0.1 coin posterior" irreducible, wisResamplingCheck]
  pure (logSpace ++ rest)
