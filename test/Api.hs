{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Main where

import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import StrictMemo (strictMemoChecks)
import LazyPPL.Distributions
import LazyPPL.Distributions.GP (gp, wiener)
import LazyPPL.Distributions.IBP (ibp)
import LazyPPL.Distributions.Memoization (memoize, memrec)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure, exitSuccess)

import Api.Support
import Api.Domains
import Api.Distributions
import Api.Processes
import Api.Inference

-- Accuracy of samplers, densities and processes at extreme parameters.
accuracyChecks :: IO [Bool]
accuracyChecks = concat <$> sequence [dirichletSamplingChecks, lawChecks, gridLawChecks, betaQuantileChecks False, dirichletStickChecks, densityChecks, wienerBridgeChecks, numericChecks, processChecks, strictMemoChecks]

-- Sampler and density domains: tree labels, log weights, chances, distribution
-- parameters and the concentrations of the nonparametric priors.
domainChecks :: IO [Bool]
domainChecks = concat <$> sequence [endpointChecks, logWeightChecks, chanceChecks, parameterChecks, betaLabelChecks, concentrationChecks]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["categorical-boundary"] -> categoricalBoundaryChecks >>= finish
    ["categorical-properties"] -> categoricalPropertyChecks >>= finish
    ["dirichlet-stick-rounding"] -> dirichletStickRoundingChecks >>= finish
    ["poisson-boundary"] -> poissonBoundaryChecks >>= finish
    ["memo"] -> memoStochasticChecks >>= finish
    ["mondrian"] -> mondrianChecks >>= finish
    ["zero-weight"] -> zeroAcceptanceChecks >>= finish
    ["domains"] -> domainChecks >>= finish
    ["accuracy"] -> accuracyChecks >>= finish
    ["laws"] -> concat <$> sequence [dirichletSamplingChecks, lawChecks, gridLawChecks, betaQuantileChecks False, dirichletStickChecks] >>= finish
    ["quantiles"] -> betaQuantileChecks False >>= finish
    ["quantiles-inside"] -> betaQuantileChecks True >>= finish
    ["sticks"] -> dirichletStickChecks >>= finish
    ["grid-laws"] -> gridLawChecks >>= finish
    ["densities"] -> densityChecks >>= finish
    ["density-rows"] -> densityRowsChild
    ["wiener"] -> concat <$> sequence [wienerBridgeChecks, processChecks] >>= finish
    ["posterior"] -> posteriorChecks >>= finish
    _ -> do
      additional <- concat <$> sequence [categoricalBoundaryChecks, categoricalPropertyChecks, dirichletStickRoundingChecks, poissonBoundaryChecks, densityBoundaryChecks, lazyCategoricalChecks, memoStochasticChecks, mondrianChecks, recursiveMemoCheck, dirichletProcessChecks, zeroAcceptanceChecks, domainChecks, accuracyChecks, posteriorChecks]
      if args == ["additional"] then finish additional else existingChecks additional

finish :: [Bool] -> IO ()
finish results = do
  let passed = length (filter id results)
      total = length results
  putStrLn $ show passed ++ "/" ++ show total ++ " checks passed."
  if passed == total then exitSuccess else exitFailure

existingChecks :: [Bool] -> IO ()
existingChecks additional = do
  results <-
    sequence
      [ approxEq "normalPdf 0 1 0" 0.3989422804 (normalPdf 0 1 0) 1e-9
      , approxEq "normalPdf 0 1 1" 0.2419707245 (normalPdf 0 1 1) 1e-9
      , approxEq "expPdf 2 0.5" (2 * exp (-1)) (expPdf 2 0.5) 1e-9
      , approxEq "poissonPdf 3 2" (4.5 * exp (-3)) (poissonPdf 3 2) 1e-9
      , approxEq "poissonPdf 1 0" (exp (-1)) (poissonPdf 1 0) 1e-9
      , -- Six standard errors of the sampling distribution of the mean of 5000 draws.
        approxEq "normal 3 2 sample mean" 3 (fst normalSampleMoments) (6 * 2 / sqrt 5000)
      , -- Six standard deviations of the sampling distribution of the variance.
        approxEq "normal 3 2 sample variance" 4 (snd normalSampleMoments) (6 * sqrt (2 * 4 * 4 / 4999))
      , assertBool "mh samples stay in [0,1]" =<< smokeMhWithinUnitInterval
      , assertBool "mhirreducible samples stay in [0,1]" =<< smokeMhIrreducibleWithinUnitInterval
      , assertBool "wis samples stay in [0,1]" =<< smokeWisWithinUnitInterval
      , (\n -> approxEq "wis 4 resamples from all four samples" 4 n 0.5) =<< wisDistinctResamples
      , approxEq "memoize returns f n at n for n in 0..40 (mismatches)" 0 memoizeMismatches 0.5
      , approxEq "gp with cov 0 0 = 1 has sd 1 at 0" 1 gpStandardDeviationAtZero 0.15
      , (\d -> approxEq "gp constant covariance 1e8: gap within six nugget standard deviations" 0 d (6*sqrt (2*2**(-33)))) =<< gpConstantKernelGap 1e8
      , (\d -> approxEq "gp constant covariance 1e20: gap within six nugget standard deviations" 0 d (6*sqrt (2*2**(-33)))) =<< gpConstantKernelGap 1e20
      , (\d -> approxEq "gp with an RBF kernel scaled by 1e20 stays finite at 31 points" 0 d 0.5) =<< gpScaledKernelFinite 1e20
      , (\d -> approxEq "gp rank-two covariance 1e16: line residual within six nugget standard deviations" 0 d (6*sqrt (92*2**(-33)))) =<< gpRankTwoLineResidual 1e16
      , (\d -> approxEq "gp rank-two covariance 1e100: line residual within six nugget standard deviations" 0 d (6*sqrt (92*2**(-33)))) =<< gpRankTwoLineResidual 1e100
      , (\d -> approxEq "gp with covariance 1e100 cos (x - y) stays finite at 0..5" 0 d 0.5) =<< gpCosineKernelFinite 1e100
      , (\d -> approxEq "gp scale equivariance for four kernels at 1e-300 to 1e300" 0 d 1e-6) =<< gpScaleEquivarianceError
      , (\d -> assertBool "gp admits the smallest positive first-point variance" (d > 0 && not (isNaN d || isInfinite d))) =<< gpSmallestVarianceSample
      , approxEq "ibp 1 first row empty with probability exp (-1)" (exp (-1)) ibpFirstRowEmptyFraction 0.05
      , assertBool "gp samples stay finite" =<< smokeGpFinite
      ]
  finish (additional ++ results)
