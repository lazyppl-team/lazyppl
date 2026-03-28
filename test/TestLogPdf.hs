module Main where

import LazyPPL.Distributions
import System.Exit (exitFailure, exitSuccess)

approxEq :: String -> Double -> Double -> Double -> IO Bool
approxEq name expected actual tol = do
  let ok = abs (expected - actual) < tol
  if ok
    then putStrLn $ "  PASS: " ++ name
    else putStrLn $ "  FAIL: " ++ name ++ " expected=" ++ show expected ++ " actual=" ++ show actual
  return ok

main :: IO ()
main = do
  putStrLn "Testing log-PDF/PMF functions..."
  results <- sequence
    [ approxEq "normalLogPdf 0 1 0" (-0.9189385) (normalLogPdf 0 1 0) 1e-4
    , approxEq "normalLogPdf 0 1 1" (-1.4189385) (normalLogPdf 0 1 1) 1e-4
    , approxEq "exponentialLogPdf 1 1" (-1.0) (exponentialLogPdf 1 1) 1e-6
    , approxEq "exponentialLogPdf 2 0.5" (log 2 - 1.0) (exponentialLogPdf 2 0.5) 1e-6
      -- (k-1)*log(1) - 1/1 - 2*log(1) - logGamma(2) = -1
    , approxEq "gammaLogPdf 2 1 1" (-1.0) (gammaLogPdf 2 1 1) 1e-6
    , approxEq "betaLogPdf 1 1 0.5" 0.0 (betaLogPdf 1 1 0.5) 1e-6
      -- B(2,2) = 1/6, pdf at 0.5 = 6*0.25 = 1.5
    , approxEq "betaLogPdf 2 2 0.5" (log 1.5) (betaLogPdf 2 2 0.5) 1e-6
    , approxEq "bernoulliLogPmf 0.3 True" (log 0.3) (bernoulliLogPmf 0.3 True) 1e-6
    , approxEq "bernoulliLogPmf 0.3 False" (log 0.7) (bernoulliLogPmf 0.3 False) 1e-6
    , approxEq "categoricalLogPmf [0.2,0.3,0.5] 2" (log 0.5) (categoricalLogPmf [0.2, 0.3, 0.5] 2) 1e-6
    , approxEq "categoricalLogPmf OOB" (-1e300) (categoricalLogPmf [0.5, 0.5] 3) 1e-6
    , approxEq "uniformBoundedLogPdf 0 10 5" (-log 10) (uniformBoundedLogPdf 0 10 5) 1e-6
    , approxEq "uniformBoundedLogPdf OOB" (-1e300) (uniformBoundedLogPdf 0 1 2) 1e-6
      -- Dirichlet(1,1,1) normalizer = Gamma(3)/(Gamma(1)^3) = 2
    , approxEq "dirichletLogPdf [1,1,1] uniform" (log 2) (dirichletLogPdf [1,1,1] [1/3, 1/3, 1/3]) 1e-4
    , approxEq "logNormalLogPdf 0 1 1" (-0.9189385) (logNormalLogPdf 0 1 1) 1e-4
    , approxEq "logNormalLogPdf negative" (-1e300) (logNormalLogPdf 0 1 (-1)) 1e-6
      -- C(10,5) = 252
    , approxEq "binomialLogPmf 10 0.5 5" (log 252 - 10 * log 2) (binomialLogPmf 10 0.5 5) 1e-4
    , approxEq "binomialLogPmf OOB" (-1e300) (binomialLogPmf 5 0.3 6) 1e-6
    , approxEq "poissonLogPmf 3 2" (2 * log 3 - 3 - log 2) (poissonLogPmf 3 2) 1e-6
      -- consistency with existing Pdf functions
    , approxEq "normalLogPdf vs normalPdf" (log (normalPdf 2.5 1.3 3.0)) (normalLogPdf 2.5 1.3 3.0) 1e-10
    , approxEq "expLogPdf vs expPdf" (log (expPdf 2.0 0.5)) (exponentialLogPdf 2.0 0.5) 1e-10
    , approxEq "poissonLogPmf vs poissonPdf" (log (poissonPdf 4.0 3)) (poissonLogPmf 4.0 3) 1e-10
    ]
  let nPass  = length (filter id results)
      nTotal = length results
  putStrLn $ show nPass ++ "/" ++ show nTotal ++ " tests passed."
  if nPass == nTotal then exitSuccess else exitFailure
