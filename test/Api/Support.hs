{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Api.Support where

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Data.List (isInfixOf, isPrefixOf, nub, sort, tails)
import LazyPPL (Meas(..), Prob, Tree(..), mh, mhirreducible, randomTree, runProb, sample, score, scoreLog, scoreProductLog, uniform, weightedsamples, wis)
import Numeric.MathFunctions.Comparison (addUlps)
import System.Timeout (timeout)
import LazyPPL.Distributions

approxEq :: String -> Double -> Double -> Double -> IO Bool
approxEq name expected actual tol = do
  let ok = abs (expected - actual) < tol
  putStrLn $
    (if ok then "PASS" else "FAIL")
      ++ " "
      ++ name
      ++ " expected="
      ++ show expected
      ++ " actual="
      ++ show actual
  pure ok

assertBool :: String -> Bool -> IO Bool
assertBool name ok = do
  putStrLn $ (if ok then "PASS" else "FAIL") ++ " " ++ name
  pure ok

-- Evaluates a value with a time limit. An exception or a timeout gives Nothing.
within :: Int -> a -> IO (Maybe a)
within micros x = do
  result <- try (timeout micros (evaluate x))
  pure $ case result of
    Left err -> const Nothing (err :: SomeException)
    Right value -> value

-- Passes when evaluating the value raises an error call.
rejects :: String -> a -> IO Bool
rejects name x = do
  result <- try (timeout 500000 (evaluate x))
  assertBool (name ++ " is rejected" ++ case result of
      Left err -> const "" (err :: ErrorCall)
      Right Nothing -> " (no answer within 0.5 s)"
      Right (Just _) -> " (it returned a value)")
    (either (const True) (const False) result)

-- Passes when running the action raises an error call.
rejectsIO :: String -> IO Double -> IO Bool
rejectsIO name action = do
  result <- try action
  assertBool (name ++ case result of
      Left err -> const "" (err :: ErrorCall)
      Right value -> " (it returned " ++ show value ++ ")")
    (either (const True) (const False) result)

-- The tree whose every label is u.
constantLabel :: Double -> Tree
constantLabel u = let t = Tree u (repeat t) in t

-- The largest distance between the empirical distribution function of the
-- sample and cdf, with an allowance for rounding: at a draw x the exact function
-- is taken at x (at the least positive double for a draw that underflowed to
-- 0), and just below x at the double before x. A draw is a rounded double, so
-- equal draws can be an atom: Beta(0.001,1) puts 0.475 below the least positive
-- double, which all round to 0. statistics-0.16.5.0's kolmogorovSmirnovCdfD
-- computes the statistic for a continuous law, without such atoms.
ksDistance :: (Double -> Double) -> [Double] -> Double
ksDistance cdf xs = maximum
  [ max (i / n - cdf (if x == 0 then encodeFloat 1 (-1074) else x)) (cdf (addUlps (-1) x) - (i - 1) / n)
  | (i, x) <- zip [1 ..] (sort xs) ]
  where n = fromIntegral (length xs)

-- The rows of a table that test/reference/reference.py wrote with mpmath, each
-- split at its tabs; lines that start with # are comments. Cabal runs the test
-- in the package root, where the path below starts.
referenceTable :: FilePath -> IO [[String]]
referenceTable name = do
  text <- readFile ("test/reference/" ++ name)
  pure [cells line | line <- lines text, not (null line), take 1 line /= "#"]
  where
    cells s = case break (== '\t') s of
      (cell, []) -> [cell]
      (cell, _ : rest) -> cell : cells rest

-- Prints the failures of a table check and one summary line.
tableReport :: String -> [(Bool, String)] -> IO [Bool]
tableReport name results = do
  mapM_ (\(_, detail) -> putStrLn ("FAIL " ++ detail)) (filter (not . fst) results)
  let passed = length (filter fst results)
  putStrLn ((if passed == length results then "PASS " else "FAIL ") ++ name ++ ": "
            ++ show passed ++ " of " ++ show (length results) ++ " rows")
  pure (map fst results)

-- Whether evaluating the value raises an error call within 2 s.
raisesError :: a -> IO Bool
raisesError x = do
  result <- try (timeout 2000000 (evaluate x))
  pure (either (\err -> const True (err :: ErrorCall)) (const False) result)

raisesBetaLabel :: a -> IO Bool
raisesBetaLabel x = do
  result <- try (timeout 2000000 (evaluate x))
  pure (check result)
  where check :: Either BetaLabelError b -> Bool
        check = either (const True) (const False)
