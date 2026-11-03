module Main where

import Control.Monad (unless)
import LazyPPL (sample)
import LazyPPL.SingleSite (mh1)
import LazyPPL.Distributions (normal)
import System.Random (mkStdGen, setStdGen)

main :: IO ()
main = do
  setStdGen (mkStdGen 42)
  results <- mh1 (sample (normal 0 1))
  let draws = take 100 (map fst results)
  unless (length draws == 100 && all (\x -> not (isNaN x || isInfinite x)) draws
          && any (<0) draws && any (>0) draws) $
    fail "Single-site normal sampling did not produce finite values of both signs"
  putStrLn "PASS: single-site normal sampling decodes the target's Double heap payload"
