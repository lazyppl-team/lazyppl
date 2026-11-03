module Main where

import Control.Exception (evaluate)
import Control.Monad (unless)
import LazyPPL (Tree(..), mh, mhirreducible, runProb)
import LazyPPL.Distributions (categorical)
import System.Environment (getArgs)
import System.Exit (exitFailure)

check :: String -> Bool -> IO ()
check name result = do
  putStrLn $ (if result then "PASS " else "FAIL ") ++ name
  unless result exitFailure

categoricalRetention :: Bool -> IO ()
categoricalRetention infinite = do
  let n = 8000000
      remaining = if infinite then 1 : repeat 0 else [1]
      weights = replicate n 0 ++ remaining
      label = let t = Tree 0.5 (repeat t) in t
  selected <- evaluate (runProb (categorical weights) label)
  check (if infinite then "categorical infinite stream fits the 256 MiB heap budget"
                     else "categorical finite stream fits the 256 MiB heap budget")
    (selected == n)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["categorical-finite"] -> categoricalRetention False
    ["categorical-infinite"] -> categoricalRetention True
    [] -> streamChecks >> categoricalRetention False >> categoricalRetention True
    _ -> error "unknown stream test"

streamChecks :: IO ()
streamChecks = do
  lazyDraws <- mh 0.1 (pure (repeat True))
  check "infinite sample payload remains lazy"
    (take 3 (fst (head lazyDraws)) == [True, True, True])
  lazyIrreducibleDraws <- mhirreducible 0.1 1 (pure (repeat True))
  check "irreducible infinite sample payload remains lazy"
    (take 3 (fst (head lazyIrreducibleDraws)) == [True, True, True])
  draws <- mhirreducible 0.2 1 (pure True)
  result <- evaluate (fst (head (drop 4000000 draws)))
  check "discarding a long chain preserves the constant model" result
  partialDraws <- mh 0.2 (pure True)
  -- Four million accepted mutations require about 61 MiB of packed seeds.
  -- The RTS budget also allows collector growth and the sampler's live tree.
  partialResult <- evaluate (fst (head (drop 4000000 partialDraws)))
  check "partial-mutation seed records fit the 256 MiB heap budget" partialResult
