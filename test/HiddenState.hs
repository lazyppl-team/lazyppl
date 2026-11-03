module Main where

-- The documented use of the values with hidden state that runProb returns:
-- counters, IBP restaurants, generalmemoize functions, and the random functions
-- of wiener and gp. Each check binds the value with evaluate before an IO action
-- that runs once per query, and requires the answers that the same queries give
-- inside one Prob computation on the same tree. This module takes the compiler's
-- default options; hidden-state-o2-test compiles it at -O2.

import Control.Exception (evaluate)
import Control.Monad (replicateM, unless)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (nub)
import LazyPPL (Prob, Tree(..), randomTree, runProb, uniform)
import LazyPPL.Distributions.Counter (newCounter, readAndIncrement)
import LazyPPL.Distributions.GP (gp, wiener)
import LazyPPL.Distributions.IBP (Dish, newCustomer, newRestaurant)
import LazyPPL.Distributions.Memoization (generalmemoize)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (mkStdGen)
import qualified KnownLimit

-- A bind in Prob runs its first computation on the tree's first child.
firstChild :: Tree -> Tree
firstChild (Tree _ (t : _)) = t
firstChild (Tree _ []) = error "a random tree has children"

-- The answers of one IO action that runs once per key: it takes the next key
-- and queries at it.
queried :: (k -> IO a) -> [k] -> IO [a]
queried query keys = do
  remaining <- newIORef keys
  replicateM (length keys) $ do
    key <- atomicModifyIORef' remaining (\ks -> (drop 1 ks, head ks))
    query key

report :: String -> Bool -> String -> IO Bool
report name ok detail = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name ++ ": " ++ detail)
  pure ok

-- Inside one Prob computation, the counter gives 0, 1, 2, ... in the order of
-- the reads; read by an IO action after evaluate, it gives the same numbers.
counterCheck :: IO [Bool]
counterCheck = do
  let tree = randomTree (mkStdGen 9)
      n = 2000
  inside <- mapM evaluate (runProb (do c <- newCounter; replicateM n (readAndIncrement c)) tree)
  counter <- evaluate (runProb newCounter (firstChild tree))
  outside <- queried (\k -> evaluate (runProb (readAndIncrement counter) (randomTree (mkStdGen k)))) [1 .. n]
  sequence
    [ report "counter read inside one Prob computation" (inside == [0 .. n - 1]) ("first reads " ++ show (take 5 inside))
    , report "counter evaluated once, then read by an IO action" (outside == [0 .. n - 1]) ("first reads " ++ show (take 5 outside))
    ]

-- Six customers of one IBP restaurant take the first six rows of its matrix.
restaurantCheck :: IO [Bool]
restaurantCheck = do
  let tree = randomTree (mkStdGen 9)
      dishes = map show :: [Dish] -> [String]
  inside <- mapM (evaluate . dishes) (runProb (do r <- newRestaurant 2; replicateM 6 (newCustomer r)) tree)
  restaurant <- evaluate (runProb (newRestaurant 2) (firstChild tree))
  outside <- queried (\k -> evaluate (dishes (runProb (newCustomer restaurant) (randomTree (mkStdGen k))))) [1 .. 6 :: Int]
  sequence
    [ report "restaurant customers inside one Prob computation differ" (length (nub inside) > 1) (show inside)
    , report "restaurant evaluated once, then visited by an IO action" (outside == inside) (show outside)
    ]

-- A memoized, a Wiener and a Gaussian-process function queried at keys with
-- a repeat. Inside one Prob computation, a repeated key gives its first answer
-- and new keys give new answers; queried by an IO action after evaluate, the
-- function gives the same answers.
functionCheck :: String -> Prob (Double -> Double) -> Int -> IO [Bool]
functionCheck name process seed = do
  let tree = randomTree (mkStdGen seed)
      keys = [1, 2, 1, 3, 2]
  inside <- mapM evaluate (runProb (do f <- process; pure (map f keys)) tree)
  f <- evaluate (runProb process (firstChild tree))
  outside <- queried (evaluate . f) keys
  sequence
    [ report (name ++ " queried inside one Prob computation") (length (nub inside) == 3 && inside !! 2 == head inside)
        ("at " ++ show keys ++ ": " ++ show inside)
    , report (name ++ " evaluated once, then queried by an IO action") (outside == inside)
        ("at " ++ show keys ++ ": " ++ show outside)
    ]

main :: IO ()
main = do
  args <- getArgs
  results <- case args of
    ["known-limit"] -> KnownLimit.checks
    _ -> concat <$> sequence
      [ counterCheck
      , restaurantCheck
      , functionCheck "generalmemoize" (generalmemoize (const uniform)) 10
      , functionCheck "wiener" wiener 11
      , functionCheck "white-noise gp" (gp (\x y -> if x == y then 1 else 0)) 12
      ]
  putStrLn (show (length (filter id results)) ++ "/" ++ show (length results) ++ " checks passed.")
  unless (and results) exitFailure
