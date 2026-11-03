{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- The known limit of the values with hidden state that runProb returns: bound
-- by let, or returned inside a function from a Prob computation, and then used
-- by an IO action that runs more than once, such a value can lose its state.
-- GHC treats an IO action's lambda as entered once (the state hack), so it may
-- copy the binding into the action, and every run starts from new state. The
-- options above are those of the concurrency test, where the copy happens.
-- These checks fail while the limit holds; hidden-state-test runs them with
-- the argument known-limit, and its default run does not.
module KnownLimit (checks) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forM, replicateM)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import LazyPPL (randomTree, runProb)
import LazyPPL.Distributions.Counter (newCounter, readAndIncrement)
import LazyPPL.Distributions.GP (wiener)
import System.Random (mkStdGen)

-- Runs the action the given number of times in each of two threads at once.
twoThreads :: Int -> IO a -> IO [a]
twoThreads runs action = do
  slots <- forM [1, 2 :: Int] $ \_ -> do
    slot <- newEmptyMVar
    _ <- forkFinally (replicateM runs (action >>= evaluate)) (putMVar slot)
    pure slot
  concat <$> mapM (\slot -> takeMVar slot >>= either throwIO pure) slots

report :: String -> Bool -> String -> IO Bool
report name ok detail = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ "known limit, " ++ name ++ ": " ++ detail)
  pure ok

-- A counter bound by let, read 20000 times by each of two threads.
counterLet :: IO Bool
counterLet = do
  let counter = runProb newCounter (randomTree (mkStdGen 9))
      next k = runProb (readAndIncrement counter) (randomTree (mkStdGen k))
  seeds <- newIORef (0 :: Int)
  numbers <- twoThreads 20000 $ do
    k <- atomicModifyIORef' seeds (\s -> (s + 1, s))
    evaluate (next k)
  let distinct = Set.size (Set.fromList numbers)
  report "counter bound by let" (distinct == length numbers)
    (show distinct ++ " distinct numbers from " ++ show (length numbers) ++ " reads")

-- A function that reads a counter, returned from a Prob computation and bound by let.
counterReturned :: IO Bool
counterReturned = do
  let next = runProb (do c <- newCounter
                         pure (\k -> runProb (readAndIncrement c) (randomTree (mkStdGen k))))
                     (randomTree (mkStdGen 9))
  seeds <- newIORef (0 :: Int)
  numbers <- twoThreads 20000 $ do
    k <- atomicModifyIORef' seeds (\s -> (s + 1, s))
    evaluate (next k)
  let distinct = Set.size (Set.fromList numbers)
  report "counter inside a function returned from Prob" (distinct == length numbers)
    (show distinct ++ " distinct numbers from " ++ show (length numbers) ++ " reads")

-- A Wiener function bound by let and queried at 1, then at 2, by two runs of one
-- action. With one table, W(2) is drawn given W(1), and W(1) and W(2)/sqrt 2 differ.
wienerLet :: IO Bool
wienerLet = do
  let f = runProb wiener (randomTree (mkStdGen 9))
  times <- newIORef [1, 2 :: Double]
  values <- replicateM 2 $ do
    t <- atomicModifyIORef' times (\ts -> (drop 1 ts, head ts))
    v <- evaluate (f t)
    pure (v / sqrt t)
  report "wiener bound by let" (values !! 0 /= values !! 1) ("W(1), W(2)/sqrt 2 = " ++ show values)

checks :: IO [Bool]
checks = sequence [counterLet, counterReturned, wienerLet]
