{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Main where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (MVar)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forM, replicateM, unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import qualified Data.Set as Set
import LazyPPL (Prob, mh, randomTree, runProb, sample, uniform, weightedsamples)
import LazyPPL.Distributions.Counter (newCounter, readAndIncrement)
import LazyPPL.Distributions.GP (gp)
import LazyPPL.Distributions.Memoization (generalmemoize)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)
import System.Timeout (timeout)

-- Runs `draw` `runs` times in each of two threads at once.
concurrentDraws :: Int -> IO a -> IO [a]
concurrentDraws runs draw = do
  slots <- forM [1, 2 :: Int] $ \_ -> do
    slot <- newEmptyMVar
    _ <- forkFinally (replicateM runs (draw >>= evaluate)) (putMVar slot)
    pure slot
  concat <$> mapM (\slot -> takeMVar slot >>= either throwIO pure) slots

report :: String -> Bool -> String -> IO Bool
report name ok detail = do
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ name ++ ": " ++ detail
  pure ok

-- Each inference run takes its own generator from the global one. Two runs
-- that start at the same moment must still get different generators, so no
-- two runs share their first draw.
distinctFirstDraws :: String -> IO Double -> IO Bool
distinctFirstDraws name draw = do
  draws <- concurrentDraws 20000 draw
  let distinct = Set.size (Set.fromList draws)
  report name (distinct == length draws)
    (show distinct ++ " distinct first draws in " ++ show (length draws) ++ " concurrent runs")

-- The pause point of the interleaving tests below: the thread that makes the
-- `pauseAt`-th marked call signals `inside`, then waits for `release`. The
-- wait has a time limit, because a correct implementation can make the other
-- thread wait for this one.
{-# NOINLINE pauseHook #-}
pauseHook :: IORef (IO ())
pauseHook = unsafePerformIO (newIORef (pure ()))

markedCall :: IO ()
markedCall = do
  action <- atomicModifyIORef' pauseHook (\a -> (a, a))
  action

pauseAt :: Int -> IO (MVar (), MVar ())
pauseAt target = do
  inside <- newEmptyMVar
  release <- newEmptyMVar
  calls <- newIORef (0 :: Int)
  writeIORef pauseHook $ do
    n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
    when (n == target) $ do
      putMVar inside ()
      _ <- timeout 2000000 (takeMVar release)
      pure ()
  pure (inside, release)

-- Keys whose comparison makes a marked call when the left key is 1. The
-- comparison results are the ordinary ones.
newtype Key = Key Int deriving (Eq, Show)

instance Ord Key where
  compare (Key a) (Key b) = unsafePerformIO (when (a == 1) markedCall) `seq` compare a b

-- Two threads query a memoized stochastic function at two new keys at once.
-- The first thread pauses at its second comparison, after it has read the
-- table and before it has stored its entry, and the second thread runs in
-- between. Each key must still get its own draw, and a repeated query must
-- return the draw made for its key.
memoInterleaving :: IO Bool
memoInterleaving = do
  let f = runProb (generalmemoize (const uniform :: Key -> Prob Double)) (randomTree (mkStdGen 7))
  _ <- evaluate (f (Key 0))
  (inside, release) <- pauseAt 2
  slot <- newEmptyMVar
  _ <- forkFinally (evaluate (f (Key 1))) (putMVar slot)
  takeMVar inside
  second <- evaluate (f (Key 2))
  putMVar release ()
  first <- takeMVar slot >>= either throwIO pure
  writeIORef pauseHook (pure ())
  repeated1 <- evaluate (f (Key 1))
  repeated2 <- evaluate (f (Key 2))
  report "memoize under an interleaving" (first /= second && repeated1 == first && repeated2 == second)
    ("(first1,first2,repeat1,repeat2) = " ++ show (first, second, repeated1, repeated2))

-- The same interleaving for a white-noise Gaussian process, whose values at
-- distinct points are independent. The covariance function makes the marked
-- call at (1,1), inside the first thread's query.
gpInterleaving :: IO Bool
gpInterleaving = do
  let cov x y = unsafePerformIO (when (x == 1 && y == 1) markedCall) `seq` (if x == y then 1 else 0)
      f = runProb (gp cov) (randomTree (mkStdGen 8))
  _ <- evaluate (f 0)
  (inside, release) <- pauseAt 1
  slot <- newEmptyMVar
  _ <- forkFinally (evaluate (f 1)) (putMVar slot)
  takeMVar inside
  second <- evaluate (f 2)
  putMVar release ()
  first <- takeMVar slot >>= either throwIO pure
  writeIORef pauseHook (pure ())
  repeated1 <- evaluate (f 1)
  repeated2 <- evaluate (f 2)
  report "white-noise gp under an interleaving" (first /= second && repeated1 == first && repeated2 == second)
    ("(f1,f2,repeat f1,repeat f2) = " ++ show (first, second, repeated1, repeated2))

-- A counter hands out each number once, even to two threads at once. The
-- counter is bound by evaluate, as a Prob bind would bind it: a let-bound
-- counter can be inlined into the lambda below, with one IORef per call.
counterUnique :: IO Bool
counterUnique = do
  counter <- evaluate (runProb newCounter (randomTree (mkStdGen 9)))
  let next k = runProb (readAndIncrement counter) (randomTree (mkStdGen k))
  seeds <- newIORef (0 :: Int)
  numbers <- concurrentDraws 20000 $ do
    k <- atomicModifyIORef' seeds (\s -> (s + 1, s))
    evaluate (next k)
  let distinct = Set.size (Set.fromList numbers)
  report "counter shared by two threads" (distinct == length numbers)
    (show distinct ++ " distinct numbers from " ++ show (length numbers) ++ " increments")

main :: IO ()
main = do
  results <- sequence
    [ distinctFirstDraws "weightedsamples" (fst . head <$> weightedsamples (sample uniform))
    , distinctFirstDraws "mh" (fst . head <$> mh 1 (sample uniform))
    , memoInterleaving
    , gpInterleaving
    , counterUnique
    ]
  unless (and results) exitFailure
