{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module LazyPPL.Internal.History
  (History, newHistory, appendMutation, labelAt) where

import Control.Concurrent.MVar
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.Unboxed.Mutable as Vector
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import LazyPPL.Internal.Sampler (randomLabel)
import System.Random (SeedGen(fromSeed64, toSeed64), StdGen, split)

-- A record holds a StdGen as the two Word64 values (16 bytes) of its public
-- SeedGen instance in `random` 1.3. Stored unboxed, the records contain no
-- pointers, so the garbage collector skips them, and the `vector` package does
-- the allocation and copying.
data Cursor = Cursor !(Vector.IOVector (Word64,Word64)) !Int
data History = History !StdGen !(MVar (Maybe Cursor))

newHistory :: StdGen -> IO History
newHistory seed = History seed <$> newMVar Nothing

appendMutation :: History -> Int -> StdGen -> IO ()
appendMutation (History _ lock) index generator =
  modifyMVar_ lock $ \old -> do
    Cursor records count <- case old of
      Nothing -> do
        unless (index == 0) (ioError (userError "missing MH history"))
        records <- Vector.new 1
        pure (Cursor records 0)
      Just cursor -> pure cursor
    unless (count == index) (ioError (userError "out-of-order MH history"))
    unless (index < (maxBound :: Int))
      (ioError (userError "MH history exceeds addressable record count"))
    let capacity = Vector.length records
    grown <- if index < capacity
      then pure records
      -- Grow by doubling, without overflowing Int.
      else Vector.grow records (min capacity (maxBound-capacity))
    case toSeed64 generator of
      seed :| [gamma] -> Vector.write grown index (seed,gamma)
      _ -> ioError (userError "StdGen seed is not 128 bits")
    pure (Just (Cursor grown (index+1)))

-- `labelAt p history count path` is the label at `path` (deepest index first)
-- after the first `count` mutations, where a mutation replaces each label
-- independently with probability `p`.
--
-- readMVar waits for an append in progress, so those `count` records are
-- initialized. Later appends write only after them, and growth copies them to
-- a new vector. A reader's vector and count therefore stay valid.
{-# NOINLINE labelAt #-}
labelAt :: Double -> History -> Int -> [Int] -> Double
labelAt p (History initial lock) count reversedPath = unsafePerformIO $
  readMVar lock >>= \cursor -> do
    let path = reverse reversedPath
        original = fst (randomLabel (atPath 1 path initial))
    if count == 0 || not (p > 0)
      then pure original
      else case cursor of
        Nothing -> ioError (userError "missing MH checkpoint records")
        Just (Cursor records written) -> do
          unless (count <= written) (ioError (userError "incomplete MH checkpoint"))
          let search :: Int -> IO Double
              search !index
                | index < 0 = pure original
                | otherwise = do
                    (seed,gamma) <- Vector.read records index
                    let generator = atPath 2 path (fromSeed64 (seed :| [gamma]))
                        (decision, next) = randomLabel generator
                    if decision < p
                      then pure (fst (randomLabel next))
                      else search (index-1)
          value <- search (count-1)
          value `seq` pure value

-- At each node, randomTree draws once before splitting its seed and mutateTree
-- draws twice.
atPath :: Int -> [Int] -> StdGen -> StdGen
atPath _ [] generator = generator
atPath draws (index:path) generator = atPath draws path (child index (advance draws generator))
  where
    advance 0 g = g
    advance n g = advance (n-1) (snd (randomLabel g))
    child 0 g = fst (split g)
    child n g = child (n-1) (snd (split g))
