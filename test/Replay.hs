module Main where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate, throwIO)
import Control.Monad (forM, forM_, unless)
import Data.List (nub)
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)
import LazyPPL.Internal.History (History, appendMutation, labelAt, newHistory)
import LazyPPL.Internal.Sampler (randomLabel)
import System.Exit (exitFailure)
import System.Random (StdGen, mkStdGen, split)
import Test.QuickCheck

-- Reference randomTree and mutateTree, as in LazyPPL 1.0.1, built without replay or the seed
-- store.
data RefTree = RefTree Double [RefTree]

randomRefTree :: StdGen -> RefTree
randomRefTree generator =
  let (value, next) = randomLabel generator
  in RefTree value (randomRefTrees next)

randomRefTrees :: StdGen -> [RefTree]
randomRefTrees generator =
  let (left, right) = split generator
  in randomRefTree left : randomRefTrees right

mutateRefTree :: Double -> StdGen -> RefTree -> RefTree
mutateRefTree rate generator (RefTree value children) =
  let (decision, next) = randomLabel generator
      (replacement, rest) = randomLabel next
  in RefTree (if decision < rate then replacement else value)
       (mutateRefTrees rate rest children)

mutateRefTrees :: Double -> StdGen -> [RefTree] -> [RefTree]
mutateRefTrees rate generator (tree:trees) =
  let (left, right) = split generator
  in mutateRefTree rate left tree : mutateRefTrees rate right trees
mutateRefTrees _ _ [] = []

referenceLabel :: [Int] -> RefTree -> Double
referenceLabel [] (RefTree value _) = value
referenceLabel (index:path) (RefTree _ children) =
  referenceLabel path (children !! index)

data ReplayCase = ReplayCase
  { initialSeed :: Int
  , proposalSeed :: Int
  , prefixSteps :: Int
  , laterSteps :: Int
  , intermediateRate :: Double
  , demandedPaths :: [[Int]]
  } deriving Show

instance Arbitrary ReplayCase where
  arbitrary = do
    initial <- chooseInt (minBound, maxBound)
    proposals <- chooseInt (minBound, maxBound)
    prefix <- oneof [chooseInt (0, 256), elements
      [31,32,33,63,64,65,95,96,97,127,128,129,4095,4096,4097]]
    later <- chooseInt (1, 96)
    rate <- choose (0.0001, 0.9999)
    paths <- listOf1 $ do
      depth <- chooseInt (1, 4)
      vectorOf depth (chooseInt (0, 5))
    pure (ReplayCase initial proposals prefix later rate (nub ([] : take 6 paths)))
  shrink input =
    [input {prefixSteps=n} | n <- shrink (prefixSteps input), n >= 0] ++
    [input {initialSeed=n} | n <- shrink (initialSeed input)] ++
    [input {proposalSeed=n} | n <- shrink (proposalSeed input)] ++
    [input {demandedPaths=[]} | not (null (demandedPaths input))]

proposalGenerators :: StdGen -> [StdGen]
proposalGenerators generator =
  let (left, right) = split generator
  in left : proposalGenerators right

appendAll :: History -> Int -> [StdGen] -> IO ()
appendAll history start generators =
  forM_ (zip [start..] generators) $ \(index, generator) ->
    appendMutation history index generator

replayedLabels :: Double -> History -> Int -> [[Int]] -> IO [Word64]
replayedLabels rate history count paths = do
  -- Each path gets its own thread, so with -N2 labels replay concurrently.
  slots <- forM paths $ \path -> do
    slot <- newEmptyMVar
    _ <- forkFinally
      (evaluate (castDoubleToWord64 (labelAt rate history count (reverse path))))
      (putMVar slot)
    pure slot
  answers <- mapM takeMVar slots
  mapM (either throwIO pure) answers

prop_replayPreservesDelayedLabels :: ReplayCase -> Property
prop_replayPreservesDelayedLabels input =
  classify (prefixSteps input >= 96) "at least 96 appended records" $
  classify (prefixSteps input >= 4095) "near the 4096-record growth" $
  classify (any ((> 1) . length) (demandedPaths input)) "nested paths" $
  ioProperty $ do
    let initial = mkStdGen (initialSeed input)
        generators = proposalGenerators (mkStdGen (proposalSeed input))
        (prefix, suffix) = splitAt (prefixSteps input) generators
        paths = nub ([] : demandedPaths input)
    results <- forM [0, intermediateRate input, 1] $ \rate -> do
      history <- newHistory initial
      appendAll history 0 prefix
      -- Later records are appended before any label is demanded, and replay must ignore them.
      appendAll history (prefixSteps input) (take (laterSteps input) suffix)
      actual <- replayedLabels rate history (prefixSteps input) paths
      let reference = foldl (\tree generator -> mutateRefTree rate generator tree)
                        (randomRefTree initial) prefix
          expected = map (castDoubleToWord64 . (`referenceLabel` reference)) paths
      pure $ counterexample ("rate=" ++ show rate ++ ", paths=" ++ show paths ++
                             ", actual=" ++ show actual ++ ", expected=" ++ show expected)
              (actual === expected)
    pure $ conjoin results

main :: IO ()
main = do
  result <- quickCheckResult prop_replayPreservesDelayedLabels
  unless (isSuccess result) exitFailure
