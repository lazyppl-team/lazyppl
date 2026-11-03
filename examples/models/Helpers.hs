{-# LANGUAGE GeneralizedNewtypeDeriving, 
    ScopedTypeVariables,
    RankNTypes, BangPatterns #-}
module Helpers where
import LazyPPL
import Control.DeepSeq
import qualified Data.List as L (lookup)
import Data.Array (Array, Ix, (!), indices)
import Data.List (nub)


iterateNM :: Int -> (a -> IO a) -> a -> IO [a]
iterateNM 0 f a = return []
iterateNM n f a = do
  a' <- f a
  as <- iterateNM (n -1) f a'
  return $ a : as

-- | Take eagerly from a list and print the current progress. 
-- Keep line-based output for redirected demo logs; terminal-progress-bar
-- manages an animated terminal display rather than this list-taking interface.
takeWithProgress :: Int -> [a] -> IO [a]
takeWithProgress n = helper n n
  where
    helper :: Int -> Int -> [a] -> IO [a]
    helper _ i _ | i <= 0 = return []
    helper _ _ []        = return []
    helper n i ((!x):xs)    = do
      putStrLn $ "Progress: " ++ show (fromIntegral (100*(n-i+1)) / fromIntegral n) ++ "%"
      xs' <- helper n (i-1) xs
      return $ x : xs'

-- | Take as eagerly as possible from a list of tuples and print the current progress. 
hardTakeWithProgress :: NFData a => Int -> [a] -> IO [a]
hardTakeWithProgress n = helper n n
  where
    helper :: NFData a =>  Int -> Int -> [a] -> IO [a]
    helper _ i _ | i <= 0 = return []
    helper _ _ []        = return []
    helper n i (x:xs)    = do
      putStrLn $ x `deepseq` ("Progress: " ++ show (fromIntegral (100*(n-i+1)) / fromIntegral n) ++ "%")
      xs' <- helper n (i-1) xs
      return $ x : xs'

takeEager :: Int -> [a] -> [a]
takeEager n = helper n n
  where
    helper :: Int -> Int -> [a] -> [a]
    helper _ i _ | i <= 0 = []
    helper _ _ []        = []
    helper n i ((!x):xs) = x : helper n (i-1) xs

takeEveryDrop :: Int -> Int -> Int -> [a] -> [a]
takeEveryDrop nTake nEvery nDrop stream = 
  take nTake $ every nEvery $ drop nDrop stream

takeEagerEveryDrop :: Int -> Int -> Int -> [a] -> [a]
takeEagerEveryDrop nTake nEvery nDrop stream = 
  takeEager nTake $ every nEvery $ drop nDrop stream

takeProgressEveryDrop :: Int -> Int -> Int -> [a] -> IO [a]
takeProgressEveryDrop nTake nEvery nDrop stream = 
  takeWithProgress nTake $ every nEvery $ drop nDrop stream

maxWeightElement :: Ord w => [(a, w)] -> a
maxWeightElement aws =
  let maxw = maximum $ map snd aws
      (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
  a

maxWeightPair :: Ord w => [(a, w)] -> (a, w)
maxWeightPair aws =
  let maxw = maximum $ map snd aws
      (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
  (a, maxw)

-- | An example probability distribution.
exampleProb :: Prob Double
exampleProb = do
  choice <- uniform
  w <- uniform
  x <- uniform
  y <- uniform
  z <- uniform
  case floor (choice * 4.0) of
    0 -> return w
    1 -> return x
    2 -> return y
    3 -> return z



-- | One row per index and one column per distinct feature, as plotted in AdditiveClusteringDemo.
-- A held feature shows as its column number, counted from 1, so that 0 means absent.
featureMembership :: (Ix x, Eq y) => Array x [y] -> [[Int]]
featureMembership features =
  let xs = indices features
      ys = nub (concatMap (features !) xs)
  in [[ if elem y (features ! x) then column else 0 | (column, y) <- zip [1 ..] ys] | x <- xs]

-- | The vertical tick positions and labels of a feature plot: one per row, the
-- array's own indices in index order, at positions counted from the first row.
-- A subrange array is therefore labelled by the indices it contains.
featureRowTicks :: Ix x => Array x [y] -> [(Int, x)]
featureRowTicks features = zip [0 ..] (indices features)

-- | The colour-scale value of a feature-membership cell, for a plot with the given number of columns.
-- 0 means absent. A cyclic colour map such as twilight starts and ends on nearly the same light colour,
-- so held features take the dark middle half, from 0.25 to 0.75, in column order.
featureShade :: Int -> Int -> Double
featureShade _ 0 = 0
featureShade columns column = 0.25 + 0.5 * (fromIntegral column - 0.5) / fromIntegral columns
