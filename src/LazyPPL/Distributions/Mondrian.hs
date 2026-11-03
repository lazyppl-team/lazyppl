{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module LazyPPL.Distributions.Mondrian where

import LazyPPL
import Data.IORef
-- import Debug.Trace
import LazyPPL.Distributions
import LazyPPL.Distributions.Counter
import System.IO.Unsafe
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
-- import Text.RawString.QQ
-- import qualified Data.List
-- import Control.DeepSeq


-- | From Roy-Teh "The Mondrian Process", NIPS 2009

-- | 'oneDimMondrian' corresponds to a Poisson point process on [low, high] with uniform intensity
oneDimMondrian :: Double -> (Double, Double) -> Prob [Double]
oneDimMondrian budget (low, high) = do
  cutCost <- exponential (high - low)
  if budget < cutCost
    then return []
    else do
      let remaining = budget - cutCost
      cut <- uniformbounded low high
      leftcuts <- oneDimMondrian remaining (low, cut)
      rightcuts <- oneDimMondrian remaining (cut, high)
      return $ leftcuts ++ [cut] ++ rightcuts

-- | Some data types. Intuitively, Matrix is a type of 2D exchangeable arrays, and 'Mondrian Double' would be the type of block-structured graphons.

data Mondrian a
  = Block a [(Double, Double)] -- ^ args:  and intervals making up the block
  | Partition Int Double [(Double, Double)] (Mondrian a) (Mondrian a) -- ^ args: dimension cutPosition intervals (hypercube over which the Mondrian is defined) subtree1 
  deriving (Eq, Show)

newtype Row = Row Int -- ^ in [0, 1]

newtype Col = Col Int

data MatrixIO = MatrixIO Counter Counter [[Bool]]

lookup :: MatrixIO -> Row -> Col -> Bool
lookup (MatrixIO _ _ matrix) (Row r) (Col c) = matrix !! r !! c

newRow :: MatrixIO -> Prob Row
newRow (MatrixIO c _ _) = readAndIncrement c >>= (return . Row)

newCol :: MatrixIO -> Prob Col
newCol (MatrixIO _ c _) = readAndIncrement c >>= (return . Col)

data Matrix = Matrix [[Bool]]

{- | Generates a random Mondrian tree: a random block partition of the rectangle [a1, b1] × ⋯ × [an, bn]
where each block has an associated random draw from the base distribution.
-}
randomMondrian :: Prob a -> Double -> [(Double, Double)] -> Prob (Mondrian a)
randomMondrian base budget abs = do
  let lengths = map (\(a, b) -> b - a) abs
  let sumLengths = sum lengths
  cutCost <- exponential sumLengths
  if budget < cutCost
    then do p <- base; return $ Block p abs
    else do
      let remaining = budget - cutCost
      dim <- categorical $ map (/sumLengths) lengths -- if dim is true then cut is perpendicular to (a_d, b_d)
      let (a_d, b_d) = abs !! dim
      cut <- uniformbounded a_d b_d
      leftMondrian <- randomMondrian base remaining
        $ zipWith (\ab i -> if i == dim then (a_d, cut) else ab) abs [0 ..]
      rightMondrian <- randomMondrian base remaining
        $ zipWith (\ab i -> if i == dim then (cut, b_d) else ab) abs [0 ..]
      return $ Partition dim cut abs leftMondrian rightMondrian

-- | Given a Mondrian tree and two data points, apply a function on the biais of the corresponding block. 
applyOnBiaisFromMondrian2D :: Mondrian Double -> Double -> Double -> (Double -> a) -> a
applyOnBiaisFromMondrian2D mondrian x y f = do
  case mondrian of
    Block p _ -> f p
    Partition !dim !cut _ left right ->
      let !xy = case dim of {0 -> x; _ -> y}
          !cond = xy < cut
          !lr = if cond then left else right in
      applyOnBiaisFromMondrian2D lr x y f

-- | Given a Mondrian tree and two data points, sample an 'edge'.
sampleFromMondrian2D :: Mondrian Double -> Double -> Double -> Prob Bool
sampleFromMondrian2D mondrian x y = applyOnBiaisFromMondrian2D mondrian x y bernoulli

-- | Probability of getting a given truth value 'val' at a (r, c) pair from a Mondrian. 
likelihoodFromMondrian2D :: Mondrian Double -> Double -> Double -> Bool -> Double
likelihoodFromMondrian2D mondrian x y val =
  let !f = if val then id else (1-) in
    applyOnBiaisFromMondrian2D mondrian x y f

{- | Sample a block structure over [0, 1]^2 from the Mondrian process,
and use it to generate an infinite exchangeable array.
-}
sampleRelationFromRandomMondrian2D :: Prob Double -> Double -> Prob Matrix
sampleRelationFromRandomMondrian2D base budget = do
  mondrian <- randomMondrian base budget [(0, 1), (0, 1)]
  sampleRelationFromMondrian2D mondrian

sampleRelationFromMondrian2D :: Mondrian Double -> Prob Matrix
sampleRelationFromMondrian2D mondrian = do
  rs <- iid uniform
  cs <- iid uniform
  matrix <- mapM (\r -> mapM (sampleFromMondrian2D mondrian r) cs) rs
  return $ Matrix matrix

sampleMapRelationFromMondrian2D :: Mondrian Double -> Int -> Prob (Map (Double, Double) Bool)
sampleMapRelationFromMondrian2D mondrian size = do
  xs <- iid uniform
  ys <- iid uniform
  matrix <- mapM (\x ->
    mapM (\y -> (do b <- sampleFromMondrian2D mondrian x y; return ((x, y), b))) (take size xs))
    (take size ys)
  return $ fromList $ concat matrix

sampleMatrixRelationFromMondrian2D :: Mondrian Double -> Prob Matrix
sampleMatrixRelationFromMondrian2D mondrian = do
  rs <- iid uniform
  cs <- iid uniform
  matrix <- mapM (\r ->
    mapM (sampleFromMondrian2D mondrian r) cs) rs
  return $ Matrix matrix


data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Eq, Show)

{- | Create a Mondrian tree over a hypercube `abs` from a tree of (dimension, cut) given in the chronological order, and a map associating to each block its bernoulli biais.
-}
mondrianFromCuts :: [(Double, Double)] -> BinTree (Int, Double) -> Map [(Double, Double)] Double -> Double -> Mondrian Double
mondrianFromCuts abs dcs biais defaultBiais =
  case dcs of
    Leaf -> Block (findWithDefault defaultBiais abs biais) abs
    Node (dim, cut) left right ->
      Partition dim cut abs
      (mondrianFromCuts leftAbs left biais defaultBiais)
      (mondrianFromCuts rightAbs right biais defaultBiais)
      where
        leftAbs = zipWith (\ab i -> if i == dim then (a0, cut) else ab) abs [0 ..]
        rightAbs = zipWith (\ab i -> if i == dim then (cut, b0) else ab) abs [0 ..]
        (a0, b0) = abs !! dim
