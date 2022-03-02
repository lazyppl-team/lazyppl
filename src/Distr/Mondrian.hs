{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Distr.Mondrian where

import LazyPPL
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
    ( RGB(channelBlue, channelGreen, channelRed) )
import Data.IORef
-- import Debug.Trace
import Distr
import Distr.Counter
import Graphics.Matplotlib
    ( subplots,
      text,
      xlim,
      ylim,
      (#),
      (##),
      (%),
      (@@),
      lit,
      mp,
      o2,
      readData,
      Matplotlib )
import System.IO.Unsafe
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
-- import Text.RawString.QQ
-- import qualified Data.List
import qualified Text.Printf
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
      dim <- categorical $ map (/sumLengths) lengths -- ^ if dim is true then cut is perpendicular to (a_d, b_d)
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

plotMondrian2D :: Mondrian Double -> Matplotlib
plotMondrian2D mondrian = case mondrian of
  Block p abs@(~[(a1, b1), (a2, b2)]) ->
    mp # "ax.add_patch(plot.Polygon(" # polygonFromBounds abs ## "))"
        @@ [o2 "facecolor" "0.9", o2 "edgecolor" "0.5"]
    % text ((a1 + b1)/2) ((a2 + b2)/2) ("$" ++ Text.Printf.printf "%.2f" p ++ "$")
      @@ [o2 "horizontalalignment" "center", o2 "fontsize" 20]
  Partition _ _ ~[(a1, b1), (a2, b2)] left right ->
    subplots % xlim a1 b1 % ylim a2 b2
      % helper left % helper right
  where
    polygonFromBounds ~[(a1, b1), (a2, b2)] = [(a1, a2), (b1, a2), (b1, b2), (a1, b2)]
    helper (Block p abs@(~[(a1, b1), (a2, b2)])) =
      let c = hsv (p * 360) 0.4 1
          rgba alpha = (channelRed c, channelGreen c, channelBlue c, alpha)
          pythonRgba alpha = lit $ show $ rgba alpha
      in mp # "ax.add_patch(plot.Polygon(" # polygonFromBounds abs ## "))"
            @@ [o2 "facecolor" (pythonRgba 0.9),
            o2 "edgecolor" "0.3",
            o2 "linewidth" 1.5]
          % text ((a1 + b1)/2) ((a2 + b2)/2) ("$" ++ Text.Printf.printf "%.2f" p ++ "$")
            @@ [o2 "horizontalalignment" "center", o2 "fontsize" 10]
    helper (Partition _ _ _ left right) = helper left % helper right

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

{- | Display dots corresponding to the pairs (ξ_i , η_j) of a finite relation in the plot -}
plotMapRelation :: Matplotlib -> Map (Double, Double) Bool -> Matplotlib
plotMapRelation mp rel =
  readData (xs, ys, m, c)
  % mp # newDefScatter
  # "mscatter(data[0], data[1], ax=ax, s=60, linewidths=2, m=data[2], c=data[3], alpha=0.8, zorder=2)"
  where
  ks = keys rel
  (xs, ys) = unzip ks
  -- green = (200/255, 247/255, 197/255, 1)
  -- red = (254/255, 121/255, 104/255, 1)
  (m, c) = unzip $ map (\(x, y) -> if rel ! (x, y) then ("+", "g") else ("x", "r")) ks
  -- custom definition of `scatter` to support a list of markers
  -- credit: https://github.com/matplotlib/matplotlib/issues/11155#issuecomment-385939618
  newDefScatter = "\n\
\def mscatter(x, y, ax=None, m=None, **kw):\n\
\  import matplotlib.markers as mmarkers\n\
\  if not ax: ax=plt.gca()\n\
\  sc = ax.scatter(x,y,**kw)\n\
\  if (m is not None) and (len(m)==len(x)):\n\
\    paths = []\n\
\    for marker in m:\n\
\      if isinstance(marker, mmarkers.MarkerStyle):\n\
\        marker_obj = marker\n\
\      else:\n\
\        marker_obj = mmarkers.MarkerStyle(marker)\n\
\      path = marker_obj.get_path().transformed(marker_obj.get_transform())\n\
\      paths.append(path)\n\
\    sc.set_paths(paths)\n\
\  return sc\n"