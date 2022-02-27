{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}

module Distr.Mondrian where

import LazyPPL
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
    ( RGB(channelBlue, channelGreen, channelRed) )
import Data.IORef
import Debug.Trace
import Distr
import Distr.Counter
import Graphics.Matplotlib
import System.IO.Unsafe
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
-- import Text.RawString.QQ
import qualified Data.List
import qualified Text.Printf

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
  = Block a [(Double, Double)] -- ^ args: atomName and intervals making up the block
  | Partition Int Double [(Double, Double)] (Mondrian a) (Mondrian a) -- ^ args: dimension cutPosition intervals (hypercube over which the Mondrian is defined) subtree1 subtree2

newtype Row = Row Int -- ^ in [0, 1]

newtype Col = Col Int

data Matrix = Matrix Counter Counter [[Bool]]

lookup :: Matrix -> Row -> Col -> Bool
lookup (Matrix _ _ matrix) (Row r) (Col c) = matrix !! r !! c

newRow :: Matrix -> Prob Row
newRow (Matrix c _ _) = readAndIncrement c >>= (return . Row)

newCol :: Matrix -> Prob Col
newCol (Matrix _ c _) = readAndIncrement c >>= (return . Col)

{- | Generates a random Mondrian tree: a random block partition of the rectangle [a1, b1] × ⋯ × [an, bn]
where each block has an associated random draw from the base distribution.
-}
randomMondrian :: Prob a -> Double -> [(Double, Double)] -> Prob (Mondrian a)
randomMondrian base budget abs = do
  let lengths = map (\(a, b) -> b - a) abs
  let sumLengths = sum lengths
  cutCost <- exponential sumLengths
  if budget < cutCost
    then do x <- base; return $ Block x abs
    else do
      let remaining = budget - cutCost
      dim <- categorical $ map (/sumLengths) lengths -- if dim is true then cut is perpendicular to (a0, b0)
      let (a0, b0) = abs !! dim
      cut <- uniformbounded a0 b0
      leftMondrian <- randomMondrian base remaining $ zipWith (\ab i -> if i == dim then (a0, cut) else ab) abs [0 ..]
      rightMondrian <- randomMondrian base remaining $ zipWith (\ab i -> if i == dim then (cut, b0) else ab) abs [0 ..]
      return $ Partition dim cut abs leftMondrian rightMondrian

-- | Given a Mondrian tree and two data points, apply a function on the biais of the corresponding block. 
applyOnBiaisFromMondrian2D :: Mondrian Double -> Double -> Double -> (Double -> a) -> a
applyOnBiaisFromMondrian2D mondrian r c f = do
  case mondrian of
    Block p _ -> f p
    Partition dim cut _ left right -> applyOnBiaisFromMondrian2D ([right, left] !! i) r c f
      where i = fromEnum $ ([c, r] !! dim) < cut


-- | Given a Mondrian tree and two data points, sample an 'edge'.
sampleFromMondrian2D :: Mondrian Double -> Double -> Double -> Prob Bool
sampleFromMondrian2D mondrian r c = applyOnBiaisFromMondrian2D mondrian r c bernoulli

-- | Probability of getting a given truth value 'val' at a (r, c) pair from a Mondrian. 
likelihoodFromMondrian2D :: Mondrian Double -> Double -> Double -> Bool -> Double
likelihoodFromMondrian2D mondrian r c val = applyOnBiaisFromMondrian2D mondrian r c (\p -> if val then p else 1-p)

iid :: Prob a -> Prob [a]
iid p = do r <- p; rs <- iid p; return $ r : rs

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
  rowCounter <- newCounter
  colCounter <- newCounter
  return $ Matrix rowCounter colCounter matrix

sampleFiniteRelationFromMondrian2D :: Mondrian Double -> Int -> Prob (Map (Double, Double) Bool)
sampleFiniteRelationFromMondrian2D mondrian size = do
  rs <- iid uniform
  cs <- iid uniform
  matrix <- mapM (\r ->
    mapM (\c -> (do b <- sampleFromMondrian2D mondrian r c; return ((r, c), b))) (take size cs))
    (take size rs)
  return $ fromList $ concat matrix



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
plotRelation :: Matplotlib -> Map (Double, Double) Bool -> Matplotlib
plotRelation mp rel =
  readData (xs, ys, m, c)
  % mp # newDefScatter
  # "mscatter(data[0], data[1], ax=ax, s=60, linewidths=2, m=data[2], c=data[3], alpha=0.8)"
  where
  ks = keys rel
  (xs, ys) = unzip ks
  -- green = (200/255, 247/255, 197/255, 1)
  -- red = (254/255, 121/255, 104/255, 1)
  (m, c) = unzip $ map (\(x, y) -> if rel ! (x, y) then ("+", "g") else ("x", "r")) ks
  -- custom definition of `scatter` to support a list of markers
  -- credit: https://github.com/matplotlib/matplotlib/issues/11155#issuecomment-385939618
  newDefScatter = "\n\
\def mscatter(x,y,ax=None, m=None, **kw):\n\
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

{- | __Examples:__ -}

mond :: Prob (Mondrian Double)
mond = randomMondrian uniform 5 [(0, 1), (0, 1)]

mond1 :: Mondrian Double
mond1 = fst $ head $ unsafePerformIO $ mh 0.2 (sample mond)

plotMond :: Matplotlib
plotMond = plotMondrian2D mond1

paintingPietMondrian1921 :: Mondrian Double
paintingPietMondrian1921 =
  let red = 0.9
      yellow = 0.2
      blue = 0.5
      gray = 0.3
      black = 0.7 in
  mondrianFromCuts [(0, 1), (0, 1)]
  cutTree
  (fromList
    [
      ([(0.9,1), (0,0.3)], red),
      ([(0,0.2), (0,0.3)], yellow),
      ([(0.7,0.9), (0.05,0.3)], blue),
      ([(0.4,0.7), (0.05,0.2)], black),
      ([(0.2,0.7), (0.4,0.85)], red),
      ([(0.7,0.9), (0.85,1)], yellow),
      ([(0.7,0.9), (0.6,0.85)], yellow),
      ([(0.2,0.4), (0.3,0.4)], black)
    ])
    gray
    where
      cutTree =
        Node (0, 0.9)
          (Node (1, 0.3)
            (Node (0, 0.2)
              Leaf
              (Node (0, 0.4)
                (Node (1, 0.2) Leaf Leaf)
                (Node (1, 0.05)
                  Leaf
                  (Node (0, 0.7)
                    (Node (1, 0.2) Leaf Leaf)
                    Leaf))))
            (Node (1, 0.85)
              (Node (0, 0.2)
                Leaf
                (Node (0, 0.7)
                  (Node (1, 0.4)
                    (Node (0, 0.4) Leaf Leaf)
                    Leaf)
                  (Node (1, 0.6)
                    (Node (1, 0.4)
                      Leaf
                      (Node (0, 0.8) Leaf Leaf))
                    Leaf)))
              (Node (0, 0.3)
                Leaf
                (Node (0, 0.7) Leaf Leaf))))
          (Node (1, 0.3) Leaf Leaf)

plotPietMondrian :: Matplotlib
plotPietMondrian = plotMondrian2D paintingPietMondrian1921


datasetRelations :: IO [Map (Double, Double) Bool]
datasetRelations = do
  rels' <- mh 1000 $ sample (sampleFiniteRelationFromMondrian2D paintingPietMondrian1921 20)
  let rels = map fst $ take 100 $ every 10 rels'
  return rels

plotPietPlusRelation :: Matplotlib
plotPietPlusRelation = plotRelation plotPietMondrian (head $ unsafePerformIO datasetRelations)

{- | Statistical model: infers the hyperparameters of a Mondrian by observing 
relations generated from it. -}
inferMondrian:: Foldable t => 
  t (Map (Double, Double) Bool) 
  -> Prob Double -> Double -> [(Double, Double)] 
  -> Meas (Mondrian Double)
inferMondrian dataset base budget intervals = do
  mondrian <- sample $ randomMondrian base budget intervals
  let scoreRel rel =
        mapM 
        (\(r, c) -> score $ likelihoodFromMondrian2D mondrian r c (rel ! (r, c))) 
        (keys rel)
  mapM_ scoreRel dataset
  return mondrian


testInference :: IO Matplotlib
testInference = do
  let dataset = unsafePerformIO datasetRelations
  monds' <- mh 7000 $ inferMondrian dataset uniform 4 [(0, 1), (0, 1)]
  -- let mws = take 100 $ every 100 $ drop 1000 monds'
  let mws = drop 1000 monds'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m,w) -> (w,m)) mws
  return $ plotMondrian2D m

{-# NOINLINE testResult #-}
testResult :: Matplotlib
testResult = unsafePerformIO testInference