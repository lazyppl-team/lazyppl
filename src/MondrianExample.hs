{-# LANGUAGE BangPatterns #-}
module MondrianExample where

import Distr
import Distr.Mondrian
import LazyPPL
import Graphics.Matplotlib
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
import qualified Data.List
import System.IO.Unsafe

takeWithProgress :: Int -> [a] -> IO [a]
takeWithProgress n = helper n n
  where
    helper :: Int -> Int -> [a] -> IO [a]
    helper _ i _ | i <= 0 = return []
    helper _ _ []        = return []
    helper n i ((!x):xs)    = do
      putStrLn $ "Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%"
      xs' <- helper n (i-1) xs
      return $ x : xs'

-- | __Mondrian Process examples:__
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
      black = 0.7
  in mondrianFromCuts
        [(0, 1), (0, 1)]
        cutTree
        ( fromList
            [ ([(0.9, 1), (0, 0.3)], red),
              ([(0, 0.2), (0, 0.3)], yellow),
              ([(0.7, 0.9), (0.05, 0.3)], blue),
              ([(0.4, 0.7), (0.05, 0.2)], black),
              ([(0.2, 0.7), (0.4, 0.85)], red),
              ([(0.7, 0.9), (0.85, 1)], yellow),
              ([(0.7, 0.9), (0.6, 0.85)], yellow),
              ([(0.2, 0.4), (0.3, 0.4)], black)
            ]
        )
        gray
  where
    cutTree =
      Node
        (0, 0.9)
        ( Node
            (1, 0.3)
            ( Node
                (0, 0.2)
                Leaf
                ( Node
                    (0, 0.4)
                    (Node (1, 0.2) Leaf Leaf)
                    ( Node
                        (1, 0.05)
                        Leaf
                        ( Node
                            (0, 0.7)
                            (Node (1, 0.2) Leaf Leaf)
                            Leaf
                        )
                    )
                )
            )
            ( Node
                (1, 0.85)
                ( Node
                    (0, 0.2)
                    Leaf
                    ( Node
                        (0, 0.7)
                        ( Node
                            (1, 0.4)
                            (Node (0, 0.4) Leaf Leaf)
                            Leaf
                        )
                        ( Node
                            (1, 0.6)
                            ( Node
                                (1, 0.4)
                                Leaf
                                (Node (0, 0.8) Leaf Leaf)
                            )
                            Leaf
                        )
                    )
                )
                ( Node
                    (0, 0.3)
                    Leaf
                    (Node (0, 0.7) Leaf Leaf)
                )
            )
        )
        (Node (1, 0.3) Leaf Leaf)

plotPietMondrian :: Matplotlib
plotPietMondrian = plotMondrian2D paintingPietMondrian1921

datasetMapRelations :: IO [Map (Double, Double) Bool]
datasetMapRelations = do
  rels' <- mh 0.2 $ sample 
    $ sampleMapRelationFromMondrian2D paintingPietMondrian1921 20
  let rels = map fst $ take 100 $ every 10 rels'
  return rels

-- plotPietPlusRelation :: Matplotlib
-- plotPietPlusRelation = plotMapRelation plotPietMondrian (head $ unsafePerformIO datasetMapRelations)

datasetMatrixRelations :: IO [Matrix]
datasetMatrixRelations = do
  rels' <- mh 0.2 $ sample 
    $ sampleMatrixRelationFromMondrian2D paintingPietMondrian1921
  let rels = map fst $ take 100 $ every 10 rels'
  return rels

-- | Statistical model 1: infers the hyperparameters of a Mondrian by observing
-- Map relations generated from it.
inferMondrianMap ::
  Foldable t =>
  t (Map (Double, Double) Bool) ->
  Prob Double ->
  Double ->
  [(Double, Double)] ->
  Meas (Mondrian Double)
inferMondrianMap dataset base budget intervals = do
  mondrian <- sample $ randomMondrian base budget intervals
  let scoreRel rel =
        mapM
          (\(r, c) -> score $ likelihoodFromMondrian2D mondrian r c (rel ! (r, c)))
          (keys rel)
  mapM_ scoreRel dataset
  return mondrian


-- | Statistical model 2: infers the hyperparameters of a Mondrian by observing 
-- Boolean matrices (relations) generated from it.
inferMondrianMatrix :: Foldable t => Int -> t Matrix 
  -> Prob Double -> Double -> [(Double, Double)]
  -> Meas (Mondrian Double)
inferMondrianMatrix size dataset base budget intervals = do
  mondrian <- sample $ randomMondrian base budget intervals
  rs <- sample $ iid uniform
  cs <- sample $ iid uniform
  let xrs = zip rs [0 .. size]
  let ycs = zip cs [0 .. size]
  let scoreRel (Matrix rel) =
        mapM (\(x, r) ->
          mapM (\(y, c) -> score 
            (likelihoodFromMondrian2D mondrian x y (rel !! r !! c))
        ) ycs) xrs
  mapM_ scoreRel dataset
  return mondrian

mhInferenceMap :: IO Matplotlib
mhInferenceMap = do
  dataset <- datasetMapRelations
  mws' <- mh 0.2 $ inferMondrianMap dataset uniform 3 [(0, 1), (0, 1)]
  mws <- takeWithProgress 5000 $ every 100 $ drop 100 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

mhInferenceMatrix :: Int -> IO Matplotlib
mhInferenceMatrix size = do
  dataset <- datasetMatrixRelations
  mws' <- mh 0.2 $ inferMondrianMatrix size dataset uniform 3 [(0, 1), (0, 1)]
  mws <- takeWithProgress 5000 $ every 100 $ drop 100 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

-- mh1Inference :: IO Matplotlib
-- mh1Inference = do
--   dataset <- datasetRelations
--   mws' <- mh1 $ inferMondrian dataset uniform 4 [(0, 1), (0, 1)]
--   let mws = take 500 $ every 1000 $ drop 10000 mws'
--   let maxw = maximum $ map snd mws
--   let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
--   return $ plotMondrian2D m

lwisInference :: IO Matplotlib
lwisInference = do
  dataset <- datasetMapRelations
  mws' <- weightedsamples $ inferMondrianMap dataset uniform 4 [(0, 1), (0, 1)]
  let mws = take 1000 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

mhResultMap :: Matplotlib
mhResultMap = unsafePerformIO mhInferenceMap

main :: IO ()
main = do
  putStrLn "Plotting..."
  -- -- Map relations inference
  -- dataRel <- datasetMapRelations
  -- file "pietMondrian-plus-relation.svg" $ plotMapRelation plotPietMondrian $ head dataRel
  -- testInf <- mhInferenceMap
  --  -- testInf <- lwisInference
  -- file "mondrian-relation-map.svg" testInf


  -- Matrix relations inference
  dataRel <- datasetMatrixRelations
  file "pietMondrian.svg" plotPietMondrian
  testInf <- mhInferenceMatrix 20
  file "mondrian-relation-matrix.svg" testInf
  
  putStrLn "Done."