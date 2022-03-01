module MondrianExample where

import Distr
import Distr.Mondrian
import LazyPPL

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

datasetRelations :: IO [Map (Double, Double) Bool]
datasetRelations = do
  rels' <- mh 0.2 $ sample (sampleFiniteRelationFromMondrian2D paintingPietMondrian1921 20)
  let rels = map fst $ take 100 $ every 10 rels'
  return rels

plotPietPlusRelation :: Matplotlib
plotPietPlusRelation = plotRelation plotPietMondrian (head $ unsafePerformIO datasetRelations)

-- | Statistical model: infers the hyperparameters of a Mondrian by observing
-- relations generated from it.
inferMondrian ::
  Foldable t =>
  t (Map (Double, Double) Bool) ->
  Prob Double ->
  Double ->
  [(Double, Double)] ->
  Meas (Mondrian Double)
inferMondrian dataset base budget intervals = do
  mondrian <- sample $ randomMondrian base budget intervals
  let scoreRel rel =
        mapM
          (\(r, c) -> score $ likelihoodFromMondrian2D mondrian r c (rel ! (r, c)))
          (keys rel)
  mapM_ scoreRel dataset
  return mondrian

mhInference :: IO Matplotlib
mhInference = do
  dataset <- datasetRelations
  mws' <- mh 0.2 $ inferMondrian dataset uniform 4 [(0, 1), (0, 1)]
  let mws = take 1000 $ every 100 $ drop 100 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

mh1Inference :: IO Matplotlib
mh1Inference = do
  dataset <- datasetRelations
  mws' <- mh1 $ inferMondrian dataset uniform 4 [(0, 1), (0, 1)]
  let mws = take 500 $ every 1000 $ drop 10000 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

lwisInference :: IO Matplotlib
lwisInference = do
  dataset <- datasetRelations
  mws' <- weightedsamples $ inferMondrian dataset uniform 4 [(0, 1), (0, 1)]
  let mws = take 1000 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  return $ plotMondrian2D m

mhResult :: Matplotlib
mhResult = unsafePerformIO mhInference

main :: IO ()
main = do
  putStrLn "Plotting..."
  dataRel <- datasetRelations
  file "pietMondrian-plus-relation.svg" $ plotRelation plotPietMondrian $ head dataRel
  testInf <- mhInference
  -- testInf <- lwisInference
  file "mondrian-relation.svg" testInf
  putStrLn "Done."