{-# LANGUAGE BangPatterns #-}
module MondrianExample where

import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.Mondrian
import Helpers
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
import qualified Data.Foldable as Foldable
import Data.Monoid (getProduct)
import Numeric.Log (ln)
import qualified Control.Monad.Trans.Writer as Writer
import Control.Parallel.Strategies (withStrategy, parList, rseq)
import qualified Data.List
import System.IO.Unsafe

-- | __Mondrian Process examples:__
mond :: Prob (Mondrian Double)
mond = randomMondrian uniform 5 [(0, 1), (0, 1)]

mond1 :: Mondrian Double
mond1 = fst $ head $ unsafePerformIO $ mh 0.2 (sample mond)

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
  Foldable t => t (Map (Double, Double) Bool) ->
  Prob Double -> Double -> [(Double, Double)] ->
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
inferMondrianMatrix size dataset base budget intervals =
  -- These cell lookups depend only on the fixed observations, not on a draw.
  let indices = [0 .. size]
      cells = [ [[rel !! r !! c | c <- indices] | r <- indices]
              | Matrix rel <- Foldable.toList dataset ]
      observationsReady = foldr (\rows next -> foldr (\row rest ->
        foldr seq rest row) next rows) () cells
  in do
    mondrian <- sample $ randomMondrian base budget intervals
    rs <- sample $ iid uniform
    cs <- sample $ iid uniform
    let xs = zipWith const rs indices
        ys = zipWith const cs indices
        evaluateWeight weight = do
          _ <- rseq (ln (getProduct weight))
          pure weight
    Meas $ Writer.WriterT $ Prob $ \source ->
      let factor likelihood = case score likelihood of
            Meas action -> snd (runProb (Writer.runWriterT action) source)
          -- A grid point has two fixed factors for this draw, one per truth value.
          grid = [ [(factor (likelihoodFromMondrian2D mondrian x y False),
                     factor (likelihoodFromMondrian2D mondrian x y True))
                   | y <- ys] | x <- xs ]
          forceGrid = foldr (\row rest -> foldr (\(no, yes) next ->
            ln (getProduct no) `seq` ln (getProduct yes) `seq` next) rest row) () grid
          rowWeight row factors = foldr (<>) mempty
            [if observed then yes else no | (observed, (no, yes)) <- zip row factors]
          datasetWeight rows = foldr (<>) mempty (zipWith rowWeight rows grid)
          sparked = withStrategy (parList evaluateWeight) (map datasetWeight cells)
          -- Preserve the original row and dataset product association.
          total = foldr (<>) mempty sparked
      in case cells of
           [] -> ((), mempty)
           _ -> observationsReady `seq` forceGrid `seq` ((), total)
    return mondrian
