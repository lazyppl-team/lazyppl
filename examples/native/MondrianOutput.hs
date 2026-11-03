module MondrianOutput where
import MondrianExample
import MondrianPlotting
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.Mondrian
import Helpers
import Graphics.Matplotlib
import Data.Map (Map,empty,lookup,insert,size,keys,findWithDefault,fromList,(!))
import qualified Data.Foldable as Foldable
import Data.Monoid (getProduct)
import Numeric.Log (ln)
import qualified Control.Monad.Trans.Writer as Writer
import Control.Parallel.Strategies (withStrategy, parList, rseq)
import System.IO.Unsafe

plotPietMondrian :: Matplotlib
plotPietMondrian = plotMondrian2D paintingPietMondrian1921

mhInferenceMap :: IO Matplotlib
mhInferenceMap = do
  dataset <- datasetMapRelations
  mws' <- mh 0.2 $ inferMondrianMap dataset uniform 3 [(0, 1), (0, 1)]
  mws <- takeProgressEveryDrop 5000 100 100 mws'
  return $ plotMondrian2D (maxWeightElement mws)

mhInferenceMatrix :: Int -> IO Matplotlib
mhInferenceMatrix size = do
  dataset <- datasetMatrixRelations
  mws' <- mh (1/40) $ inferMondrianMatrix size dataset uniform 3 [(0, 1), (0, 1)]
  mws <- takeProgressEveryDrop 5000 1000 1000 mws'
  return $ plotMondrian2D (maxWeightElement mws)

lwisInference :: IO Matplotlib
lwisInference = do
  dataset <- datasetMapRelations
  mws' <- weightedsamples $ inferMondrianMap dataset uniform 4 [(0, 1), (0, 1)]
  let mws = take 1000 mws'
  return $ plotMondrian2D (maxWeightElement mws)

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
plotMond :: Matplotlib
plotMond = plotMondrian2D mond1

