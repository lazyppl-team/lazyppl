module Classify where

import LazyPPL
import Distr
import Data.List

dataset :: [Double]
dataset = [ 0.6, 0.7, 0.9, 1.2
          , 2.3, 2.4, 2.5
          ]

likelihood :: (Double,Bool) -> Meas ()
likelihood (x,True)  = score $ normalPdf 1 0.4 x
likelihood (x,False) = score $ normalPdf 2 0.4 x

cluster :: [Double] -> Meas [Bool]
cluster xs = do prior <- mapM (\x -> do { b <- sample (bernoulli 0.5);
                                          return (x,b)
                                        }) xs
                mapM likelihood prior
                return $ map snd prior

test1 = map fst . take 2000 . drop 100 <$> mh1 (cluster dataset)

test2 = map fst . take 2000 . drop 100 <$> mh 0.03 (cluster dataset)

collect :: Ord a => [a] -> [(a,Int)]
collect xs = map (\x -> (head x, length x)) $ group $ sort xs
