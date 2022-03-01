module Tests where
import LazyPPL hiding (exampleProb)
import Distr
import Data.List as L
import Distr.Mondrian

-- An example probability distribution.
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

bins :: [Double] -> Int -> [Int]
bins xs n = [ length [x | x <- xs
                        , (fromIntegral $ k-1)/(fromIntegral n) < x
                        , x <= (fromIntegral k)/(fromIntegral n) ] | k <- [1..n] ]

test1 = do fs' <- mh1 $ sample exampleProb
           let fs = map fst $ take 1000 $ fs'
           putStrLn $ show $ bins fs 10

accum :: [[Double]] -> [(Int,Int)]
accum xs = sortOn fst [ (i, length $ filter (==i) ls) | i <- [0..k] ]
  where
    ls = map length xs
    k = maximum ls

test2 = do xs' <- lwis 50000 $ sample $ oneDimMondrian 2.0 (0.0, 2.0)
           let xs = take 10000 $ every 50 $ drop 1000 $ xs'
           putStrLn $ show $ accum xs
