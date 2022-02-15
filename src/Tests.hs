module Tests where
import LazyPPL hiding (exampleProb)
import Distr
import Data.List as L

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

test1 = do fs' <- mh1 10 $ sample exampleProb
           let fs = map fst $ take 10 $ fs'
           putStrLn $ show $ L.nub $ L.sort fs 
