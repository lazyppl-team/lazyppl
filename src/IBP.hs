module IBP where

import LazyPPL
import Distr
import Distr.IBP

import Data.List
import Data.Monoid

import qualified Numeric.Log 

{-- Demonstration of using Indian Buffet Process for feature finding.
    Following 
    A Nonparametric Bayesian Method for Inferring Features From Similarity Judgments
    Navarro and Griffiths. NeurIPS 2006.
--} 

{--
Some helpers for printing the groups of features. 
--}
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = [] 
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs else x:(removeDuplicates xs) 


print_feature_groups :: [[Dish]] -> [String] -> IO ()
print_feature_groups features names = do 
    let n = length names
    let fs = removeDuplicates $ concat features
    mapM_ (\j -> do 
                putStr ("Group " ++ show (j+1) ++ ":")
                putStrLn $ foldl (++) " " [ names!!i | i <- [0..(n-1)], elem (fs !! j) (features!!i) ]) 
        [0..(length fs-1)];




{--
An example application.
Additive clustering is a method for assigning features to a set of 
of objects using similarity data. The number of possible features is
unknown and inferred using the IBP. Each feature has an associated 
"saliency" weight indicating how much it contributes to the similarity 
coefficient. We also infer the saliency weights.   
--}

countries_dataset = 
 [[ ],
 [0.11],
 [0.06, 0.04], 
 [0.43, 0.06, 0.03],
 [0.06, 0.32, 0.04, 0.14],
 [0.02, 0.09, 0.70, 0.02, 0.03],
 [0.02, 0.59, 0.02, 0.14, 0.04, 0.10], 
 [0.69, 0.01, 0.26, 0.35, 0.03, 0.06, 0.03],
 [0.03, 0.32, 0.01, 0.04, 0.70, 0.04, 0.11, 0.01],
 [0.01, 0.12, 0.01, 0.04, 0.20, 0.03, 0.31, 0.01, 0.45],
 [0.42, 0.12, 0.01, 0.87, 0.09, 0.02, 0.17, 0.31, 0.05, 0.04],
 [0.51, 0.35, 0.55, 0.01, 0.13, 0.22, 0.02, 0.17, 0.05, 0.02, 0.03], 
 [0.02, 0.37, 0.58, 0.03, 0.04, 0.90, 0.20, 0.04, 0.04, 0.03, 0.04, 0.15],
 [0.30, 0.11, 0.42, 0.03, 0.06, 0.20, 0.12, 0.46, 0.02, 0.04, 0.01, 0.43, 0.20],
 [0.60, 0.12, 0.03, 0.55, 0.12, 0.01, 0.05, 0.45, 0.10, 0.03, 0.57, 0.08, 0.02, 0.12], 
 [0.01, 0.08, 0.01, 0.11, 0.15, 0.02, 0.29, 0.01, 0.31, 0.83, 0.08, 0.01, 0.02, 0.01, 0.03]]
 

countries_names = ["China ", "Cuba ", "Germany ", "Indonesia ", "Iraq ", "Italy ", "Jamaica ", "Japan ", "Libya ", "Nigeria ", "Philippines ", "Russia ", "Spain ", "United States ", "Vietnam ", "Zimbabwe "]



additive_clustering :: Double -> Double -> Double -> [[Double]] -> Meas [[Dish]]
additive_clustering alpha lambda1 lambda2 similarityData = do
    restaurant <- sample $ newRestaurant alpha 
    weights <- sample $ memoize (\d -> gamma lambda1 lambda2)
    let n = length similarityData
    features <-  mapM (\i -> sample $ newCustomer restaurant) [1..n]  
    let similarity :: Int -> Int -> Double
        similarity i j = sum [weights a | a <- features!!j, elem a (features!!i)]
    mapM (\(i, j) -> score $ normalPdf (similarityData!!i!!j) 0.1 (similarity i j)) [ (i, j) | i <- [0..(n-1)], j <- [0..(i-1)] ]
    return features




main :: IO () 
main = do 
    everything <- mh 0.03 (additive_clustering 3 2 0.5 countries_dataset)
    let samples = take 20000 $ everything
    let maxw = (maximum $ map snd samples :: Product (Numeric.Log.Log Double))
    let (Just xyc) = Data.List.lookup maxw $ map (\(z,w) -> (w,z)) samples
    print xyc
    print_feature_groups xyc countries_names  

{-- Prints something like:
Group 1: Iraq Libya 
Group 2: Nigeria 
Group 3: Russia 
Group 4: Cuba Germany Italy Jamaica Russia Spain United States 
Group 5: Japan United States 
Group 6: China Indonesia Japan Philippines Vietnam 
Group 7: Jamaica Libya Nigeria Zimbabwe 
Group 8: Nigeria Zimbabwe 
--}