
module IBP where

import LazyPPL
import Distr

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State.Lazy (State, state , put, get, runState)

import Data.Default.Class
import Control.Lens
import Data.Monoid
import System.Random

import System.IO.Unsafe
import Data.IORef

import qualified Numeric.Log 


{-- 
An implementation of the indian buffet process. 
--} 

 
-- Some abstract types 
data Restaurant = R ([[Bool]], IORef Int)  
data Dish = D Int  deriving (Eq,Ord,Show)

newCustomer :: Restaurant -> Prob [Dish]
newCustomer (R (matrix, ref)) = do 
    i <- readAndIncrement ref 
    return [ D k | k <- [0..(length (matrix!!i) - 1)], matrix!!i!!k ]

            
newRestaurant :: Double -> Prob Restaurant 
newRestaurant alpha = do
        -- s <- uniform 
        -- !() <- return $ unsafePerformIO (print $ "restaurant") -- ++ show s)
        r <- uniform 
        ref <- return $ ( unsafePerformIO (newIORef (round (r-r))))
        matrix <- ibp alpha  
        return $ R (matrix, ref) 


matrix :: Double -> Int -> [Int] -> Prob [[Bool]] 
matrix alpha index features = 
     do 
        let i = fromIntegral index
        existingDishes <- mapM (\m -> bernoulli ((fromIntegral m) / i)) features 
        let newFeatures = zipWith (\a -> \b -> if b then a + 1 else a) features existingDishes 
        nNewDishes     <- fmap fromIntegral $ poisson (alpha / i) 
        let fixZero = if features == [] && nNewDishes == 0 then 1 else nNewDishes  
        let newRow = existingDishes ++ (take fixZero $ repeat True) 
        rest           <- matrix alpha (index + 1) (newFeatures ++ (take fixZero $ repeat 1)) 
        return $ newRow : rest  

-- the distribution on matrices 
ibp :: Double -> Prob [[Bool]]  
ibp alpha = matrix alpha 1 [] 


{--
Some helpers for printing the groups of features. 
--}
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = [] 
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs else x:(removeDuplicates xs) 


print_feature_groups :: [[Dish]] -> [String] -> IO ()
print_feature_groups features names = do 
    let n = length names 
    let k = length $ removeDuplicates (concat features) 
    mapM_ (\f -> do 
                putStr ("Group " ++ show (f+1) ++ ":")
                putStrLn $ foldl (++) " " [ names!!i | i <- [0..(n-1)], elem (D f) (features!!i) ]) 
        [0..(k-1)];

{--
Another possible implementation of the indian buffet process 
which uses a truncated stickbreaking construction. 
It is only an approximation to the true IBP, but doesn't need IO.   
--}
data RestaurantS = RS [Double] 

data DishS = DS Int deriving (Eq,Ord,Show)

newCustomerS :: RestaurantS -> Prob [DishS]
newCustomerS (RS rs) = 
    do fs <- mapM bernoulli rs
       return $ map DS $ findIndices id fs

newRestaurantS :: Double -> Prob RestaurantS 
newRestaurantS a = fmap RS $ stickScale 1
  where stickScale p = do r' <- beta a 1
                          let r = p * r'
                          -- Truncate when the probabilities are getting small
                          rs <- if r < 0.01 then return [] else stickScale r
                          return $ r : rs





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

