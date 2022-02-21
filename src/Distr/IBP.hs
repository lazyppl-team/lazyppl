{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distr.IBP (Restaurant,Dish,newCustomer,newRestaurant) where

import LazyPPL
import Distr
import Distr.Counter
import Distr.Memoization

import Data.List

{-- 
An implementation of the indian buffet process. 
Griffiths and Ghahramani. 
Infinite latent feature models and the Indian buffet process. NeurIPS 2006.

Using abstract types here:
e.g. 
S. Staton, H. Yang, N. L.. Ackerman, C. Freer, D. Roy. 
Exchangeable random process and data abstraction. 
Workshop on probabilistic programming semantics (PPS 2017).

--} 

 
-- Some abstract types 
newtype Restaurant = R ([[Bool]], Counter)  
newtype Dish = D Int  deriving (Eq,Ord,Show,MonadMemo Prob)

newCustomer :: Restaurant -> Prob [Dish]
newCustomer (R (matrix, ref)) = do 
    i <- readAndIncrement ref 
    return [ D k | k <- [0..(length (matrix!!i) - 1)], matrix!!i!!k ]

            
newRestaurant :: Double -> Prob Restaurant 
newRestaurant alpha = do
        r <- uniform 
        ref <- newCounter
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
Another possible implementation of the indian buffet process 
which uses a truncated stickbreaking construction. 
It is only an approximation to the true IBP, but doesn't need IO.   

See also 
Stick-breaking Construction for the Indian Buffet Process
Teh, Gorur, Ghahramani. AISTATS 2007.

A stochastic programming perspective on nonparametric Bayes
Daniel M. Roy, Vikash Mansinghka, Noah Goodman, and Joshua Tenenbaum
ICML Workshop on Nonparametric Bayesian, 2008. 
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



