{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use null" #-}

{-|
An implementation of the Indian buffet process by [Griffiths and Ghahramani](https://papers.nips.cc/paper_files/paper/2005/file/2ef35a8b78b572a47f56846acbeef5d3-Paper.pdf).

We are using abstract types to hide the implementation details, inspired by [Exchangeable Random Processes and Data Abstraction](https://www.cs.ox.ac.uk/people/hongseok.yang/paper/pps17a.pdf). 

Illustration: [Feature extraction example](https://lazyppl-team.github.io/AdditiveClusteringDemo.html). 
-}

module LazyPPL.Distributions.IBP where

import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.Counter
import LazyPPL.Distributions.Memoization

import Data.List



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
        existingDishes <- mapM (\m -> bernoulli (fromIntegral m / i)) features
        let newFeatures = zipWith (\ a b -> if b then a + 1 else a) features existingDishes
        nNewDishes     <- fromIntegral <$> poisson (alpha / i)
        let fixZero = if features == [] && nNewDishes == 0 then 1 else nNewDishes
        let newRow = existingDishes ++ take fixZero (repeat True)
        rest           <- matrix alpha (index + 1) (newFeatures ++ take fixZero (repeat 1))
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
newtype RestaurantS = RS [Double]

newtype DishS = DS Int deriving (Eq,Ord,Show)

newCustomerS :: RestaurantS -> Prob [DishS]
newCustomerS (RS rs) = do 
  fs <- mapM bernoulli rs
  return $ map DS $ findIndices id fs

newRestaurantS :: Double -> Prob RestaurantS
newRestaurantS a = RS <$> stickScale 1
  where stickScale p = do r' <- beta a 1
                          let r = p * r'
                          -- Truncate when the probabilities are getting small
                          rs <- if r < 0.01 then return [] else stickScale r
                          return $ r : rs



