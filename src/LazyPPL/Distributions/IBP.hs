{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}

{-|
An implementation of the Indian buffet process by [Griffiths and Ghahramani](https://papers.nips.cc/paper_files/paper/2005/file/2ef35a8b78b572a47f56846acbeef5d3-Paper.pdf).

The restaurant types hide their implementation state, inspired by [Exchangeable Random Processes and Data Abstraction](https://www.cs.ox.ac.uk/people/hongseok.yang/paper/pps17a.pdf). The `Dish` constructor exposes the column index used by feature-matrix examples.

Illustration: [Feature extraction example](https://lazyppl-team.github.io/AdditiveClusteringDemo.html). 
-}

module LazyPPL.Distributions.IBP
  ( Restaurant, Dish(..), newCustomer, newRestaurant, ibp
  , RestaurantS, DishS, newCustomerS, newRestaurantS
  ) where

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


-- | A restaurant of the Indian buffet process with concentration alpha. Each 'newCustomer' takes the next row
-- of its matrix, so a restaurant holds hidden state: see 'LazyPPL.runProb' for its use outside a model.
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
        let newRow = existingDishes ++ take nNewDishes (repeat True)
        rest           <- matrix alpha (index + 1) (newFeatures ++ take nNewDishes (repeat 1))
        return $ newRow : rest

-- the distribution on matrices 
ibp :: Double -> Prob [[Bool]]
ibp alpha = matrix alpha 1 []



-- | A restaurant for the approximation 'newRestaurantS' to the Indian buffet process. It holds the
-- finite list of sticks, the probabilities with which a customer takes its dishes.
newtype RestaurantS = RS [Double]

-- | A dish of a 'RestaurantS' restaurant: one dish for each stick, in the order of the sticks.
newtype DishS = DS Int deriving (Eq,Ord,Show)

-- | A new customer of a 'RestaurantS' restaurant. It takes each dish independently, with the
-- probability given by the dish's stick.
newCustomerS :: RestaurantS -> Prob [DishS]
newCustomerS (RS rs) = do 
  fs <- mapM bernoulli rs
  return $ map DS $ findIndices id fs

{-| An /approximation/ to 'newRestaurant': the Indian buffet process built by stick breaking and cut
short. The concentration alpha may be any finite positive number; other values raise an error. Its
law is not the law of 'newRestaurant'; the difference is bounded as follows.

The full construction draws @nu_1, nu_2, ...@ independently from @Beta(alpha, 1)@ and forms the sticks
@r_1 = nu_1@, @r_2 = nu_1 * nu_2@, and so on. A customer takes the @k@-th dish with probability @r_k@,
independently for each @k@.

This function generates the sticks in order and stops at the first one below 0.01. That stick is kept
as the last one: every stick before it is at least 0.01, and it is below 0.01. The later sticks are
not generated.

The later sticks are the error. Given the last kept stick @r@, they are @r * nu'_1@,
@r * nu'_1 * nu'_2@, and so on, where the @nu'_j@ are further independent draws from @Beta(alpha, 1)@,
whose mean is @alpha / (alpha + 1)@. Each later stick is the probability of one dish, so the expected
number of dishes that this function leaves out of one customer's row, compared with the exact process,
is the expected sum of the later sticks:

> r * (alpha / (alpha + 1)) / (1 - alpha / (alpha + 1)) = alpha * r

As @r < 0.01@, that is fewer than @0.01 * alpha@ dishes. The bound grows with @alpha@: it is below 0.1
when @alpha = 10@ and below 10 when @alpha = 1000@. Use 'newRestaurant' when that error matters.

See also /Stick-breaking Construction for the Indian Buffet Process/, Teh, Gorur and Ghahramani,
AISTATS 2007, and /A stochastic programming perspective on nonparametric Bayes/, Roy, Mansinghka,
Goodman and Tenenbaum, ICML Workshop on Nonparametric Bayesian, 2008.
-}
newRestaurantS :: Double -> Prob RestaurantS
newRestaurantS a
  | not (a > 0 && not (isInfinite a)) = error ("newRestaurantS: the concentration alpha must be finite and positive, got " ++ show a)
  | otherwise = RS <$> stickScale 1
  where stickScale p = do r' <- beta a 1
                          let r = p * r'
                          -- Truncate when the probabilities are getting small
                          rs <- if r < 0.01 then return [] else stickScale r
                          return $ r : rs
