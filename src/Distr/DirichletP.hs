{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distr.DirichletP (Restaurant, Table, newCustomer, newRestaurant, dp) where

import Data.List
import Data.Maybe
import Distr
import Distr.Memoization
import LazyPPL

{-| Abstract types for the CRP.
e.g.
S. Staton, H. Yang, N. L. Ackerman, C. Freer, D. Roy.
Exchangeable random process and data abstraction.
Workshop on probabilistic programming semantics (PPS 2017).
-}

newtype Restaurant = R [Double]

newtype Table = T Int deriving (Eq, Ord, Show, MonadMemo Prob)

newCustomer :: Restaurant -> Prob Table
newCustomer (R restaurant) =
  do
    r <- uniform
    return $ T $ fromJust $ findIndex (> r) (scanl1 (+) restaurant)

newRestaurant :: Double -> Prob Restaurant
newRestaurant alpha = do
  sticks <- stickBreaking alpha 0
  return $ R sticks

{- | Stick breaking breaks the unit interval into an
    infinite number of parts (lazily) --}
stickBreaking :: Double -> Double -> Prob [Double]
stickBreaking alpha lower =
  do
    r <- beta 1 alpha
    let v = r * (1 - lower)
    vs <- stickBreaking alpha (lower + v)
    return (v : vs)

{- | We can then define the Dirichlet Process --}
iid :: Prob a -> Prob [a]
iid p = do r <- p; rs <- iid p; return $ r : rs

dp :: Double -> Prob a -> Prob (Prob a)
dp alpha p = do
  xs <- iid p
  vs <- stickBreaking alpha 0
  return $ do
    r <- uniform
    return $ xs !! fromJust (findIndex (> r) (scanl1 (+) vs))
