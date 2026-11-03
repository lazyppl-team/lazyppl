{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| Abstract types for the [Dirichlet Process](https://en.wikipedia.org/wiki/Dirichlet_process) viewed through the interface of the [Chinese Restaurant Process](https://en.wikipedia.org/wiki/Chinese_restaurant_process).

Ideas following [S. Staton, H. Yang, N. L. Ackerman, C. Freer, D. Roy. Exchangeable random process and data abstraction. Workshop on probabilistic programming semantics (PPS 2017).](https://www.cs.ox.ac.uk/people/hongseok.yang/paper/pps17a.pdf)

Our implementation here uses stick breaking, with a lazily broken stick. Other urn-based implementations are possible with hidden state, and they should be observationally equivalent.

For illustrations, see [non-parametric clustering](https://lazyppl-team.github.io/ClusteringDemo.html) and [relational inference](https://lazyppl-team.github.io/IrmDemo.html).

-}

module LazyPPL.Distributions.DirichletP (
{- * Chinese Restaurant Process interface -}
{- | For clustering, we regard each data point as a "customer" in a "restaurant", and they are in the same cluster if they sit at the same `Table`. 
-}
Restaurant, Table, newRestaurant, newCustomer, 
-- * Random distribution interface
dp) where

import Data.Coerce (coerce)
import qualified Numeric.Sum as Sum
import LazyPPL
import LazyPPL.Distributions
import LazyPPL.Distributions.Memoization (MonadMemo)


-- | Abstract type of restaurants
newtype Restaurant = R [Double]

-- | Abstract type of tables. This supports `Eq` so that we can ask whether customers are at the same table (i.e. whether points are in the same cluster). 
newtype Table = T Int deriving (Eq, Show, MonadMemo Prob)

{-| A customer enters the restaurant and is assigned a table. -}
newCustomer :: Restaurant -> Prob Table
-- Coercing the newtype preserves categorical's random-tree allocation.
newCustomer (R restaurant) = coerce (categorical restaurant)

{-| Create a new restaurant with concentration parameter alpha, which may be any finite positive number; other values raise an error. -}
newRestaurant :: Double -- ^ Concentration parameter, alpha
              -> Prob Restaurant
newRestaurant alpha
  | not (concentration alpha) = error ("newRestaurant: the concentration alpha must be finite and positive, got " ++ show alpha)
  | otherwise = do
      sticks <- stickBreaking alpha Sum.zero
      return $ R sticks

-- | Whether alpha is a concentration that beta 1 alpha takes: finite and positive.
concentration :: Double -> Bool
concentration alpha = alpha > 0 && not (isInfinite alpha)

{- | Stick breaking breaks the unit interval into an
    infinite number of parts (lazily) --}
stickBreaking :: Double -> Sum.KBNSum -> Prob [Double]
stickBreaking alpha lower =
  do
    r <- beta 1 alpha
    let v = r * (1 - Sum.kbn lower)
    vs <- stickBreaking alpha (Sum.add lower v)
    return (v : vs)

{-| [Dirichlet Process](https://en.wikipedia.org/wiki/Dirichlet_process) as a random distribution. The concentration alpha may be any finite positive number; other values raise an error. -}
dp :: Double -- ^ Concentration parameter, alpha
   -> Prob a -- ^ Base distribution
   -> Prob (Prob a)
dp alpha p
  | not (concentration alpha) = error ("dp: the concentration alpha must be finite and positive, got " ++ show alpha)
  | otherwise = do
      xs <- iid p
      vs <- stickBreaking alpha Sum.zero
      -- Prob's fmap adds a tree split. Map this result on the same tree so the
      -- shared selector also preserves the existing fixed-seed distribution.
      return $ Prob $ \tree -> xs !! runProb (categorical vs) tree
