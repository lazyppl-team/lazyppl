{- | Gaussian processes in the `LazyPPL` library.

Gaussian processes are random functions. We provide the Wiener process (`wiener`) and a generic Gaussian process (`gp`). 

Although the implementation uses hidden state, it is still safe, i.e. statistically commutative and discardable. 

For illustrations, see the [Gaussian process regression](https://lazyppl-team.github.io/GaussianProcessDemo.html) 
or [Wiener process regression](https://lazyppl-team.github.io/WienerDemo.html). The latter also illustrates composing the Wiener process function to get jump diffusion.
-}


module LazyPPL.Distributions.GP (wiener, gp, GPError(..)) where

import LazyPPL (Prob,Tree(Tree),Prob(Prob),runProb)
import LazyPPL.Distributions (normal,iid)
import LazyPPL.Distributions.GP.Pure (GPError(..), emptyGPState, sampleGP)
import qualified Data.Map
import Control.Concurrent.MVar (modifyMVar, newMVar)
import System.IO.Unsafe
import Prelude hiding ((<>))

{-| [Wiener process](https://en.wikipedia.org/wiki/Wiener_process) (Brownian motion).

This is a random function. 

The Wiener process is implemented by using a Brownian bridge and a hidden memo table.
A query reads the table, draws and stores under one lock, so concurrent queries
condition on each other; if a query raises an error, the table is unchanged.
The returned function holds hidden state: see 'LazyPPL.runProb' for its use outside a model. -}
wiener :: Prob (Double -> Double)
wiener = Prob $ \(Tree _ gs) ->
  unsafePerformIO $ do
    ref <- newMVar (Data.Map.singleton 0 0)
    return $ \x -> unsafePerformIO $ modifyMVar ref $ \table ->
      if isNaN x || isInfinite x then error ("wiener: the time must be finite, got " ++ show x) else
      case Data.Map.lookup x table of
        Just y -> return (table, y)
        Nothing -> do
          let m = bridge (Data.Map.lookupLT x table) x (Data.Map.lookupGT x table)
              y = runProb m (gs !! (1 + Data.Map.size table))
          y `seq` return (Data.Map.insert x y table, y)

-- | The value at time y given its nearest neighbours in the table: the
-- conditional mean plus the conditional standard deviation times a standard
-- normal draw. It reads the same label of the tree as normal mean sd would, so
-- each point keeps its subtree.
bridge :: Maybe (Double,Double) -> Double -> Maybe (Double,Double) -> Prob Double
bridge lower y upper = Prob $ \t -> mean + sd * runProb (normal 0 1) t
  where
    (mean, sd) = case (lower, upper) of
      (Just (x,x'), Nothing) -> (x', sqrt (y - x))
      (Nothing, Just (z,z')) -> (z', sqrt (z - y))
      -- Between x < y < z the variance is left * right / width, with left = y - x,
      -- right = z - y and width = z - x. As sqrt (min left right) times
      -- sqrt (max left right / width), whose second factor lies between sqrt (1/2)
      -- and 1, the standard deviation is representable whenever its true value is, even
      -- where the variance or left / width is not. The mean moves from the nearer
      -- neighbour by that neighbour's share of the width, at most 1/2.
      (Just (x,x'), Just (z,z')) ->
        let left = y - x
            right = z - y
            width = z - x
        in ( if left <= right then x' + (left / width) * (z' - x') else z' - (right / width) * (z' - x')
           , sqrt (min left right) * sqrt (max left right / width) )
      -- The table always holds the point 0.
      (Nothing, Nothing) -> error "wiener: empty table"

{-| [Gaussian process](https://en.wikipedia.org/wiki/Gaussian_process) with given covariance function.

This is a random function.

The function is defined by using linear algebra and a hidden memo table.

At distinct points the values have covariance
@C(x,y) = cov(x,y) + 2^-33 * cov(x,x) * [x == y]@, and a repeated query returns
the same value. The covariance function must be symmetric, which is not
checked. A point with zero variance returns 0, and its covariance with every
other queried point must be 0.

A covariance value that is NaN or infinite raises 'GPCovarianceNotFinite'. Any
other failure, including an overflow or a NaN in the
[Cholesky factorization](https://en.wikipedia.org/wiki/Cholesky_decomposition)
of @C@, raises 'GPCovarianceNotPositiveDefinite'. A query reads the table,
draws and stores under one lock, so concurrent queries condition on each other;
a failed query leaves the table unchanged. The returned function holds hidden
state: see 'LazyPPL.runProb' for its use outside a model. -}
gp :: (Double -> Double -> Double) -- ^ Covariance function
   -> Prob (Double -> Double) -- ^ Returns a random function
gp cov = do
  ns <- iid (normal 0 1)
  return $ unsafePerformIO $ do
    ref <- newMVar (Data.Map.empty, emptyGPState)
    return $ \x -> unsafePerformIO $ modifyMVar ref $ \(table, state) ->
      case Data.Map.lookup x table of
        Just value -> return ((table, state), value)
        Nothing -> do
          let seed = ns !! (1 + Data.Map.size table)
              (value, next) = sampleGP cov state x seed
          -- Force sampleGP's checks inside modifyMVar, so a failed query changes nothing.
          value `seq` return ((Data.Map.insert x value table, next), value)
