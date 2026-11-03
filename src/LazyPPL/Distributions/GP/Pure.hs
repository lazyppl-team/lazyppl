{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module LazyPPL.Distributions.GP.Pure (GPError(..), GPState, emptyGPState, sampleGP) where

import Control.Exception (Exception, throw)
import Data.List (foldl')

-- | A 'GPError' holds the point that could not be sampled, and its constructor says why.
data GPError
  = GPCovarianceNotFinite Double
  | GPCovarianceNotPositiveDefinite Double
  deriving (Eq, Show)

instance Exception GPError

-- Rows, and each row's entries for earlier points, are newest first.
data Row = Row !Double !Double !Double [Double]

-- | The checked rows of the [Cholesky factor](https://en.wikipedia.org/wiki/Cholesky_decomposition),
-- and the zero-variance points, which have no row.
data GPState = GPState [Row] [Double]

-- | Nothing sampled yet.
emptyGPState :: GPState
emptyGPState = GPState [] []

-- | @sampleGP cov state x seed@ samples at a new point @x@ from an independent
-- standard normal draw @seed@. The values have covariance
-- @C(x,y) = cov(x,y) + 2^-33 * cov(x,x) * [x == y]@, and zero-variance points
-- get 0.
sampleGP :: (Double -> Double -> Double) -> GPState -> Double -> Double -> (Double, GPState)
sampleGP cov (GPState rows zeros) x seed
  | not (finite prior && finite diagonal) || any nonfiniteCross cross =
      throw (GPCovarianceNotFinite x)
  | prior < 0 || any invalidZero cross =
      throw (GPCovarianceNotPositiveDefinite x)
  | prior == 0 = (0, GPState rows (x:zeros))
  | pivot <= 0 || isNaN pivot =
      throw (GPCovarianceNotPositiveDefinite x)
  | otherwise = (sum (zipWith (*) lower seeds) + pivot*seed,
                 GPState (Row x seed pivot lower:rows) zeros)
  where
    prior = cov x x
    diagonal = prior + nugget * prior
    cross = [(False,cov x a,cov a x) | Row a _ _ _ <- rows] ++
            [(True,cov x a,cov a x) | a <- zeros]
    nonfiniteCross (_,a,b) = not (finite a && finite b)
    invalidZero (zero,a,b) = (prior == 0 || zero) && (a /= 0 || b /= 0)
    seeds = [z | Row _ z _ _ <- rows]
    lower = foldl' append [] (reverse rows)
    append previous (Row a _ d coefficients) =
      let !value = (cov x a - sum (zipWith (*) coefficients previous)) / d
      in value:previous
    pivot = sqrt (diagonal - sum (map (\a -> a*a) lower))

-- The nugget is a fixed fraction of each point's variance, so C does not
-- depend on the query order. It stops the factorization failing on nearly
-- dependent rows, as for an RBF kernel on a fine grid.
nugget :: Double
nugget = 2 ** (-33)

-- `lower` and `pivot` in sampleGP are one Cholesky-Banachiewicz step, forward
-- substitution against the earlier rows followed by a positive pivot. `matrix`
-- 0.3.6.4 has no incremental Cholesky API, so its `cholDecomp` would factor the
-- whole matrix again at each new point, O(m^3) work per query against O(m^2)
-- here, with m points already queried.

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
