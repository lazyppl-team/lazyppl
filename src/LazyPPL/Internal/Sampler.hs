{- | Rules that every sampler of the random tree shares: how a label is drawn,
and which weights inference accepts. This module is internal. -}
module LazyPPL.Internal.Sampler (randomLabel, checkedLogWeight, labelIndex) where

import Data.Bits (complement, shiftR)
import Data.Word (Word64)
import Numeric.Log (Log, ln)
import System.Random (RandomGen, genWord64)

{- | A label of the random tree, uniform on the open interval (0,1).

@random@ from random 1.3.1 gives a 'Double' in the closed interval [0,1], and
an inverse transform fails at an end point: @-log 0@ is infinite, and a
Bernoulli(1) coin compares a label of 1 with 1. With k the 52 high bits of
the complement of one 'Data.Word.Word64', this gives @(k + 1/2) / 2^52@, in
@[2^-53, 1 - 2^-53]@. It reads one word, as @random@ does, so every node of a
tree gets the same generator as before; both labels decrease as the word
grows, and they differ by at most @2^-52@. -}
randomLabel :: RandomGen g => g -> (Double, g)
randomLabel g =
  let (w, g') = genWord64 g
  in (encodeFloat (2 * toInteger (complement w `shiftR` 12) + 1) (-53), g')

{- | The 52-bit grid index of a label, the k of the midpoint grid
u = (k + 1/2) / 2^52 above. It is exact for every label 'randomLabel' draws:
u * 2^52 = k + 1/2 is exactly representable in binary64, and floor (k + 1/2)
is k. Labels of hand-built trees keep the index of the grid point at or below
them. -}
labelIndex :: Double -> Word64
labelIndex u = fromInteger (floor (u * 4503599627370496))

{- | A weight is a non-negative real number, kept as its logarithm so that it
may exceed the range of 'Double': the logarithm is finite, or negative
infinity for weight 0. A NaN or positive-infinite logarithm raises an error
that names the caller. -}
checkedLogWeight :: String -> Log Double -> Log Double
checkedLogWeight caller w
  | isNaN l || (l > 0 && isInfinite l) =
      error (caller ++ ": the weight must be non-negative and finite, but its logarithm is " ++ show l)
  | otherwise = w
  where l = ln w
