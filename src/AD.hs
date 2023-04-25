module AD where

{- | This file defines a simple forward-mode version of AD.
-}

import Data.Map
import Data.Number.Erf

-- | Nagata number (aka, generalized dual number) whose second
--   component is the gradient vector represented as a sparse
--   map data structure from variable names to their partial
--   derivative. Absent entries are zero.
data Nagata v d = N { primal :: d, tangent :: (Map v d) }
  deriving Show

instance (Ord v, Ord d, Num d) => Num (Nagata v d) where
  fromInteger n   = N (fromInteger n) empty
  N x dx + N y dy = N (x + y) (unionWith (+) dx dy)
  N x dx - N y dy = N (x - y) (unionWith (+) dx (fmap negate dy))
  N x dx * N y dy = N (x * y) (unionWith (+) (fmap (y *) dx) (fmap (x *) dy))
  negate (N x dx) = N (negate x) (fmap negate dx)
  abs (N x dx) = N (abs x) (if x >= 0 then dx else fmap negate dx)
  signum = error "undefined"

instance (Ord v, Ord d, Fractional d) => Fractional (Nagata v d) where
  fromRational r  = N (fromRational r) empty 
  recip (N x dx)  = N (recip x) (fmap (recip (-x * x)*) dx) 
  N x dx / N y dy = N (x / y) (let z = y * y in unionWith (+) (fmap ((y / z) *) dx) (fmap ((-x / z) *) dy))

instance (Ord v, Ord d, Floating d) => Floating (Nagata v d) where
   pi = N pi empty
   exp (N x dx) = N (exp x) (fmap ((exp x) *) dx)
   log (N x dx) = N (log x) (fmap ((recip x) *) dx)
   sqrt (N x dx) = N (sqrt x) (fmap ((recip (2 * sqrt x)) *) dx)
   N x dx ** N y dy  = error "undefined"
   logBase x y = error "undefined"
   sin (N x dx) = N (sin x) (fmap ((cos x) *) dx) 
   cos (N x dx) = N (cos x) (fmap ((negate $ sin x) *) dx) 
   tan (N x dx) = N (tan x) (fmap ((recip $ cos x ** 2) *) dx)
   asin = error "undefined"
   acos = error "undefined"
   atan = error "undefined"
   sinh = error "undefined"
   cosh = error "undefined"
   tanh = error "undefined"
   asinh = error "undefined"
   acosh = error "undefined"
   atanh = error "undefined"

instance Eq d => Eq (Nagata v d) where
  (N x _) == (N y _) = x == y

instance Ord d => Ord (Nagata v d) where
  (N x _) <= (N y _) = x <= y

-- Standard Normal PDF
normpdf :: Floating d => d -> d
normpdf x = exp (negate (x * x) / 2) / (sqrt (2 * pi))

-- Probit function (inv cdf of normal)
instance (Ord v, Ord d, Floating d, InvErf d) => InvErf (Nagata v d) where
  invnormcdf (N x dx) = N (invnormcdf x) (fmap (/ (normpdf (invnormcdf x))) dx)

-- Probit function (inv cdf of normal)
instance (Ord v, Ord d, Floating d, Erf d) => Erf (Nagata v d) where
  normcdf (N x dx) = N (normcdf x) (fmap (* (normpdf x)) dx)

{-
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a
  {-# MINIMAL floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, isNaN, isInfinite, isDenormalized, isNegativeZero,
              isIEEE #-}
  	-- Defined in ‘GHC.Float’
-}
