module TestingAD where

import AD
import LazyPPL
import Data.Number.Erf
import Control.Monad
import Control.Monad.Extra

normal :: InvErf d => d -> d -> Prob d d
normal m s = do 
  x <- uniform
  return $ (invnormcdf x + m) * s

normalPdf :: Floating d => d -> d -> d -> d
normalPdf m s x = let x' = (x - m)/s in  exp (negate (x' * x') / 2) / (sqrt (2 * pi)*s)

normalLogPdf :: Floating d => d -> d -> d -> d
normalLogPdf m s x = let x' = (x - m)/s in negate (x' * x') / 2 - (log (sqrt (2 * pi)*s))


dataset :: Floating d => [(d,d)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]
--dataset :: []
--dataset = map (\(x,y) -> (fromRational x,fromRational y)) [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]


-- perform Bayesian regression, with noise sigma.
-- prior is a distribution over printable labels (a) and functions (b->d).
-- the data set is a list of input/output (b,d) pairs. 
regress :: Floating d => d -> Prob d (a,(b -> d)) -> [(b, d)] -> Meas d a
regress sigma prior dataset =
  do
    (label,f) <- sample prior
--    forM_ dataset (\(x, y) -> score $ normalPdf (f x) sigma y)
    forM_ dataset (\(x, y) -> scoreLog $ normalLogPdf (f x) sigma y)
    return label

randlinear :: InvErf d => Prob d ((d,d),(d -> d))
randlinear = do
  a <- normal 0 3
  b <- normal 0 3
  let f = \x -> a * x + b
  return ((a,b),f)

  
test = gradientAscent 500 0.000001 $ (\(a,b) -> (primal a,primal b)) <$> regress 0.1 randlinear dataset
  


