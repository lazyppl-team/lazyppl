module TestingAD where

import AD
import LazyPPL
import Data.Number.Erf
import Control.Monad
import Control.Monad.Extra
import Graphics.Matplotlib hiding (density)
import Data.Map (empty) 

normal :: InvErf d => d -> d -> Prob d d
normal m s = do 
  x <- uniform
  return $ (invnormcdf x + m) * s

normalPdf :: Floating d => d -> d -> d -> d
normalPdf m s x = let x' = (x - m)/s in  exp (negate (x' * x') / 2) / (sqrt (2 * pi)*s)

normalLogPdf :: Floating d => d -> d -> d -> d
normalLogPdf m s x = let x' = (x - m)/s in negate (x' * x') / 2 - (log (sqrt (2 * pi)*s))


normalImproper :: InvErf d => d -> d -> Meas d d
normalImproper m s = do 
  -- If uniform is used just as an initialization point,
  -- shift it a bit so it is roughly in the range of the normal,
  -- and then use score to actually put in the normal weight.
  -- You need to remove the clamp on gradientOptimize for this to work. 
  x <- (m + 0.5 - ) <$> sample uniform 
  scoreLog $ normalLogPdf m s x
  return $ x


dataset :: Floating d => [(d,d)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]
--dataset :: []
--dataset = map (\(x,y) -> (fromRational x,fromRational y)) [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]


-- perform Bayesian regression, with noise sigma.
-- prior is a distribution over printable labels (a) and functions (b->d).
-- the data set is a list of input/output (b,d) pairs. 
regress :: Floating d => d -> Meas d (a,(b -> d)) -> [(b, d)] -> Meas d a
regress sigma prior dataset =
  do
    (label,f) <- prior
--    forM_ dataset (\(x, y) -> score $ normalPdf (f x) sigma y)
    forM_ dataset (\(x, y) -> scoreLog $ normalLogPdf (f x) sigma y)
    return label

randlinear :: InvErf d => Prob d ((d,d),(d -> d))
randlinear = do
  a <- normal 0 3
  b <- normal 0 3
  let f = \x -> a * x + b
  return ((a,b),f)

randlinearImproper :: InvErf d => Meas d ((d,d),(d -> d))
randlinearImproper = do
  a <- normalImproper 0 3
  b <- normalImproper 0 3
  let f = \x -> a * x + b
  return ((a,b),f)

  
testA = do
  (a,b) <- gradientAscent 500 0.000001 $ (\(a,b) -> (primal a,primal b)) <$> regress 0.1 (sample randlinear) dataset
  print (a,b)
  plotFuns "images/adtest-A.svg" dataset [(\x -> a*x + b)] 1
  
testB = do
  (a,b) <- gradientAscent 500 0.0001 $ (\(a,b) -> (primal a,primal b)) <$> regress 0.1 randlinearImproper dataset
  print (a,b)
  plotFuns "images/adtest-B.svg" dataset [(\x -> a*x + b)] 1

  

-- Now for something non-parametric. A step function where the steps come from a point process.

-- Exponential distribution with given rate. 
exponential :: Floating d => d -> Prob d d
exponential rate = do 
  x <- uniform
  return $ - (log x / rate)

sigmoid x = let x' = x in (exp x') / (exp x' +1)

-- A step function which steps up by one at each point in the point process
-- Using sigmoids so that there is smoothness.
-- Lookahead by one point but not all the way along the point process.
stepfn :: (Floating a,Ord a) => Prob d [a] -> Prob d (a -> a)
stepfn pointProcess = do
    xs <- pointProcess
    let h (a1 : a2 : _) x | x <= a1 = sigmoid (x - a1) + sigmoid (x - a2)
        h (a : as)      x | x > a   = sigmoid (x - a) + h as x
    return $ h xs
  
-- Poisson point process that extends infinitely in one dimension.
-- Defined by stepping using exponential distribution.
poissonPP :: Floating d => d -> d -> Prob d [d]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)


testC = do
  f <- gradientAscent 50000 0.00001 $ regress 0.1 (sample $ (\f -> (f,f)) <$> stepfn (poissonPP 0 0.3)) dataset
--  f <- gradientAscent 50000 0.0000001 $ regress 0.1 (sample $ (\x -> (x,x)) <$> (sigmoidsplice (poissonPP 0 0.1) randConst)) dataset
  plotFuns "images/adtest-C.svg" dataset [deNagata f] 1


deNagata f x = let N y _ = f (N x empty) in y


-- Plot the points drawn from weighted samples
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interestingPoints :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interestingPoints f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon
    then acc
    else interestingPoints f lower mid epsilon delta (mid : (interestingPoints f mid upper epsilon delta acc))

sampleFun f =
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interestingPoints f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs

plotFuns :: String -> [(Double,Double)] -> [Double -> Double] -> Double -> IO ()
plotFuns filename dataset funs alpha = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a f -> let xfs = sampleFun f in a % plot (map fst xfs) (map snd xfs) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" alpha, o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim (-2 :: Int) (10 :: Int)) funs
        putStrLn "Done."
        return ()


{--
-- This doesn't work: the branch points don't appear at all in the tangent numbers, 
-- and so the function appears to not depend on the branch points.
--} 

splice :: Ord a => Prob d [a] -> Prob d (a -> b) -> Prob d (a -> b)
splice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let h [] x = default_f x
        h ((a, f) : xfs) x | x <= a = f x
        h ((a, f) : xfs) x | x > a = h xfs x
    return (h (zip xs fs))

randConst :: InvErf d => Prob d (d -> d)
randConst =
  do
    b <- normal 0 3
    let f = \x -> b
    return f

-- Sigmoid splice also didn't work but maybe just needs fine-tuning.

sigmoidsplice :: (Floating a , Ord a) => Prob d [a] -> Prob d (a -> a) -> Prob d (a -> a)
sigmoidsplice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let h [] x = default_f x
        h ((_,f1) : (a1,f2) : _) x | x <= a1 = (1 - (sigmoid (x-a1))) * (f1 x) + (sigmoid (x-a1)) * (f2 x)
        h ((_,f1) : (a1,f2) : (a2,f3) : _) x | x > a1 && x <= a2 = (1 - (sigmoid (x-a1))) * (f1 x) + 0.5 * ((sigmoid (x-a1)) * (f1 x) + (1-(sigmoid (x-a2))) * (f2 x)) + (sigmoid (x-a2)) * (f3 x)
        h (_ : (a1,f2) : (a2,f3) : xfs) x | x > a2 = h ((a1,f2) : (a2,f3) : xfs) x
    return $ (h (zip ((- 1/0) : xs) (default_f : fs)))

