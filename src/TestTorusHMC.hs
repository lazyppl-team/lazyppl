{-# LANGUAGE ExtendedDefaultRules #-}
module TestTorusHMC where
import LazyPPLTorusHMC
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
import System.Random (getStdGen)

import Debug.Trace

import Graphics.Matplotlib hiding (density)

normal :: InvErf d => d -> d -> Prob d d 
normal m s = do { x <- uniform ; return $ s * (invnormcdf x) + m }

normalPdf :: Floating d => d -> d -> d -> d
normalPdf m s x = let x' = (x - m)/s in  exp (negate (x' * x') / 2) / (sqrt (2 * pi)*s)

--
-- Random linear functions
--

linear :: InvErf d => Prob d (d -> d)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f

dataset :: Floating d => [(d, d)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

regress :: (Floating d,Show d,Show a) => d -> Prob d (a -> d) -> [(a, d)] -> Meas d (a -> d)
regress sigma prior dataset =
  do
    f <- sample prior
    forM_ dataset (\(x, y) -> scoreLog $ normalLogPdf (f x) sigma y)
    return f

plotLinReg =
  do fs' <- mh (malaKernel 0.000005) (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ fs'
     plotFuns "images/mala-linear-reg.svg" dataset fs 0.05
     fs' <- mh (grwKernel 0.003) (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ fs'
     plotFuns "images/grw-linear-reg.svg" dataset fs 0.05
     fs' <- mh (lmhKernel 0.5) (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ fs'
     plotFuns "images/lmh-linear-reg.svg" dataset fs 0.05



--
-- Random piecewise linear functions
--

exponential :: Erf d => d -> Prob d d
exponential rate = do 
  x <- uniform
  return $ - (log x / rate)

poissonPP :: Erf d => d -> d -> Prob d [d]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)


splice :: Ord d => Prob d [d] -> Prob d (d -> d) -> Prob d (d -> d)
splice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let -- h :: [(d, d -> d)] -> d -> d
        h [] x = default_f x
        h ((a, f) : xfs) x | x <= a = f x
        h ((a, f) : xfs) x | x > a = h xfs x
    return (h (zip xs fs))

randConst :: InvErf d => Prob d (d -> d)
randConst =
  do
    b <- normal 0 5
    let f = \x -> b
    return f

plotStepReg =
  do fs' <- mh (malaKernel 0.000005) (regress (toNagata 0.5) (splice (poissonPP 0 0.3) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ fs'
     plotFuns "images/mala-piecewiseconst-reg.svg" dataset fs 0.02
     fs' <- mh (grwKernel 0.003) (regress (toNagata 0.5) (splice (poissonPP 0 0.3) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ fs'
     plotFuns "images/grw-piecewiseconst-reg.svg" dataset fs 0.02
     fs' <- mh (lmhKernel 0.2) (regress (toNagata 0.5) (splice (poissonPP 0 0.3) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ fs'
     plotFuns "images/lmh-piecewiseconst-reg.svg" dataset fs 0.02 


--
-- Plotting routines
-- 

interestingPoints :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interestingPoints f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon
    then acc
    else interestingPoints f lower mid epsilon delta (mid : (interestingPoints f mid upper epsilon delta acc))

sampleFun f =
  let xs = ((-0.25) : (interestingPoints f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs


plotFuns :: String -> [(Double,Double)] -> [Double -> Double] -> Double -> IO ()
plotFuns filename dataset funs alpha = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a f -> let xfs = sampleFun f in a % plot (map fst xfs) (map snd xfs) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" alpha, o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim (-2 :: Int) (10 :: Int)) funs
        putStrLn "Done."
        return ()

plotHistogram :: (Show a , Eq a, Ord a) => String -> [a] -> IO ()
plotHistogram filename xs = do
  putStrLn $ "Generating " ++ filename ++ "..."
  let categories = sort $ nub xs
  let counts = map (\c -> length $ filter (==c) xs) categories
  file filename $ bar (map show categories) $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
  putStrLn $ "Done."


-- Tests to check we get uniform and normal distributions.


plotTestsNUTS :: IO ()
plotTestsNUTS = do
  xws <- mh (nutsKernel (LFConfig 0.5 7 0)) $ sample $ uniform
  let xws' = map (\(N x _) -> floor (x * 100)) xws
  plotHistogram "images/nuts/test-uniform.svg" (take 100000 xws')
  xws <- mh (nutsKernel (LFConfig 0.5 7 0)) $ sample $ normal 0 1
  let xws' = map (\(N x _) -> floor (x * 10)) xws
  plotHistogram "images/nuts/test-normal.svg" (take 100000 xws')

plotLinRegHMC (eps, steps) = 
  do fs' <- mh (hmcKernel (LFConfig eps steps 0)) (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 1000 $ fs'
     let name = "images/hmc/hmc-linear-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotLinRegHMCAll =
  do let configs = [(e, s) | e <- [0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05], s <- [1, 5, 10, 15, 20]]
     let configs = [(0.001, 10)]
     let x = map plotLinRegHMC configs
     sequence_ x

plotStepRegHMC (eps, steps, i) = 
  do fs' <- mh (hmcKernel (LFConfig eps steps 0)) (regress (toNagata 0.5) (splice (poissonPP 0 0.3) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 1000 $ drop 1 fs'
     let name = "images/hmc/hmc-piecewiseconst-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ "chain-" ++ show i ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegHMCAll =
  do g <- getStdGen
     let t = dualizeTree $ randomTree g
     let (_, N w dq) = runMeas (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) t
     --let (k, w) = runMeas (poissonPP 0 0.2) t
     --print w
     let configs = [(e, s) | e <- [0.0005, 0.001, 0.005, 0.01, 0.05], s <- [15, 20, 25]]
     --let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     let configs = [(0.0005, 20), (0.0005, 25), (0.001, 15), (0.001, 20), (0.001, 25)]
     let conf = [(x, y, i)| (x, y) <- configs, i <- [0..10]]
     let x = map plotStepRegHMC conf
     sequence_ x

plotStepRegLAHMC (eps, steps, chances, alpha, i) = 
  do fs' <- lahmcPersistence (lookaheadHMC chances) (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) (LFConfig eps steps 0) alpha
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ drop 0 fs'
     let y = map (\x -> map (fs!!x) [0..20]) $ 0:[100, 200..1900]
     print y
     let name = "images/hmc/lahmc-persistence-piecewiseconst-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ "chances-" ++ show chances ++ "alpha-" ++ show alpha ++ "("++ show i ++ ").png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegLAHMCAll =
  do let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [15, 20, 25, 30, 35, 40]]
     let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     let configs = [(0.005, 20, 3, 0.5, x) | x<- [1..50]]
     let x = map plotStepRegLAHMC configs
     sequence_ x
     

plotLinRegNUTS (eps, steps) = 
  do fs' <- mh (nutsKernel (LFConfig eps steps 0)) (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 1000 $ fs'
     let name = "images/nuts/nuts-linear-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotLinRegNUTSAll =
  do let configs = [(e, s) | e <- [0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05], s <- [1, 5, 10, 15, 20]]
     let configs = [(0.001, 5)]
     let x = map plotLinRegNUTS configs
     sequence_ x

plotStepRegNUTS (eps, steps, i) = 
  do fs' <- mh (nutsKernel (LFConfig eps steps 0)) (regress (toNagata 0.5) (splice (poissonPP 0 0.3) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 10000 $ drop 1 fs'
     let name = "images/nuts/nuts-piecewiseconst-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ "chain-" ++ show i ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegNUTSAll =
  do g <- getStdGen
     let t = dualizeTree $ randomTree g
     let (_, N w dq) = runMeas (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) t
     --let (k, w) = runMeas (poissonPP 0 0.2) t
     --print w
     let configs = [(e, s) | e <- [0.0005, 0.001, 0.005, 0.01, 0.05], s <- [4, 5, 6]]
     --let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     let configs = [(0.0005, 6), (0.001, 6)]
     let conf = [(x, y, i)| (x, y) <- configs, i <- [0..10]]
     let x = map plotStepRegNUTS conf
     sequence_ x

plotTests :: IO ()
plotTests = do
  let (mean, var) = (5, 1) 
  let n = 1000
  xws <- mh (hmcKernel (LFConfig 0.005 20 0)) $ sample $ uniform
  let xws' = map (\(N x _) -> floor (x * 100)) xws
  plotHistogram "images/hmc/test-uniform.svg" (take n xws')
  xws <- mh (hmcKernel (LFConfig 0.005 20 0)) $ sample $ normal mean var
  let xws' = map (\(N x _) -> floor (x * 10)) xws
  plotHistogram ("images/hmc/test-normal-mean"++ show (primal mean) ++ "var" ++ show (primal var) ++ ".svg") (take n xws')
  let (eps, steps, chances, alpha) = (0.005, 20, 3, 0.5)
  xws <- lahmcPersistence (lookaheadHMC chances) (sample uniform) (LFConfig eps steps 0) alpha
  let xws' = map (\(N x _) -> floor (x * 100)) xws
  plotHistogram "images/hmc/lahmc-test-uniform.svg" (take n xws')
  xws <- lahmcPersistence (lookaheadHMC chances) (sample (normal mean var)) (LFConfig eps steps 0) alpha
  let xws' = map (\(N x _) -> floor (x * 10)) xws
  plotHistogram ("images/hmc/lahmc-test-normal-mean"++ show (primal mean) ++ "var" ++ show (primal var) ++ ".svg") (take n xws')



main :: IO ()
--main = do { plotTests ; plotLinRegHMCALL ; plotStepRegHMCAll }
main = do plotTests
--main = do plotStepRegLAHMCAll
