{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module TestHMC2 where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
import LazyPPLHMC2 hiding (uniform)

import System.Random hiding (uniform)

import Debug.Trace

import Graphics.Matplotlib hiding (density)

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map

import System.CPUTime
import Control.DeepSeq (deepseq)

normal :: Floating d => d -> d -> Prob d d
--normal m s = do { x <- sample stdnormal ; scoreLog (normalLogPdf 0 1 x); return $ s * x + m }
normal m s = do { x <- stdnormal; return $ s * x + m }
uniform :: Erf d => Prob d d
uniform = do { x <- normal 0 1; return $ normcdf x}


linear :: Floating d => Prob d (d -> d)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f

simpleModel2 :: (Floating d, Ord d, Show d) => d ->  Meas d [d]
simpleModel2 alpha = 
  do 
    x <- sample $ normal 0 1
    y <- sample $ normal 0 1
    --t <- sample $ normal 0 1
    let z = if (x > 0.0) then [x, y] else [x] 
    score (if (x > 0.0) then alpha else 1) 
    return z

plotLinearPrior =
  do
    g <- getStdGen
    fs' <- mh g (grwKernel 0.2) 0 $ sample linear
    let fs = map (\f -> primal . f . toNagata) $ take 1000 $ every 100 $ map fst fs'
    plotFuns "images/mala-linear-prior.png" [] fs 0.1

dataset :: Floating d => [(d, d)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

plotDataset =
  do
    plotFuns "images/regression-dataset.png" dataset [] 0.1

normalPdf :: Floating d => d -> d -> d -> d
normalPdf m s x = let x' = (x - m)/s in  exp (negate (x' * x') / 2) / (sqrt (2 * pi)*s)

--normalLogPdf :: Floating d => d -> d -> d -> d
--normalLogPdf m s x = let x' = (x - m)/s in negate (x' * x') / 2 - (log (sqrt (2 * pi)*s))

regress :: (Floating d,Show d,Show a) => d -> Prob d (a -> d) -> [(a, d)] -> Meas d (a -> d)
regress sigma prior dataset =
  do
    f <- sample prior
    --forM_ dataset (\(x, y) -> scoreLog $ traceShow (normalLogPdf (f x) sigma y,x,y) $ normalLogPdf (f x) sigma y)
    forM_ dataset (\(x, y) -> scoreLog $ (normalLogPdf (f x) sigma y))
    return f


plotLinReg =
  do g <- getStdGen
     fs' <- mh g (hmcKernel (LFConfig 0.001 10 0)) 0 (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 1000 $ map fst fs'
     plotFuns "images/mala/hmc2-linear-reg.png" dataset fs 0.05
     fs' <- mh g (malaKernel 0.0005) 0 (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ map fst fs'
     plotFuns "images/mala/mala2-linear-reg.png" dataset fs 0.05
     fs' <- mh g (grwKernel 0.1) 0 (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ map fst fs'
     plotFuns "images/mala/grw-linear-reg.png" dataset fs 0.05
     fs' <- mh g (lmhKernel 0.5) 0 (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 500 $ map fst fs'
     plotFuns "images/mala/lmh-linear-reg.png" dataset fs 0.05
     
  
plotLinRegHMC (eps, steps) = 
  do g <- getStdGen
     fs' <- mh g (hmcKernel (LFConfig eps steps 0)) 0 (regress (toNagata 0.5) linear dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 1000 $ map fst fs'
     let name = "images/hmc2/hmc-linear-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotLinRegHMCAll =
  do let configs = [(e, s) | e <- [0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05], s <- [1, 5, 10, 15, 20]]
     let configs = [(0.001, 10)]
     let x = map plotLinRegHMC configs
     sequence_ x


exponential :: Erf d => d -> Prob d d
exponential rate = do 
  x <- uniform
  return $ - (log x / rate)

-- Poisson point process that extends infinitely in one dimension.
-- Defined by stepping using exponential distribution.
poissonPP :: (Erf d, Show d) => d -> d -> Prob d [d]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)


splice :: (Ord d, Num d, Show d) => Prob d [d] -> Prob d (d -> d) -> Prob d (d -> d)
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

randConst :: Floating d => Prob d (d -> d)
randConst =
  do
    b <- normal 0 3
    let f = \x -> b
    return f

plotStepReg =
  -- eps, steps = (0.0005, 25), (0.005, 30), (0.005, 20)
  do g <- getStdGen
     fs' <- mh g (hmcKernel (LFConfig 0.005 30 0)) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ map fst fs'
     plotFuns "images/mala/hmc2-piecewiseconst-reg.png" dataset fs 0.02
     fs' <- mh g (malaKernel 0.0005) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ map fst fs'
     plotFuns "images/mala/mala2-piecewiseconst-reg.png" dataset fs 0.02
     fs' <- mh g (grwKernel 0.1) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ map fst fs'
     plotFuns "images/mala/grw-piecewiseconst-reg.png" dataset fs 0.01
     fs' <- mh g (lmhKernel 0.5) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ map fst fs'
     plotFuns "images/mala/lmh-piecewiseconst-reg.png" dataset fs 0.02
     

plotStepRegHMC (eps, steps) = 
  do g <- getStdGen
     fs' <- mh g (hmcKernel (LFConfig eps steps 0)) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ drop 1 $ map fst fs'
     let name = "images/hmc2/hmc-piecewiseconst-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ ".png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegHMCAll =
  do g <- getStdGen
     let t = dualizeTree $ randomTree g
     let (_, N w dq) = runMeas (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) t
     --let (k, w) = runMeas (poissonPP 0 0.2) t
     --print w
     let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [15, 20, 25, 30, 35, 40]]
     let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     let configs = [(0.05, 20)]
     let x = map plotStepRegHMC configs
     sequence_ x


-- eps = 0.05, steps = 40
plotStepRegLAHMC (eps, steps, chances, alpha, i) = 
  do fs' <- lahmcPersistence (lookaheadHMC chances) (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) (LFConfig eps steps 0) alpha
     let fs = map (\f -> primal . f . toNagata) $ take 5000 $ drop 0 fs'
     let y = map (\x -> map (fs!!x) [0..20]) $ 0:[100, 200..1900]
     print y
     let name = "images/hmc2/lahmc-persistence-piecewiseconst-reg-eps-" ++ show eps ++ "steps-" ++ show steps ++ "chances-" ++ show chances ++ "alpha-" ++ show alpha ++ "("++ show i ++ ").png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegLAHMCAll =
  do let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [15, 20, 25, 30, 35, 40]]
     let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     --let configs = [(0.005, 20, 3, 0.5, x) | x<- [1..10]]
     let configs = [(0.05, 50, 9, 0.5, x) | x<- [1..3]]
     let x = map plotStepRegLAHMC configs
     sequence_ x

plotStepRegNUTS (eps, depth, i) = 
  do 
     g <- getStdGen
     fs' <- mh g (nutsKernel (LFConfig eps depth 0)) 0 (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset)
     let fs = map (\f -> primal . f . toNagata) $ take 2000 $ drop 1 $ map fst fs'
     let name = "images/hmc2/nuts-piecewiseconst-reg-eps-" ++ show eps ++ "depth-" ++ show depth ++ "("++ show i ++ ").png"
     --print ("done with eps: " ++ show eps ++ " steps: " ++ show steps)
     plotFuns name dataset fs 0.02 

plotStepRegNUTSAll =
  do
  --do g <- getStdGen
     --let t = dualizeTree $ randomTree g
     --let (_, N w dq) = runMeas (regress (toNagata 0.5) (splice (poissonPP 0 0.2) randConst) dataset) t
     --let (k, w) = runMeas (poissonPP 0 0.2) t
     --print w
     let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [15, 20, 25, 30, 35, 40]]
     let configs = [ (0.005, 15),  (0.005, 20), (0.005, 25), (0.005, 30), (0.001, 15),  (0.001, 20), (0.001, 25), (0.001, 30)]
     let configs = [(0.05, 20)]
     let configs' = [(0.05, 6, x) | x<- [1..10]]
     let x = map plotStepRegNUTS configs'
     sequence_ x



simpleModelHMC (eps, steps, alpha, rep) =  
  do 
     --g <- getStdGen
     let g = mkStdGen rep
     --let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) 0)
     let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) 0)

     let t = dualizeTree $randomTree g
     --let r = runProb simpleModel t
     --let k = runMeas (simpleModel2 alpha) t
     start <- getCPUTime
     let count = 2000
     let burnin = 0
     let filename = "samples_produced/test/test_model2_alpha" ++ show (primal alpha) ++ "-" ++ show rep ++ "_" ++ alg ++ "_count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
     print filename
     fs' <- alg_kernel (simpleModel2 alpha)
     --let fs = map (\f -> primal . f . toNagata) $ take 1000 $ map fst fs'
     --print $ take 5 fs'
     --print $ map length $ take 6000 $ map fst fs'
     let samples = take count $ drop 0 $ map fst fs'
     let results = map (\xs -> map primal xs) samples
     results `deepseq` return ()
     -- jsonVal :: Value
     end <- getCPUTime
     let time = fromIntegral (end - start) / (10^12)
     let jsonVal = object
          [ "metadata" .= object
              [ "alg" .= String alg2,
                "eps" .= eps,
                "leapfrog_steps" .= steps,
                "count" .= count,
                "burnin" .= burnin
              ],
            alg2 .= object
              [ "eps" .= eps,
                "leapfrog_steps" .= steps,
                "burnin" .= burnin,
                "time" .= time,
                "samples" .= results,
                "model_info" .= object
                  [ "model_hyperparam" .= object
                      [ "alpha" .= primal alpha
                      ],
                    "data_info" .= object []
                  ]
              ]
          ]
     B.writeFile filename (encode jsonVal)
     print $ "eps" ++ show eps ++ "L" ++ show steps 
     --print $ map (\xs -> map primal xs) $ drop 5900 samples
     print $ map length $ drop 5900 samples
     print $ length $ filter (\xs -> length xs == 2) $ samples

simpleModelHMCAll = 
  do let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [5, 10, 15, 20]]
     let configs = [(0.005, 20), (0.01, 5), (0.01, 10), (0.01, 15), (0.01, 20), (0.05, 5), (0.05, 10), (0.05, 15), (0.05, 20), (0.1, 5), (0.1, 10), (0.1, 15), (0.1, 20)]
     let configs = [(0.2, 5, 0.5, rep)| rep <- [0..9]] ++ [(0.2, 5, 1, rep)| rep <- [0..9]] ++ [(0.2, 5, 2, rep)| rep <- [0..9]]
     let x = map simpleModelHMC configs
     sequence_ x


simpleModelNUTS :: Show d => (Double, Int, Nagata Integer Double, d) -> IO ()
simpleModelNUTS (eps, steps, alpha, rep) =  
  do newStdGen
     g <- getStdGen
     let t = dualizeTree $randomTree g
     --let r = runProb simpleModel t
     let k = runMeas (simpleModel2 alpha) t
     --print r
     print k 
     start <- getCPUTime
     let count = 2000
     let burnin = 0
     fs' <- mh g (nutsKernel (LFConfig eps steps 0)) 0 (simpleModel2 alpha)
     --let fs = map (\f -> primal . f . toNagata) $ take 1000 $ map fst fs'
     --print $ take 5 fs'
     --print $ map length $ take 6000 $ map fst fs'
     let samples = take count $ drop 0 $ map fst fs'
     let results = map (\xs -> map primal xs) samples
     results `deepseq` return ()
     -- jsonVal :: Value
     end <- getCPUTime
     let time = fromIntegral (end - start) / (10^12)
     let jsonVal = object
          [ "metadata" .= object
              [ "alg" .= String "LazyNUTS",
                "eps" .= eps,
                "leapfrog_steps" .= steps,
                "count" .= count,
                "burnin" .= burnin
              ],
            "LazyNUTS" .= object
              [ "eps" .= eps,
                "leapfrog_steps" .= steps,
                "burnin" .= burnin,
                "time" .= time,
                "samples" .= results,
                "model_info" .= object
                  [ "model_hyperparam" .= object
                      [ "alpha" .= primal alpha
                      ],
                    "data_info" .= object []
                  ]
              ]
          ]
     let filename = "samples_produced/test/test_model2_alpha" ++ show (primal alpha) ++ "-" ++ show rep ++ "_lazyNUTS__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
     B.writeFile filename (encode jsonVal)
     print $ "eps" ++ show eps ++ "L" ++ show steps 
     --print $ map (\xs -> map primal xs) $ drop 5900 samples
     print $ map length $ drop 5900 samples
     print $ length $ filter (\xs -> length xs == 2) $ samples

simpleModelNUTSAll = 
  do let configs = [(e, s) | e <- [0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5], s <- [5, 10, 15, 20]]
     let configs = [(0.005, 20), (0.01, 5), (0.01, 10), (0.01, 15), (0.01, 20), (0.05, 5), (0.05, 10), (0.05, 15), (0.05, 20), (0.1, 5), (0.1, 10), (0.1, 15), (0.1, 20)]
     let configs = [(0.2, 5, 0.5, rep)| rep <- [0..9]] ++ [(0.2, 5, 2, rep)| rep <- [0..9]] ++ [(0.2, 5, 1, rep)| rep <- [0..9]]
     let x = map simpleModelNUTS configs
     sequence_ x
    
{--

splice :: Prob [Double] -> Prob (Double -> Double) -> Prob (Double -> Double)
splice pointProcess randomFun =
  do
    xs <- pointProcess
    fs <- mapM (const randomFun) xs
    default_f <- randomFun
    let h :: [(Double, Double -> Double)] -> Double -> Double
        h [] x = default_f x
        h ((a, f) : xfs) x | x <= a = f x
        h ((a, f) : xfs) x | x > a = h xfs x
    return (h (zip xs fs))
\end{code}

Note that this second-order function works for any point process and for any random function. We will use the random linear function `linear`, and for a point process we will use the following Poisson point process, `poissonPP`. This generates an infinite random list of points, where the gaps between them are exponentially distributed.
\begin{code}
poissonPP :: Double -> Double -> Prob [Double]
poissonPP lower rate =
  do
    step <- exponential rate
    let x = lower + step
    xs <- poissonPP x rate
    return (x : xs)
\end{code}
Here are five draws from the process. Each draw is really an infinite set of points, but we have truncated the display to the viewport [0,20]. Laziness then takes care of truncating the infinite sequences appropriately.
![](images/regression-poissonpp.png)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotPoissonPP =
  do
    pws <- mh 1 $ sample $ poissonPP 0 0.1
    let ps = map (takeWhile (20>)) $ map fst $ take 5 $ map fst pws
    let filename = "images/regression-poissonpp.png"
    putStrLn $ "Plotting " ++ filename ++ "..."
    let myscatter mpl i = mpl % setSubplot i % scatter (ps !! i) (map (const (0::Double)) (ps !! i)) @@ [o2 "s" (10::Int),o2 "c" "black"]  % xlim (0::Int) (20::Int) % ylim (-1 :: Int) (1::Int) % mp # "ax.yaxis.set_major_formatter(mticker.NullFormatter())"
    let myscatteraxes mpl i = if i < (length ps - 1) then myscatter mpl i % mp # "ax.xaxis.set_major_formatter(mticker.NullFormatter())" else myscatter mpl i
    file filename $ foldl myscatteraxes (subplots @@ [o2 "nrows" (length ps),o2 "ncols" (1::Int)]) [0..(length ps - 1)]
    putStrLn $ "Done."
\end{code}
</details>
<br></br>

We can now invoke a random piecewise linear function by calling `splice (poissonPP 0 0.1) linear`{.haskell}. 
Here are ten draws from this distribution. Because the viewport is bounded, laziness takes care of truncations to the point process that we passed to `splice`.

![](images/regression-piecewise-prior.png)

<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotPiecewisePrior =
  do
    fs' <- mh 1 $ sample $ splice (poissonPP 0 0.1) linear
    let fs = map fst $ take 10 $ map fst fs'
    plotFuns "images/regression-piecewise-prior.png" [] fs 1
\end{code}
</details>
<br></br>
This is a random function just like any other, so we can use it as a prior, using the same regression routines as before, `regress 0.1 (splice (poissonPP 0 0.1) linear)`{.haskell}. We can then sample from the unnormalized distribution using Metropolis-Hastings.
\begin{code}
plotPiecewiseReg =
  do
    fs' <- mhirreducible 0.2 0.1 (regress 0.1 (splice (poissonPP 0 0.1) linear) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 $ map fst fs'
    plotFuns "images/regression-piecewise-reg.png" dataset fs 0.01
\end{code}
![](images/regression-piecewise-reg.png)

Finally, we can also do our regression using piecewise constant functions. Our prior will now be `randConst`{.haskell}, a random linear function with slope 0.
\begin{code}
randConst :: Prob (Double -> Double)
randConst =
  do
    b <- normal 0 3
    let f = \x -> b
    return f
\end{code}
Using the same recipe as before, we can construct our prior by calling `splice (poissonPP 0 0.1) randConst`{.haskell} and perform inference on it to get the resultant unnormalized distribution of piecewise constant functions.
\begin{code}
plotPiecewiseConst =
  do
    fs' <- mhirreducible 0.2 0.1 (regress 0.1 (splice (poissonPP 0 0.1) randConst) dataset)
    let fs = map fst $ take 1000 $ every 1000 $ drop 10000 $ map fst fs'
    plotFuns "images/regression-piecewise-const.png" dataset fs 0.01
\end{code}
![](images/regression-piecewise-const.png)


<details class="code-details">
<summary>Graphing routines</summary>
\begin{code}--}
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

plotHistogram :: (Show a , Eq a, Ord a) => String -> [a] -> IO ()
plotHistogram filename xs = do
  putStrLn $ "Generating " ++ filename ++ "..."
  let categories = sort $ nub xs
  let counts = map (\c -> length $ filter (==c) xs) categories
  file filename $ bar (map show categories) $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
  putStrLn $ "Done."

plotTests :: IO ()
plotTests = do
  g <- getStdGen
  let (mean, var) = (0, 1) 
  let n = 10000
  -- hmc
  xws <- mh g (hmcKernel (LFConfig 0.05 10 0)) 0 $ sample $ uniform
  let xws' = map (\(N x _) -> floor (x * 100)) $ map fst xws
  plotHistogram "images/hmc2/test-uniform.svg" (take n xws')
  xws <- mh g (hmcKernel (LFConfig 0.05 10 0)) 0 $ sample $ normal mean var
  let xws' = map (\(N x _) -> floor (x * 10)) $ map fst xws
  plotHistogram ("images/hmc2/test-normal-mean"++ show (primal mean) ++ "var" ++ show (primal var) ++ ".svg") (take n xws')
  let (eps, steps, chances, alpha) = (0.005, 10, 3, 0.5)
  -- lahmc
  xws <- lahmcPersistence (lookaheadHMC chances) (sample uniform) (LFConfig eps steps 0) alpha
  let xws' = map (\(N x _) -> floor (x * 100)) xws
  plotHistogram "images/hmc2/lahmc-test-uniform.svg" (take n xws')
  xws <- lahmcPersistence (lookaheadHMC chances) (sample (normal mean var)) (LFConfig eps steps 0) alpha
  let xws' = map (\(N x _) -> floor (x * 10)) xws
  plotHistogram ("images/hmc2/lahmc-test-normal-mean"++ show (primal mean) ++ "var" ++ show (primal var) ++ ".svg") (take n xws')
  -- nuts
  xws <- mh g (nutsKernel (LFConfig 0.05 5 0)) 0 $ sample $ uniform
  print $ take 10 xws
  let xws' = map (\(N x _) -> floor (x * 100)) $ map fst xws
  plotHistogram "images/hmc2/nuts-test-uniform.svg" (take n xws')
  xws <- mh g (nutsKernel (LFConfig 0.05 5 0)) 0 $ sample $ normal mean var
  print $ take 10 xws
  let xws' = map (\(N x _) -> floor (x * 10)) $ map fst xws
  plotHistogram ("images/hmc2/nuts-test-normal-mean"++ show (primal mean) ++ "var" ++ show (primal var) ++ ".svg") (take n xws')
  


-- main :: IO ()
-- main = do {plotLinearPrior ; plotDataset ; plotLinReg ; plotPiecewisePrior ; plotPoissonPP ; plotPiecewiseReg ; plotPiecewiseConst }

main :: IO ()
main = simpleModelHMCAll