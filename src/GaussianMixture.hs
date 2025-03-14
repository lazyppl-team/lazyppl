{-# LANGUAGE ExtendedDefaultRules #-}
module GaussianMixture where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
import LazyPPLTorusHMC hiding (logSumExp)

import System.Random hiding (uniform)

import Debug.Trace

import Graphics.Matplotlib hiding (density)

normal :: (Floating d, InvErf d) => d -> d -> Prob d d
--normal m s = do { x <- sample stdnormal ; scoreLog (normalLogPdf 0 1 x); return $ s * x + m }
normal m s = do { x <- stdnormal; return $ s * x + m }
--uniform :: Erf d => Prob d d
--uniform = do { x <- normal 0 1; return $ normcdf x}


linear :: (Floating d, InvErf d) => Prob d (d -> d)
linear =
  do
    a <- normal 0 3
    b <- normal 0 3
    let f = \x -> a * x + b
    return f

logSumExp :: (Floating a, Ord a) => [a] -> a
logSumExp [] = error "logSumExp: empty list"
logSumExp xs = 
    let m = maximum xs
    in m + log (Prelude.sum (map (\x -> exp (x - m)) xs))

multivariateNormal :: (Floating d, Ord d, InvErf d) => [d] -> d -> Int -> Prob d [d]
multivariateNormal mean var dims = do
    x <- replicateM dims stdnormal
    return $ zipWith (+) mean (map (* sqrt var) x)

multivariateNormalLogPdf :: (Floating d, Ord d) => [d] -> d -> [d] -> d
multivariateNormalLogPdf mean var x =
    Prelude.sum $ zipWith (\m xi -> normalLogPdf m (sqrt var) xi) mean x

gaussianMixture :: (Floating d, Ord d, Show d) => Int -> Int -> d -> (Int -> Prob d [d]) -> [[d]] -> Meas d [[d]]
gaussianMixture dims nbComp sigma prior dataset = 
    do 
        means <- replicateM nbComp (sample (prior dims))
        forM_ dataset (\datapoint -> scoreLog (logSumExp (map (\mean -> multivariateNormalLogPdf mean sigma datapoint) means) - log (fromIntegral nbComp)))
        return means

plotHistogram :: (Show a , Eq a, Ord a) => String -> [a] -> IO ()
plotHistogram filename xs = do
  putStrLn $ "Generating " ++ filename ++ "..."
  let categories = sort $ nub xs
  let counts = map (\c -> length $ filter (==c) xs) categories
  file filename $ bar (map show categories) $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
  putStrLn $ "Done."

plotMixture :: String -> [[Double]] -> [[Double]]-> [[Double]]-> IO ()
plotMixture filename dataset true_means means = 
    do  let alpha1 = 0.08 :: Double
        let alpha2 = 0.08 :: Double
        putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ (scatter (map head dataset) (map (\xs -> xs!!1) dataset) @@ [o2 "c" "pink", o2 "alpha" alpha1] % (scatter (map head true_means) (map (\xs -> xs!!1) true_means) @@ [o2 "c" "blue", o2 "alpha" alpha2]) % (scatter (map head means) (map (\xs -> xs!!1) means) @@ [o2 "c" "red", o2 "alpha" alpha2]))
        putStrLn "Done."
        return ()


plotGaussianMixtureOneDim = 
    do  
        let nbComp = 1
        let true_means = [0]
        let prior_means = [11]
        let sigma = 1
        let dims = 1
        let dataset_size = 1
        -- change dataset to randomly choose between multiple components means
        let dataset = getDataset (mkStdGen 0) dataset_size (multivariateNormal true_means sigma dims)
        let nagataDataset = map (map toNagata) dataset
        print dataset
        means <- mh (hmcKernel (LFConfig 0.001 5 0)) (gaussianMixture dims nbComp (toNagata sigma) (multivariateNormal prior_means (toNagata sigma)) nagataDataset)
        let nb_samples = 100000
        xs  <- mh (nutsKernel (LFConfig 0.0001 5 0)) (gaussianMixture dims nbComp (toNagata sigma) (multivariateNormal prior_means (toNagata sigma)) nagataDataset)
        let meansNUTS = take nb_samples xs
        print $ (sum (map primal (concat (concat meansNUTS))))/(fromIntegral nb_samples)
        print $ normcdf (-7.00) 
        --print $ (map (map primal) (concat (take 100 (drop 900 means))))
        --plotMixture "images/hmc/gaussian-mixture.png" (map (\xs -> xs ++ [0]) dataset) (map (\xs -> xs ++ [0]) [[0]])  (map (\xs -> xs ++ [0]) (map (map primal) (means!!0 ++ means!!1000))) 
        plotMixture "images/nuts/gaussian-mixture.png" (map (\xs -> xs ++ [0]) dataset) (map (\xs -> xs ++ [0]) [[0]])  (map (\xs -> xs ++ [0]) (map (map primal) (concat meansNUTS)))
        plotHistogram "images/nuts/gaussian-mixture_hist.svg" (map (\x -> floor ((primal x)*10)) (concat (concat meansNUTS)))

getDataset g n f = 
    let Tree x xs = randomTree g 
    in map (\i -> fst (runMeas (sample f) (xs!!i))) [0..n-1]


simpleModel :: (Floating d, Ord d, Show d, InvErf d) => Prob d d
simpleModel = 
    do
        x <- uniform
        y <- normal 0 2
        z <- normal 20 2
        let t = if x > 0.5 then y else z
        return t

plotSimpleModel = 
    do
        xs <- mh (nutsKernel (LFConfig 0.001 5 0)) (sample simpleModel)
        let nb_samples = 100000
        let ys = take nb_samples xs
        let zs = map primal ys
        plotHistogram "images/nuts/simple-model.svg" (map (\x -> floor (x*10)) zs)


main :: IO ()
main = plotGaussianMixtureOneDim
--main = plotSimpleModel