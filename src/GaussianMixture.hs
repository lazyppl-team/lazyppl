{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module GaussianMixture where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
import LazyPPLHMC2 hiding (uniform, logSumExp, findIndex)

import System.Random hiding (uniform)

import Debug.Trace
import Data.List (findIndex)
import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants

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

uniformDiscrete :: (Erf d, Ord d) => Int -> Int -> Prob d Int
uniformDiscrete l u = do
    x <- uniform
    return $ fromIntegral $ l + myFloor (x* fromIntegral (u-l+1))

myFloor :: (Floating d, Ord d) => d -> Int
myFloor x = go 0
  where
    go n
      | fromIntegral n > x = n - 1
      | otherwise          = go (n + 1)

gammaQs :: (Floating d, Ord d) => d -> [d]
gammaQs x = qs
  where
    e = exp (-x)
    q1 = e
    terms = tail $ scanl (\t k -> t * x / fromIntegral k) e [1..]  -- e^{-x} x^n/n!
    qs = q1 : zipWith (+) qs terms

poisson :: (Show d, Floating d, Erf d, InvErf d, Ord d) => d -> Prob d Int
poisson rate = do
  x <- uniform
  --let cmf = map (\y -> 1 - incompleteGamma (fromIntegral (y + 1)) rate) [0,1..]
  --let cmf = trace (show x ++ show (invnormcdf x)) $ gammaQs rate
  let cmf = gammaQs rate
  let (Just n) = findIndex (> x) cmf
  --return $ trace (show n) $ fromIntegral n
  return $ fromIntegral n

linear :: Floating d => Prob d (d -> d)
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

multivariateNormal :: (Floating d, Ord d) => d -> d -> Int -> Prob d [d]
multivariateNormal mean sigma dims = do
    x <- replicateM dims stdnormal
    return $ map (\t -> mean + t * sigma) x

uniformMeans :: (Erf d, Floating d, Ord d) => d -> Int -> Prob d [d]
uniformMeans c dims = do
    x <- replicateM dims uniform
    return $ map (* c) x

multivariateNormalLogPdf :: (Floating d, Ord d) => [d] -> d -> [d] -> d
multivariateNormalLogPdf mean sigma x =
    Prelude.sum $ zipWith (\m xi -> normalLogPdf m sigma xi) mean x

gaussianMixture :: (Floating d, Ord d, Show d) => Int -> Int -> d -> (Int -> Prob d [d]) -> [[d]] -> Meas d [[d]]
gaussianMixture dims nbComp sigma prior dataset =
    do
        means <- replicateM nbComp (sample (prior dims))
        forM_ dataset (\datapoint -> scoreLog (logSumExp (map (\mean -> multivariateNormalLogPdf mean sigma datapoint) means) - log (fromIntegral nbComp)))
        return means

plotMixture :: String -> [[Double]] -> [[Double]]-> [[Double]]-> IO ()
plotMixture filename dataset true_means means =
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ (scatter (map head dataset) (map (\xs -> xs!!1) dataset) @@ [o2 "c" "pink", o2 "alpha" 0.08] % (scatter (map head true_means) (map (\xs -> xs!!1) true_means) @@ [o2 "c" "blue", o2 "alpha" 1.0]) % (scatter (map head means) (map (\xs -> xs!!1) means) @@ [o2 "c" "red", o2 "alpha" 1.0]))
        putStrLn "Done."
        return ()


gaussianMixtureModel :: (Floating d, Ord d, Show d) => Int -> d -> Prob d Int -> (Int -> Prob d [d]) -> [[d]] -> Meas d [[d]]
gaussianMixtureModel dims sigma priorNbComp priorMeans dataset =
    do
        k <- sample priorNbComp
        let nbComp = k + 1
        means <- replicateM nbComp (sample (priorMeans dims))
        forM_ dataset (\datapoint -> scoreLog (logSumExp (map (\mean -> multivariateNormalLogPdf mean sigma datapoint) means) - log (fromIntegral nbComp)))
        return means

gaussianMixtureModel2  :: (Floating d, Ord d, Show d) => Int -> d -> (Int -> Prob d [d]) -> [[d]] -> Meas d [[d]]
gaussianMixtureModel2 dims sigma priorMeans dataset =
    do
        let nbComp = 9
        means <- replicateM nbComp (sample (priorMeans dims))
        forM_ dataset (\datapoint -> scoreLog (logSumExp (map (\mean -> multivariateNormalLogPdf mean sigma datapoint) means) - log (fromIntegral nbComp)))
        return means

{- plotGaussianMixture = 
    do  
        --g <- newStdGen
        let nbComp = 1
        let true_means = [0]
        let prior_mean = 0
        let sigma = 1
        let dims = 1
        let n = 200
        -- change dataset to randomly choose between multiple components means
        let dataset = getDataset (mkStdGen 0) n (multivariateNormal true_means sigma dims)
        let nagataDataset = map (map toNagata) dataset
        print dataset
        means' <- mh (hmcKernel (LFConfig 0.005 20 0)) (gaussianMixture dims nbComp (toNagata sigma) (multivariateNormal prior_mean (toNagata sigma)) nagataDataset)
        let means = map fst means'
        --print $ (map (map primal) (means!!1000))
        plotMixture "images/hmc/gaussian-mixture.png" (map (\xs -> xs ++ [0]) dataset) (map (\xs -> xs ++ [0]) [[0]])  (map (\xs -> xs ++ [0]) (map (map primal) (concat (map (means!!) ([0..20] ++ [100, 200..1000])))))
 -}

getDataset g n f =
    let Tree x xs = randomTree g
    in map (\i -> fst (runMeas (sample f) (xs!!i))) [0..n-1]


runGaussianMixtureModel (eps, steps, count, burnin, dims, nbComp, trueMeans, trainDataset, testDataset, modelId, poRate, sigma, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        let nagataDataset = map (map toNagata) trainDataset
        print $ "start rep " ++ show rep
        let filename = "samples_produced/gmm/gmm_dims" ++ show dims ++ "_num_mix" ++ show nbComp ++ "_po_rate" ++ show (primal poRate) ++ "_std" ++ show sigma ++ "_id" ++ show modelId ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (gaussianMixtureModel dims (toNagata sigma) (poisson poRate) (uniformMeans (toNagata 100)) nagataDataset)
        let samples = take count $ drop burnin $ map fst fs
        let results = map (map (map primal)) samples
        results `deepseq` return ()
        -- jsonVal :: Value
        end <- getCPUTime
        let time = fromIntegral (end - start) / (10^12)
        print(time)
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
                            [ "po_rate" .= primal poRate,
                            "dims" .= dims,
                            "num_mix" .= nbComp,
                            "std" .= sigma
                            ],
                        "data_info" .= object
                            [ "test_data" .= testDataset,
                            "train_data" .= trainDataset,
                            "data_means" .= trueMeans
                            ],
                        "model_id" .= modelId
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)


runGaussianMixtureModelUniform (eps, steps, count, burnin, dims, nbComp, trueMeans, trainDataset, testDataset, modelId, poRate, sigma, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        let lower = 8
        let upper = 20
        let nagataDataset = map (map toNagata) trainDataset
        print $ "start rep " ++ show rep
        let filename = "samples_produced/gmm/gmmuniform_dims" ++ show dims ++ "_num_mix" ++ show nbComp ++ "_po_rate" ++ show (primal poRate) ++ "_std" ++ show sigma ++ "_id" ++ show modelId ++ "_l" ++ show lower ++ "_u" ++ show upper ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (gaussianMixtureModel dims (toNagata sigma) (uniformDiscrete lower upper) (uniformMeans (toNagata 100)) nagataDataset) 
        let samples = take count $ drop burnin $ map fst fs
        --let results = map (\xs -> map primal xs) samples
        let results = map (\xs -> map (\ys -> map primal ys) xs) samples
        results `deepseq` return ()
        -- jsonVal :: Value
        end <- getCPUTime
        --let alg = "lazyHMCmod"
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
                            [ "po_rate" .= primal poRate,
                            "dims" .= dims,
                            "num_mix" .= nbComp,
                            "std" .= sigma,
                            "l" .= lower, 
                            "u" .= upper
                            ],
                        "data_info" .= object
                            [ "test_data" .= testDataset,
                            "train_data" .= trainDataset,
                            "data_means" .= trueMeans
                            ],
                        "model_id" .= modelId
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)
        --print $ take 1 results


runGaussianMixtureModelAll =
    do
        let modelId = 0
        bs <- B.readFile "samples_produced/gmm/datasets/dims3_num_mix9_po_rate10_std10.0_id0.json"
        let parsed = decode bs :: Maybe [[[Double]]]
        case parsed of
            Just [trueMeans, trainData, testData] -> do
                print $ length trueMeans
                print $ length trainData
                let configs = [(0.05, 50, 1300, 0, 3, 9, trueMeans, trainData, testData, modelId, 10, 10, rep)| rep <- [0..9]]
                let configs = [(e, l, 700, 0, 3, 9, trueMeans, trainData, testData, modelId, 10, 10, rep)| rep <- [0..9], e <- [0.05, 0.01, 0.1], l <- [25, 50, 75]]
                let configs = [(0.05, 50, 10, 0, 3, 9, trueMeans, trainData, testData, modelId, 10, 10, rep)| rep <- [0..9]]
                let x = map runGaussianMixtureModel configs
                sequence_ x
            Just _ -> putStrLn "Unexpected JSON structure "
            Nothing -> putStrLn "Failed to parse JSON"

testPoisson =
    do
    let rate = 10
    print $ map (\y -> 1 - incompleteGamma (fromIntegral (y + 1)) rate) [0,1..20]
    print $ take 20 $ gammaQs rate

main :: IO ()
main = runGaussianMixtureModelAll