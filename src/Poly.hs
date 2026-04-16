{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Poly where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
--import LazyPPLHMC2 hiding (uniform, logSumExp, findIndex)
import LazyPPLHMC hiding (uniform, logSumExp, findIndex)

import System.Random hiding (uniform)

import Debug.Trace
import Data.List (findIndex)
import Numeric.SpecFunctions
import Numeric.MathFunctions.Constants

import Graphics.Matplotlib hiding (density)

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map

import System.Environment (getArgs)
import Text.Read (readMaybe)

import System.CPUTime
import Control.DeepSeq (deepseq)

normal :: Floating d => d -> d -> Prob d d
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
    terms = tail $ scanl (\t k -> t * x / fromIntegral k) e [1..]
    qs = q1 : zipWith (+) qs terms

poisson :: (Show d, Floating d, Erf d, InvErf d, Ord d) => d -> Prob d Int
poisson rate = do
  x <- uniform
  let cmf = gammaQs rate
  let (Just n) = findIndex (> x) cmf
  return $ fromIntegral n

logSumExp :: (Floating a, Ord a) => [a] -> a
logSumExp [] = error "logSumExp: empty list"
logSumExp xs =
    let m = maximum xs
    in m + log (Prelude.sum (map (\x -> exp (x - m)) xs))

mean1 :: Floating d => [d] -> d
mean1 xs = Prelude.sum xs / fromIntegral (length xs)

std1 :: (Floating d, Ord d) => [d] -> d
std1 xs =
    let m = mean1 xs
        v = Prelude.sum (map (\x -> (x - m) * (x - m)) xs) / fromIntegral (length xs)
        s = sqrt v
    in if s < 1e-6 then 1e-6 else s

polyEval :: Floating d => [d] -> d -> d
polyEval coeffs x = go coeffs 0 1 0
  where
    go [] _ _ acc = acc
    go (c:cs) j p acc = go cs (j+1) (p*x) (acc + c*p)

polyModel :: (Floating d, Ord d, Erf d, InvErf d, Show d)
         => d -> d -> d -> d -> Int -> [Double] -> [Double] -> Meas d [d]
polyModel poRate coefStd alpha noiseStd minDegree xTrain yTrain =
    do
        let xs0 = map realToFrac xTrain
        let ys0 = map realToFrac yTrain
        let xMean = mean1 xs0
        let xStd = std1 xs0
        k <- sample (poisson poRate)
        let deg = k + minDegree
        bs <- forM [0..deg] (\j -> do
                let jf = fromIntegral (j+1)
                let sd = coefStd * exp ((-alpha) * log jf)
                sample (normal 0 sd)
            )
        let xs = map (\x -> (x - xMean) / xStd) xs0
        let ll = Prelude.sum $ zipWith (\x y -> normalLogPdf (polyEval bs x) noiseStd y) xs ys0
        scoreLog ll
        return bs

runPolyModel (eps, steps, count, burnin, xTrain, yTrain, xTest, yTest, trueCoeffs, modelId, poRate, coefStd, alpha, noiseStd, minDegree, rep) =
    do
        let g = mkStdGen rep
        --let (alg, alg2, alg_kernel, filename) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin Nothing, "samples_produced/poly/poly_po_rate" ++ show (primal poRate) ++ "_coefstd" ++ show coefStd ++ "_alpha" ++ show alpha ++ "_noisestd" ++ show noiseStd ++ "_mindeg" ++ show minDegree ++ "_id" ++ show modelId ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        --let (alg, alg2, alg_kernel, filename) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin Nothing, "samples_produced/poly/poly_po_rate" ++ show (primal poRate) ++ "_coefstd" ++ show coefStd ++ "_alpha" ++ show alpha ++ "_noisestd" ++ show noiseStd ++ "_mindeg" ++ show minDegree ++ "_id" ++ show modelId ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        --let (alg, alg2, alg_kernel, filename) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin Nothing, "samples_produced/poly/poly_po_rate" ++ show (primal poRate) ++ "_coefstd" ++ show coefStd ++ "_alpha" ++ show alpha ++ "_noisestd" ++ show noiseStd ++ "_mindeg" ++ show minDegree ++ "_id" ++ show modelId ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        let (alg, alg2, alg_kernel, filename) = ("lazyLMH","lazyLMH", mh g (lmhKernel eps) burnin Nothing, "samples_produced/poly/poly_po_rate" ++ show (primal poRate) ++ "_coefstd" ++ show coefStd ++ "_alpha" ++ show alpha ++ "_noisestd" ++ show noiseStd ++ "_mindeg" ++ show minDegree ++ "_id" ++ show modelId ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_p" ++ show eps ++ "_burnin" ++ show burnin ++ ".json")
        print $ "start rep " ++ show rep
        print filename
        start <- getCPUTime
        fs <- alg_kernel (polyModel poRate (toNagata coefStd) (toNagata alpha) (toNagata noiseStd) minDegree xTrain yTrain)
        let samples = take count $ drop burnin $ map fst fs
        let results = map (map primal) samples
        results `deepseq` return ()
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
                            "coef_std" .= coefStd,
                            "alpha" .= alpha,
                            "noise_std" .= noiseStd,
                            "min_degree" .= minDegree
                            ],
                        "data_info" .= object
                            [ "x_train" .= xTrain,
                            "y_train" .= yTrain,
                            "x_test" .= xTest,
                            "y_test" .= yTest,
                            "true_coeffs" .= trueCoeffs
                            ],
                        "model_id" .= modelId
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runPolyModelAll seed =
    do
        let modelId = 0
        bs <- B.readFile "samples_produced/poly/poly_alpha0.0_coef_std0.5_cores3_min_degree1_noise_std0.25_po_rate4.0_true_degree5_id_0.json"
        let parsed = decode bs :: Maybe [[Double]]
        case parsed of
            Just [xTrain, yTrain, xTest, yTest, trueCoeffs] -> do
                let poRate = 4
                let coefStd = 0.5
                let alpha = 0
                let noiseStd = 0.25
                let minDegree = 1
                let configs = [(eps, 6, 1000000, 0, xTrain, yTrain, xTest, yTest, trueCoeffs, modelId, po, coefStd, alpha, noiseStd, minDegree, seed) | eps <- [0.5], po <- [4]]
                let x = map runPolyModel configs
                sequence_ x
                --let configs = (0.008, 6, 2000, 0, xTrain, yTrain, xTest, yTest, trueCoeffs, modelId, poRate, coefStd, alpha, noiseStd, minDegree, seed)
                --runPolyModel configs
            Just _ -> putStrLn "Unexpected JSON structure "
            Nothing -> putStrLn "Failed to parse JSON"

main :: IO ()
main = do
    args <- getArgs
    let seed = case args of
            ["--seed", s] ->
                case readMaybe s of
                    Just n  -> n
                    Nothing -> error "Seed must be an integer"
            _ -> error "Usage: --seed <int>"

    putStrLn $ "Seed is: " ++ show seed
    runPolyModelAll seed
