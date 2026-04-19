{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Walk where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
import LazyPPLHMC hiding (uniform, logSumExp, findIndex)

import System.Random hiding (uniform)

import Debug.Trace
import Data.List (findIndex, find)
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




walkModel :: (Floating d, Ord d, Show d, Erf d) => d -> Meas d d
walkModel distLim = do
        startSample <- sample uniform
        let start = (3 * startSample)
        finalDistance <- walkStep start 0
        scoreLog (normalLogPdf 1.1 0.1 finalDistance)
        return start
    where
        walkStep position distance
            | position <= 0 || distance >= distLim = return distance
            | otherwise = do
                stepSample <- sample uniform
                let step = 2 * stepSample - 1
                walkStep (position + step) (distance + abs step)

walkModelLazy :: (Floating d, Ord d, Show d, Erf d) => d -> Meas d d
walkModelLazy distLim = do
        startSample <- sample uniform
        stepSamples <- sample iiduniform
        let steps = map (\x -> 2*x-1) stepSamples
        let start = (3 * startSample)
        let positions = scanl (+) start steps
        let distances = scanl (+) 0 (map abs steps)
        case find (\(p, d) -> p <= 0 || d >= distLim) (zip positions distances) of
            Just (finalPos, finalDistance) -> scoreLog (normalLogPdf 1.1 0.1 finalDistance)
            Nothing -> score 0
        return start


runWalk (algName, eps, steps, count, burnin, distLim, rep) =
    do
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel, filename) =
                case algName of
                    "hmc" -> ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel (LFConfig eps steps 0)) burnin Nothing, "samples_produced/walk/walkLazy_dist_lim" ++ show distLim ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "hmcosc" -> ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel (LFConfig eps steps 0)) burnin Nothing, "samples_produced/walk/walkLazy_dist_lim" ++ show distLim ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "nuts" -> ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin Nothing, "samples_produced/walk/walkLazy_dist_lim" ++ show distLim ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "lmh" -> ("lazyLMH", "lazyLMH", mh g (lmhKernel eps) burnin Nothing, "samples_produced/walk/walkLazy_dist_lim" ++ show distLim ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    _ -> error "Unknown algorithm"

        print $ "start rep " ++ show rep
        print filename
        start <- getCPUTime
        fs <- alg_kernel (walkModelLazy (toNagata distLim))
        let results = map primal $ take count $ drop burnin $ map fst fs 
        results `deepseq` return ()
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
                            [ "dist_lim" .= distLim
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)
        print "done"



main :: IO ()
main = do
    args <- getArgs

    let getArgMaybe name =
            case dropWhile (/= name) args of
                (_:val:_) -> Just val
                _         -> Nothing

    let getArgDef name def =
            case getArgMaybe name of
                Just v  -> v
                Nothing -> def

    let getArgNum name def =
            case getArgMaybe name of
                Just v  -> maybe def id (readMaybe v)
                Nothing -> def

    let eps    = getArgNum "--eps" 0.1
    let steps  = getArgNum "--steps" 10
    let count  = getArgNum "--count" 100
    let burnin = getArgNum "--burnin" 100
    let dist   = getArgNum "--dist" 10
    let alg    = getArgDef "--alg" "hmc"

    case getArgMaybe "--seed" of
        Just s ->
            case readMaybe s of
                Just seed -> do
                    putStrLn $ "Seed is: " ++ show seed
                    runWalk (alg, eps, steps, count, burnin, dist, seed)
                Nothing -> error "Seed must be an integer"

        Nothing -> do
            putStrLn "No seed provided, running seeds 0 to 10"
            mapM_ (\seed -> do
                putStrLn $ "Seed is: " ++ show seed
                runWalk (alg, eps, steps, count, burnin, dist, seed)
                ) [0..10]