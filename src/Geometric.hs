{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Geometric where
import AD
import Data.Colour
import Data.Colour.Names
import Control.Monad
import Data.Number.Erf
import Data.List (sort,nub)
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



geometric :: (Floating d, Ord d, Show d, Erf d) => d -> Meas d Int
geometric p = do
    x <- sample uniform
    if x < p
        then return 1
        else do
            n <- geometric p
            return (1 + n)

geometricLazy :: (Floating d, Ord d, Show d, Erf d) => d -> Meas d Int
geometricLazy p = do
    x <- sample iiduniform
    let (Just n) = findIndex (<p) x
    return (1+n)


runGeom (algName, eps, steps, count, burnin, p, rep) =
    do
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel, filename) =
                case algName of
                    "hmc" -> ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin Nothing,"samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ "lazyHMCmod" ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "hmcosc" -> ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin Nothing, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ "lazyHMCOsc" ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "nuts" -> ("lazyNUTSnew", "lazyNUTSnew", mh g (nutsKernel (LFConfig eps steps 0)) burnin Nothing, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ "lazyNUTSnew" ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
                    "lmh" -> ("lazyLMH", "lazyLMH", mh g (lmhKernel eps) burnin Nothing, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ "lazyLMH" ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_burnin" ++ show burnin ++ ".json")
                    _ -> error "Unknown algorithm"
        
        print $ "start rep " ++ show rep
        print filename
        start <- getCPUTime
        fs <- alg_kernel (geometricLazy (toNagata p))
        let results = take count $ drop burnin $ map fst fs
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
                            [ "p" .= p
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runGeomAll seed =
    do
        let configs = [(e, l, 1300, 0, 0.2, seed)| l <- [5], e <- [0.1]]
        let x = map runGeom configs
        sequence_ x



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

    let alg    = getArgDef "--alg" "nuts"
    let seed   = maybe (error "Seed must be an integer") id (readMaybe (getArgDef "--seed" "0"))
    let eps    = getArgNum "--eps" 0.1
    let steps  = getArgNum "--steps" 5
    let count  = getArgNum "--count" 1300
    let burnin = getArgNum "--burnin" 0
    let p      = getArgNum "--p" 0.2

    putStrLn $ "Seed is: " ++ show seed
    runGeom (alg, eps, steps, count, burnin, p, seed)