{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Geometric where
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


runGeom (eps, steps, count, burnin, p, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel, filename) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin,"samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        --let (alg, alg2, alg_kernel, filename) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        --let (alg, alg2, alg_kernel, filename) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json")
        --let (alg, alg2, alg_kernel, filename) = ("lazyLMH", "lazyLMH", mh g (lmhKernel eps) burnin, "samples_produced/geom/geomLazy_p" ++ show p ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_burnin" ++ show burnin ++ ".json")
        
        print $ "start rep " ++ show rep
        print filename
        start <- getCPUTime
        fs <- alg_kernel (geometricLazy (toNagata p))
        let results = take count $ drop burnin $ map fst fs
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
                            [ "p" .= p
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runGeomAll =
    do
        let configs = [(e, l, 1300, 0, 0.2, rep)| rep <- [0..9], l <- [5, 10, 15], e <- [0.05, 0.1]]
        let x = map runGeom configs
        sequence_ x



main :: IO ()
main = runGeomAll