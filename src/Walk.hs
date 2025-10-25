{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Walk where
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


runWalk (eps, steps, count, burnin, distLim, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        --let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel (LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel (LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        let (alg, alg2, alg_kernel) = ("lazyLMH", "lazyLMH", mh g (lmhKernel 0.5) burnin)

        print $ "start rep " ++ show rep
        let filename = "samples_produced/walk/walk3_dist_lim" ++ show distLim ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (walkModel (toNagata distLim))
        let results = map primal $ take count $ drop burnin $ map fst fs 
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

runWalkAll =
    do
        let configs = [(e, l, 1300, 100, 10, rep)| rep <- [0..9], e <- [0.1], l <- [6]]
        let x = map runWalk configs
        sequence_ x



main :: IO ()
main = runWalkAll