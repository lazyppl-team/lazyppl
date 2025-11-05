{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleMix where
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

iidnormal :: Floating d => d -> d -> Prob d [d]
iidnormal m s= do 
  x <- normal m s
  xs <- iidnormal m s
  return $ x : xs



simpleMix2 :: (Floating d, Ord d, Show d, Erf d) => d -> d -> d -> d -> Meas d [d]
simpleMix2 w1 w2 m1 m2 = do
        x <- sample $ normal 0 1
        if x > 0
            then do 
                z <- sample $ normal m1 1
                score w1
                return $ trace (show [x, z]) [x, z]
            else do
                z <- sample $ normal m2 1
                score w2
                return $ trace (show [x, z]) [x, z]

simpleMix3 :: (Floating d, Ord d, Show d, Erf d) => Int -> d -> d -> d -> d -> Meas d [d]
simpleMix3 n w1 w2 m1 m2 = do
        x <- sample $ normal 0 1
        if x > 0
            then do 
                z <- sample $ normal 0 1
                scoreLog $ (fromIntegral n) * (normalLogPdf z 1 m1)
                return $ trace (show [x, z]) [x, z]
            else do
                z <- sample $ normal 0 1
                scoreLog $ (fromIntegral n) * (normalLogPdf z 1 m2)
                return $ trace (show [x, z]) [x, z]

simpleMix4 :: (Floating d, Ord d, Show d, Erf d) => Int -> d -> d -> d -> d -> Meas d [d]
simpleMix4 n w1 w2 m1 m2 = do
        x <- sample $ normal 0 1
        zs <- sample $ iidnormal 0 1
        if x > 0
            then do 
                let t = head zs
                scoreLog $ (fromIntegral n) * (normalLogPdf t 1 m1)
                score w1
                --return $ trace (show [x, t]) [x, t]
                return [x, t]
            else do
                let t = zs!!1
                scoreLog $ (fromIntegral n) * (normalLogPdf t 1 m2)
                score w2
                --return $ trace (show [x, t]) [x, t]
                return [x, t]


runSimpleMix (eps, steps, count, burnin, w1, w2, m1, m2, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        print $ "start rep " ++ show rep
        let filename = "samples_produced/simple_mix/simpleMix2_m1" ++ show m1 ++ "_m2" ++ show m2 ++ "_w1" ++ show w1 ++ "_w2" ++ show w2 ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (simpleMix2 (toNagata w1) (toNagata w2) (toNagata m1) (toNagata m2))
        let results = map (map primal) $ take count $ drop burnin $ map fst fs 
        print $ map exp $ take 100 $ map snd fs
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
                            [ "w1" .= w1,
                              "w2" .= w2,
                              "m1" .= m1,
                              "m2" .= m2
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runSimpleMix3 (eps, steps, count, burnin, n, w1, w2, m1, m2, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        --let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burinin)
        let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        print $ "start rep " ++ show rep
        let filename = "samples_produced/simple_mix/simpleMix3_m1" ++ show m1 ++ "_m2" ++ show m2 ++ "_w1" ++ show w1 ++ "_w2" ++ show w2 ++ "_n" ++ show n ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (simpleMix3 n (toNagata w1) (toNagata w2) (toNagata m1) (toNagata m2))
        let results = map (map primal) $ take count $ drop burnin $ map fst fs 
        print $ map exp $ take 10 $ map snd fs
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
                            [ "n" .= n,
                              "w1" .= w1,
                              "w2" .= w2,
                              "m1" .= m1,
                              "m2" .= m2
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runSimpleMix4 (eps, steps, count, burnin, n, w1, w2, m1, m2, rep) =
    do
        --g <- getStdGen
        let g = mkStdGen rep
        --let (alg, alg2, alg_kernel) = ("lazyHMCmod", "lazyHMCmod", mh g (hmcKernel(LFConfig eps steps 0)) burnin)
        --let (alg, alg2, alg_kernel) = ("lazyHMCOsc", "lazyHMCOsc", mh g (hmcOscKernel(LFConfig eps steps 0)) burnin)
        let (alg, alg2, alg_kernel) = ("lazyNUTS", "lazyNUTS", mh g (nutsKernel (LFConfig eps steps 0)) burnin)
        print $ "start rep " ++ show rep
        let filename = "samples_produced/simple_mix/simpleMix4_m1" ++ show m1 ++ "_m2" ++ show m2 ++ "_w1" ++ show w1 ++ "_w2" ++ show w2 ++ "_n" ++ show n ++ "-" ++ show rep ++ "_" ++ alg ++ "__count" ++ show count ++ "_eps" ++ show eps ++ "_leapfrogsteps" ++ show steps ++ "_burnin" ++ show burnin ++ ".json"
        print filename
        start <- getCPUTime
        fs <- alg_kernel (simpleMix4 n (toNagata w1) (toNagata w2) (toNagata m1) (toNagata m2))
        let results = map (map primal) $ take count $ drop burnin $ map fst fs 
        print $ map exp $ take 10 $ map snd fs
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
                            [ "n" .= n,
                              "w1" .= w1,
                              "w2" .= w2,
                              "m1" .= m1,
                              "m2" .= m2
                            ],
                        "data_info" .= object
                            [ 
                            ]
                        ]
                    ]
                ]
        B.writeFile filename (encode jsonVal)

runSimpleMixAll =
    do
        let configs = [(e, 32, 1300, 0, 1, 2, 1, 10, rep)| rep <- [0..9], e <- [0.1, 0.05]]
        let configs4 = [(e, 6, 1300, 0, 100, 1, 2, 1, 2, rep)| rep <- [0..9], e <- [0.1, 0.05]]
        let x = map runSimpleMix4 configs4
        sequence_ x



main :: IO ()
main = runSimpleMixAll