{-# LANGUAGE BangPatterns #-}
module Distr.Mondrian where

import LazyPPL
import Distr
import Distr.Counter
import Data.IORef
import System.IO.Unsafe
import Debug.Trace


-- From Roy-Teh "The Mondrian Process", NIPS 2009

-- This corresponds to a point process on [low, high] with uniform intensity 
oneDimMondrian :: Double -> (Double, Double) -> Prob [Double]
oneDimMondrian budget (low, high) = do
    cutCost <- exponential (high - low)
    if budget < cutCost then return [] else do
        let remaining = budget - cutCost
        cut <- uniformbounded low high
        leftcuts <- oneDimMondrian remaining (low, cut)
        rightcuts <- oneDimMondrian remaining (cut, high)
        return $ leftcuts ++ [cut] ++ rightcuts




-- Some data types. Intuitively, Matrix is a type of 2D exchangeable arrays, and 'Mondrian Double' would be the type of block-structured graphons. 

{--

data Mondrian a = Block a (Double, Double) (Double, Double) -- args: atomName and two intervals making up the block
                | Partition Int Double (Mondrian a) (Mondrian a) -- args: dimension cutPosition subtree1 subtree2  

newtype Row = Row Int -- in [0, 1]
newtype Col = Col Int

data Matrix = Matrix Counter Counter [[Bool]]


{--
Generates a random block partition of the rectangle [aa, aA] x [bb, bB] 
where each block has an associated random draw from the base distribution. 
--}
newMondrian :: Prob a -> Double -> (Double, Double) -> (Double, Double) -> Prob (Mondrian a)
newMondrian base budget (aa, aA) (bb, bB) = do
    cutCost <- exponential (aA - aa + bB - bb)
    if budget < cutCost then do {x <- base; return $ Block x (aa, aA) (bb, bB)} else do
        let remaining = budget - cutCost
        dim <- bernoulli $ (aA - aa) / (aA - aa + bB - bb)
            -- if dim is true then cut is perpendicular to (aa, aA)
        if dim then
            do  cut <- uniformbounded aa aA
                leftTree <- newMondrian base remaining (aa, cut) (bb, bB)
                rightTree <- newMondrian base remaining (cut, aA) (bb, bB)
                return $ Partition 0 cut leftTree rightTree
        else
            do  cut <- uniformbounded bb bB
                leftTree <- newMondrian base remaining (aa, aA) (bb, cut)
                rightTree <- newMondrian base remaining (aa, aA) (cut, bB)
                return $ Partition 1 cut leftTree rightTree


{--
Sample a block structure over [0, 1]^2 from the Mondrian process, 
and use it to generate an infinite exchangeable array.
--}
newMatrix :: Prob Double -> Double -> Prob Matrix
newMatrix base budget = do
    mondrian <- newMondrian base budget (0, 1) (0, 1)
    rs <- iid uniform
    cs <- iid uniform
    matrix <- mapM (\r -> mapM (sampleMondrian mondrian r) cs) rs
    rowCounter <- newCounter
    colCounter <- newCounter
    return $ Matrix rowCounter colCounter matrix
        where
            iid :: Prob a -> Prob [a]
            iid p = do {r <- p ; rs <- iid p ; return $ r : rs}


{-- 
Given a Mondrian block structure and two data points, sample an 'edge'. 
--}
sampleMondrian :: Mondrian Double -> Double -> Double -> Prob Bool
sampleMondrian mondrian r c =
    do  case mondrian of
            Block p _ _ -> bernoulli p
            Partition 0 cut left right -> if r < cut then sampleMondrian left r c else sampleMondrian right r c
            Partition 1 cut left right -> if c < cut then sampleMondrian left r c else sampleMondrian right r c


lookup :: Matrix -> Row -> Col -> Bool
lookup (Matrix _ _ matrix) (Row r) (Col c) = matrix!!r!!c

newRow :: Matrix -> Prob Row
newRow (Matrix c _ _) = readAndIncrement c >>= (return . Row)

newCol :: Matrix -> Prob Col
newCol (Matrix _ c _) = readAndIncrement c >>= (return . Col)

--}

data Mondrian a = Block a [(Double, Double)] -- args: atomName and intervals making up the block
                | Partition Int Double (Mondrian a) (Mondrian a) -- args: dimension cutPosition subtree1 subtree2  

newtype Row = Row Int -- in [0, 1]
newtype Col = Col Int

data Matrix = Matrix Counter Counter [[Bool]]


{--
Generates a random block partition of the rectangle [aa, aA] x [bb, bB] 
where each block has an associated random draw from the base distribution. 
--}
newMondrian :: Prob a -> Double -> [(Double, Double)] -> Prob (Mondrian a)
newMondrian base budget as = do
    let lengths = map (\(a, b) -> b - a) as
    let sumLengths = sum lengths
    cutCost <- exponential sumLengths
    if budget < cutCost 
        then do {x <- base; return $ Block x as} 
        else do let remaining = budget - cutCost
                dim <- categorical lengths -- if dim is true then cut is perpendicular to (aa, aA)
                let (aa, aA) = as!!dim
                cut <- uniformbounded aa aA
                leftMondrian <- newMondrian base remaining $ zipWith (\ ab i -> if i == dim then (aa, cut) else ab) as [0..]
                rightMondrian <- newMondrian base remaining $ zipWith (\ ab i -> if i == dim then (cut, aA) else ab) as [0..]
                return $ Partition dim cut leftMondrian rightMondrian


{--
Sample a block structure over [0, 1]^2 from the Mondrian process, 
and use it to generate an infinite exchangeable array.
--}
newMatrix :: Prob Double -> Double -> Prob Matrix
newMatrix base budget = do
    mondrian <- newMondrian base budget [(0, 1), (0, 1)]
    rs <- iid uniform
    cs <- iid uniform
    matrix <- mapM (\r -> mapM (sampleMondrian mondrian r) cs) rs
    rowCounter <- newCounter
    colCounter <- newCounter
    return $ Matrix rowCounter colCounter matrix
        where
            iid :: Prob a -> Prob [a]
            iid p = do {r <- p ; rs <- iid p ; return $ r : rs}


{-- 
Given a Mondrian block structure and two data points, sample an 'edge'. 
--}
sampleMondrian :: Mondrian Double -> Double -> Double -> Prob Bool
sampleMondrian mondrian r c =
    do  case mondrian of
            Block p _  -> bernoulli p
            Partition 0 cut left right -> if r < cut then sampleMondrian left r c else sampleMondrian right r c
            Partition 1 cut left right -> if c < cut then sampleMondrian left r c else sampleMondrian right r c


lookup :: Matrix -> Row -> Col -> Bool
lookup (Matrix _ _ matrix) (Row r) (Col c) = matrix!!r!!c

newRow :: Matrix -> Prob Row
newRow (Matrix c _ _) = readAndIncrement c >>= (return . Row)

newCol :: Matrix -> Prob Col
newCol (Matrix _ c _) = readAndIncrement c >>= (return . Col)





