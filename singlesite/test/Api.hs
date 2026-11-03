{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
-- A constant test tree compiled as a static closure would end in IND_STATIC
-- indirections, which the single-site traversal does not read.
module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Trans.Writer (runWriterT)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map as M
import Data.Monoid (getProduct)
import Data.Word (Word64)
import LazyPPL (Meas(..), randomTree, runProb, sample, score, uniform)
import LazyPPL.Distributions (bernoulli)
import LazyPPL.SingleSite (mh1)
import qualified LazyPPL.SingleSite.Internal as S
import ReverseSupport (reverseSupport)
import System.Random (RandomGen(..), SplitGen(..), mkStdGen, setStdGen)
import System.Timeout (timeout)

main :: IO ()
main = do
  supported <- reverseSupport
  unless supported (fail "single-site rejects proposals without reverse support")
  putStrLn "PASS single-site rejects proposals without reverse support"
  constant <- constantMeasureRepeats
  unless constant (fail "mh1 does not repeat the sample of a site-free measure")
  putStrLn "PASS mh1 repeats the sample of a site-free measure"
  rejected <- zeroWeightMoveRejected
  unless rejected (fail "mh1 accepted a move from positive weight to weight 0")
  putStrLn "PASS mh1 rejects a move from positive weight to weight 0"
  accepted <- ratioOneMoveAccepted
  unless accepted (fail "mh1 rejected a move whose acceptance ratio is 1")
  putStrLn "PASS mh1 accepts a move whose acceptance ratio is 1"
  setStdGen (mkStdGen 23)
  coin <- map fst . take 60000 . drop 1000 <$> mh1 coinModel
  posterior <- posteriorFraction "mh1 coin posterior" coin
  unless posterior (fail "mh1 does not sample the coin posterior")

-- A generator that returns one word forever.
newtype ConstWord = ConstWord Word64

instance RandomGen ConstWord where
  genWord64 g@(ConstWord w) = (w, g)

instance SplitGen ConstWord where
  splitGen g = (g, g)

-- A move from positive weight to weight 0 has acceptance ratio 0, so it is
-- never accepted. The word 2^64-1 makes the proposed label and the acceptance
-- uniform as small as the generator allows; the word 2^63 puts every label of
-- the initial tree near 1/2, where the weight is 1.
zeroWeightMoveRejected :: IO Bool
zeroWeightMoveRejected = do
  -- Read through an IORef, so the compiler cannot build the tree as a static closure.
  word <- newIORef (2 ^ (63 :: Int)) >>= readIORef
  let model = do x <- sample uniform; score (if x > 0.1 then 1 else 0); pure x
      Meas writer = model
      treeSeed = ConstWord word
      tree = randomTree treeSeed
      (x0, w0) = runProb (runWriterT writer) tree
  evaluate (rnf x0)
  evaluate (rnf w0)
  sites <- S.trunc tree
  let (_, _, _, _, (x1, w1)) = S.mhStep model (treeSeed, ConstWord maxBound, M.empty, sites, (x0, w0))
  answered <- timeout 2000000 (evaluate (rnf x1))
  case answered of
    Nothing -> putStrLn "zero-weight move: the step gave no answer within 2 s" >> pure False
    Just () -> do
      putStrLn $ "zero-weight move: from x=" ++ show x0 ++ " (weight " ++ show (getProduct w0)
        ++ ") the step returned x=" ++ show x1 ++ " (weight " ++ show (getProduct w1) ++ ")"
      pure (x1 == x0 && getProduct w1 == getProduct w0)

-- A move between two samples of weight 1, each with one site, has acceptance
-- ratio 1, so it is always accepted. The word 0 makes the acceptance uniform as
-- large as the generator allows; the word 2^63 puts every label of the initial
-- tree near 1/2, and the proposed label is the largest label.
ratioOneMoveAccepted :: IO Bool
ratioOneMoveAccepted = do
  -- Read through an IORef, so the compiler cannot build the tree as a static closure.
  word <- newIORef (2 ^ (63 :: Int)) >>= readIORef
  let model = do x <- sample uniform; score 1; pure x
      Meas writer = model
      treeSeed = ConstWord word
      tree = randomTree treeSeed
      (x0, w0) = runProb (runWriterT writer) tree
  evaluate (rnf x0)
  evaluate (rnf w0)
  sites <- S.trunc tree
  let (_, _, _, _, (x1, _)) = S.mhStep model (treeSeed, ConstWord 0, M.empty, sites, (x0, w0))
  answered <- timeout 2000000 (evaluate (rnf x1))
  case answered of
    Nothing -> putStrLn "ratio-one move: the step gave no answer within 2 s" >> pure False
    Just () -> do
      putStrLn $ "ratio-one move: from x=" ++ show x0 ++ " the step returned x=" ++ show x1
      pure (x1 /= x0)

-- A measure that reads no random site has a one-point sample space, and the
-- documented chain repeats its initial sample and weight, rather than failing
-- on an empty site list. `return` never splits the tree, while `sample (pure ())`
-- splits it once through the writer's fmap.
constantMeasureRepeats :: IO Bool
constantMeasureRepeats = do
  pureMeasure <- take 5 <$> mh1 (return ())
  lifted <- take 5 <$> mh1 (sample (pure ()))
  pure $ and [ map fst samples == replicate 5 ()
             && all (\(_, weight) -> getProduct weight == 1) samples
             | samples <- [pureMeasure, lifted] ]

-- A fair coin b. On true, a uniform u is read and scored 6u, which integrates
-- to 3; on false, the score is 1. So P(b) = 3/4 exactly, and the site u exists
-- only on the true branch.
coinModel :: Meas Bool
coinModel = do
  b <- sample (bernoulli 0.5)
  if b then sample uniform >>= \u -> score (6 * u) else score 1
  pure b

-- The fraction of true draws is within five standard errors of 3/4. The
-- standard error comes from the means of 40 consecutive batches, and it must be
-- below 0.01 for the check to say anything.
posteriorFraction :: String -> [Bool] -> IO Bool
posteriorFraction name draws = do
  let n = length draws
      batches = 40
      size = n `div` batches
      batchMean k = fromIntegral (length (filter id (take size (drop (k * size) draws)))) / fromIntegral size :: Double
      means = map batchMean [0 .. batches - 1]
      p = sum means / fromIntegral batches
      se = sqrt (sum [(m - p) ^ (2 :: Int) | m <- means] / fromIntegral (batches - 1) / fromIntegral batches)
      ok = se < 0.01 && abs (p - 0.75) < 5 * se
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ name ++ ": P(true) = " ++ show p ++ ", standard error " ++ show se ++ ", exact 3/4"
  pure ok
