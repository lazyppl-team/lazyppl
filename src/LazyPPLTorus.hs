{-# LANGUAGE GeneralizedNewtypeDeriving, 
    ScopedTypeVariables,
    RankNTypes, BangPatterns, DeriveFunctor #-}

-- This version of LazyPPL uses a unit interval at each side but it is considered as a circle.

module LazyPPLTorus where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random hiding (uniform)
import qualified System.Random as R
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Numeric.Log

import Control.DeepSeq
import Control.Exception (evaluate)

import System.IO.Unsafe

import GHC.Exts.Heap
import System.Mem
import Unsafe.Coerce
import Data.Maybe

import qualified Data.Map as M
import qualified Data.List as L (lookup)

import Debug.Trace

import AD
import Data.Number.Erf


-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by d (Doubles, or dual numbers)
-- | Our source of randomness will be a Tree, populated by gaussian choices for each label.
-- | Often people would just use a list or stream instead of a tree.
-- | But a tree allows us to be lazy about how far we are going all the time.
data Tree d = Tree d [Tree d]
  deriving (Functor, Show)

-- | A probability distribution over a is 
-- | a function 'Tree d -> a'
-- | The idea is that it uses up bits of the tree as it runs
-- | The parameter d specifies the type of real numbers used for
-- | probabilities. Typically d will be Double or dual numbers.
newtype Prob d a = Prob (Tree d -> a)

-- | Two key things to do with trees:
-- | Split tree splits a tree in two (bijectively)
-- | Get the label at the head of the tree and discard the rest
splitTree :: Tree d -> (Tree d , Tree d)
splitTree (Tree r (t : ts)) = (t , Tree r ts)

-- | In contrast to the original LazyPPL, here the standard uniform distribution is a derived construct.
-- | We use this measure-preserving transformation so that random walks around the circle bounce rather than jump.
uniform :: (Fractional d) => Prob d d
uniform = Prob $ \(Tree r _) -> 2 * abs (r - (1/2))
-- Other times we might use the circle, e.g. cosine gives the arcsin distribution, or Box-Muller transformations  


-- | Probabilities form a monad.
-- | Sequencing is done by splitting the tree
-- | and using different bits for different computations.
instance Monad (Prob d) where
  return a = Prob $ const a
  (Prob m) >>= f = Prob $ \g ->
    let (g1, g2) = splitTree g
        (Prob m') = f (m g1)
    in m' g2
instance Functor (Prob d) where fmap = liftM
instance Applicative (Prob d) where {pure = return ; (<*>) = ap}

{- | An unnormalized measure is represented by a probability distribution over pairs of a weight and a result 
     In contrast to original LazyPPL, we use a log weight here, rather than using log numbers.
-}
newtype Meas d a = Meas {unMeas :: (WriterT (Sum d) (Prob d) a) }
  deriving(Functor, Applicative, Monad)

{- | The two key methods for Meas are sample (from a probability) and score (aka factor, weight) -}
score :: Floating d => d -> Meas d ()
score r = Meas $ tell $ Sum $ log r

-- scoreLog :: Log d -> Meas d ()
scoreLog :: d -> Meas d ()
scoreLog r = Meas $ tell $ Sum r

sample :: Num d => Prob d a -> Meas d a
sample p = Meas $ lift p


{- | Preliminaries for the simulation methods. Generate a tree with normal random labels
    This uses 'split' to split a random seed -}
randomTree :: RandomGen g => g -> Tree Double
randomTree g = let (a,g') = random g in
               Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree Double]
randomTrees g = let (g1,g2) = split g in randomTree g1 : randomTrees g2

{- | 'runProb' runs a probability deterministically, given a source of randomness -}
runProb :: Prob d a -> Tree d -> a
runProb (Prob a) = a

runMeas :: Meas d a -> Tree d -> (a,d)
runMeas (Meas m) t = let (x,Sum w) = runProb (runWriterT m) t in (x,w)

-- A minimal example with one dimension. 

minExample :: (Floating d) => Meas d d
minExample = do
   x <- sample uniform
   score x
   return x

-- TOM: The dualizeTree function takes an infinite tree of
--      Doubles to an infinite tree of dual numbers.
--
--      The idea is that every element in the tree is a variable/parameter.
--      Hence, if d is the (primal) value of the n-th element,
--      then it's tangent is the one-hot vector whose n-th component is 1.
--      Because we are using a sparse map representation of the vector,
--      we represent this with the singleton map {n |-> 1}.

{-
-- Simpler definition, but very slow.

dualizeTree :: Tree Double -> Tree (Nagata Integer Double) 
dualizeTree = goT [0..]
  where 
    goT (n:ns) (Tree d ts) = Tree (N d (M.singleton n 1)) (goF ns ts) 

    goF ns (t:ts) = goT ns1 t : goF ns2 ts
      where (ns1,ns2) = splitList ns

splitList ~(x:xs) = (x : snd r, fst r) 
    where r = splitList xs
-}

dualizeTree :: Tree Double -> Tree (Nagata Integer Double) 
dualizeTree = goT 0 1
  where 
    goT !n !i (Tree d ts) = Tree (N d (M.singleton n 1)) (goF (n+i) i ts) 

    goF !n !i (t:ts) = goT n (2*i) t : goF (n+i) (2*i) ts

test :: Show a => Meas (Nagata Integer Double) a -> IO ()
test p = 
  do newStdGen
     g <- getStdGen
     let rs = dualizeTree (randomTree g)
     let r = (runProb (runWriterT (unMeas p)) rs)
     print r
     
-- | Gradient ascent implementation that takes a number of iterations, a learning factor and
--   a measurement program. It maximizes the measurement program's score.
gradientAscent :: Double -> Meas (Nagata Integer Double) a -> IO [(a,Nagata Integer Double)]
gradientAscent = gradientOptimize (+) 

--gradientDescent :: Int -> Double -> Meas (Nagata Integer Double) a -> IO a
--gradientDescent = gradientOptimize (-) 

gradientOptimize :: (Double -> Double -> Double) -> Double -> Meas (Nagata Integer Double) a -> IO [(a,Nagata Integer Double)]
gradientOptimize op alpha p =
  do newStdGen
     g <- getStdGen
     let rs = dualizeTree (randomTree g)
     return $ go rs
  where
    go rs =
       let (result,score) = (runProb (runWriterT (unMeas p)) rs) in
       (result,getSum score) : (go $ fmap (learn $ tangent $ getSum score) rs)

    learn dr (N x dx) = N (op x (alpha * M.findWithDefault 0 key dr)) dx
      where key = head (M.keys dx) 

prettyAscent :: Int -> Double ->  Meas (Nagata Integer Double) a -> IO a
prettyAscent n alpha p = do
   rss <- gradientAscent alpha p
   forM [0..(n-1)] (\i -> do {let hdr = "* Iteration " ++ show (i + 1) ++ ":" in putStr hdr ; let (result,score) = (rss!!i) in putStrLn ("Log likelihood = " ++ show score) }) 
   return $ fst $  rss !! n


mh :: (forall g. RandomGen g => g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double,Tree Double))
      ->  Meas (Nagata Integer Double) a -> IO [a]
mh k m = do
    -- The parameter k takes a monadic measure and a tree and proposes a new tree,
    -- also returning the acceptance ratio for the new tree.
    --
    -- Top level: produce a stream of samples.
    -- Split the random number generator in two
    -- One part is used as the first seed for the simulation,
    -- and one part is used for the randomness in the MH algorithm.
    newStdGen
    g <- getStdGen
    let (g1,g2) = split g
    let t = randomTree g1
    -- Now run step over and over to get a stream of (tree,result,weight)s.
    let (samples,_) = runState (iterateM step t) g2
    -- The stream of seeds is used to produce a stream of result/weight pairs.
    return $ map fst $ map (runMeas m) $ map dualizeTree samples
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where step :: RandomGen g => Tree Double -> State g (Tree Double)
          step t = do
            g <- get
            let (g1, g2) = split g
            -- Call k, which proposes a new tree and gives the log ratio for it.
            let (logratio,t') = k g1 m t
            let (r, g2') = random g2
            put g2'
            if r < min 1 (exp $ logratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
              then return t' -- trace ("---- Weight: " ++ show w') w')
              else return t -- trace ("---- Weight: " ++ show w) w)

-- | Now here are four MH kernels: lmhKernel, grwKernel, srwKernel and malaKernel

-- | Lightweight MH algorithm from POPL 2023
mutateTreeLMH :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> Tree Double
mutateTreeLMH p g m (Tree a ts) =
  let (a',g') = (random g :: (Double,g)) in
  let (a'',g'') = (random g' :: (Double,g)) in
  if a' < p then Tree (a'') (mutateTreesLMH p g'' m ts) else Tree a (mutateTreesLMH p g' m ts)
mutateTreesLMH :: RandomGen g => Double -> g ->  Meas (Nagata Integer Double) a -> [Tree Double] -> [Tree Double]
mutateTreesLMH p g m (t:ts) = let (g1,g2) = split g in mutateTreeLMH p g1 m t : mutateTreesLMH p g2 m ts

lmhKernel :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
lmhKernel p g m t = 
  let t' = mutateTreeLMH p g m t in
  let (_,N w _) = runMeas m (dualizeTree t) in 
  let (_,N w' _) = runMeas m (dualizeTree t') in 
  (w' - w, t')  

circle x = x - (fromIntegral $ floor x) -- mapping from R to the circle, wrapping it round

-- | Gaussian Random walk
mutateTreeGRW :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> Tree Double
mutateTreeGRW sigma g m (Tree a ts) =
  let (a',g') = (random g :: (Double,g)) in
  Tree (circle (a + sigma * (invnormcdf a'))) (mutateTreesGRW sigma g' m ts)
mutateTreesGRW :: RandomGen g => Double -> g ->  Meas (Nagata Integer Double) a -> [Tree Double] -> [Tree Double]
mutateTreesGRW sigma g m (t:ts) = let (g1,g2) = split g in mutateTreeGRW sigma g1 m t : mutateTreesGRW sigma g2 m ts

grwKernel :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
grwKernel sigma g m t =
  let t' = mutateTreeGRW sigma g m t in
  let (_,N w _) = runMeas m (dualizeTree t) in 
  let (_,N w' _) = runMeas m (dualizeTree t') in 
  (w' - w, t')  

-- Lookup a vertex in a tree. Uses the same indexing as dualizeTree.
lookupTree :: Tree d -> Integer -> d
lookupTree (Tree x _) 0 = x
lookupTree (Tree x (t : ts)) n = if n `mod` 2 == 1 then lookupTree t (n `div` 2) else lookupTree (Tree x ts) (n `div` 2) 

merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | x==y = merge xs (y:ys)
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)


-- | MALA (Metropolis-adjusted Langevin algorithm)
-- | https://en.wikipedia.org/wiki/Metropolis-adjusted_Langevin_algorithm#Further_details
-- | Here, t is X_k and t' is X_{k+1}
malaKernel :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
malaKernel tau g m t =
  let (_,N w dw) = runMeas m (dualizeTree t) in
  let t'' = mutateTreeGRW (sqrt(2 * tau)) g m t in -- X_k + 2\tau Normal 
  let t' = fmap (gradientStep dw) (dualizeTree t'') in -- X_k + tau grad log pi (X_k) + 2\tau Normal 
  let (_,N w' dw') = runMeas m t' in
  -- Calculate log MH ratio
  -- The q(x'|x) requires calculating the l2 norm of the whole space, which is infinite-dimensional.
  -- But the ratio of the q's will cancel in all dimensions
  -- except where one or the other gradients is non-zero.
  -- Find the union of the sites with non-zero gradient.
  let sites = merge (M.keys dw) (M.keys dw') in 
  -- Find the log ratio as the sum of squares.
  -- It's ok to ignore dimensions where both gradients are zero.
  -- Helper function: circle distance
  let cdist x x' dx' = let x'' = circle (x' + tau * dx') in if (abs (x-x'') < 0.5) then (x-x'')^2 else (1-(x-x''))^2 in
  let qlogratio = (1/(4*tau))* (Prelude.sum [cdist (lookupTree t i) (primal (lookupTree t' i)) (M.findWithDefault 0 i dw') | i <- sites] - Prelude.sum [cdist (primal (lookupTree t' i)) (lookupTree t i) (M.findWithDefault 0 i dw) | i <- sites]) in
  (w' - w - qlogratio , fmap primal t') 
  -- NB This ratio is only correct if the standard deviation is low, so that the chance of going twice round the circle in one step is negligible.
  where 
    gradientStep dr (N x dx) = N (circle $ x + (tau * M.findWithDefault 0 key dr)) dx
      where key = head (M.keys dx) 



-- | Utilities for running MH

-- | Useful function which thins out a list.
every :: Int -> [a] -> [a]
every n xs = case drop (n -1) xs of
  (y : ys) -> y : every n ys
  [] -> []
iterateNM :: Int -> (a -> IO a) -> a -> IO [a]
iterateNM 0 f a = return []
iterateNM n f a = do
  a' <- f a
  as <- iterateNM (n -1) f a'
  return $ a : as

-- | Take eagerly from a list and print the current progress. 
takeWithProgress :: Int -> [a] -> IO [a]
takeWithProgress n = helper n n
  where
    helper :: Int -> Int -> [a] -> IO [a]
    helper _ i _ | i <= 0 = return []
    helper _ _ []        = return []
    helper n i ((!x):xs)    = do
      putStrLn $ "Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%"
      xs' <- helper n (i-1) xs
      return $ x : xs'

-- | Take as eagerly as possible from a list of tuples and print the current progress. 
hardTakeWithProgress :: NFData a => Int -> [a] -> IO [a]
hardTakeWithProgress n = helper n n
  where
    helper :: NFData a =>  Int -> Int -> [a] -> IO [a]
    helper _ i _ | i <= 0 = return []
    helper _ _ []        = return []
    helper n i (x:xs)    = do
      putStrLn $ x `deepseq` ("Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%")
      xs' <- helper n (i-1) xs
      return $ x : xs'

takeEager :: Int -> [a] -> [a]
takeEager n = helper n n
  where
    helper :: Int -> Int -> [a] -> [a]
    helper _ i _ | i <= 0 = []
    helper _ _ []        = []
    helper n i ((!x):xs) = x : helper n (i-1) xs

takeEveryDrop :: Int -> Int -> Int -> [a] -> [a]
takeEveryDrop nTake nEvery nDrop stream = 
  take nTake $ every nEvery $ drop nDrop stream

takeEagerEveryDrop :: Int -> Int -> Int -> [a] -> [a]
takeEagerEveryDrop nTake nEvery nDrop stream = 
  takeEager nTake $ every nEvery $ drop nDrop stream

takeProgressEveryDrop :: Int -> Int -> Int -> [a] -> IO [a]
takeProgressEveryDrop nTake nEvery nDrop stream = 
  takeWithProgress nTake $ every nEvery $ drop nDrop stream

maxWeightElement :: Ord w => [(a, w)] -> a
maxWeightElement aws =
  let maxw = maximum $ map snd aws
      (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
  a

maxWeightPair :: Ord w => [(a, w)] -> (a, w)
maxWeightPair aws =
  let maxw = maximum $ map snd aws
      (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
  (a, maxw)

