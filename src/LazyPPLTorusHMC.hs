{-# LANGUAGE GeneralizedNewtypeDeriving, 
    ScopedTypeVariables,
    RankNTypes, BangPatterns, DeriveFunctor #-}
module LazyPPLTorusHMC where

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
import qualified Data.List as L (lookup, (\\))

import Debug.Trace

import AD
import Data.Number.Erf

{- | This file defines
    1. Two monads: 'Prob' (for probabilities) and 'Meas' (for unnormalized probabilities)
    2. 'mh' (Metropolis-Hastings algorithm, parameterized by a kernel)
    3. Three MH kernels: LMH (POPL 2023), Gaussian random walk, and MALA

    This differs from the original LazyPPL.hs in that it uses standard normals 
    instead of gaussians to populate the tree. 
-}

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

stdnormal :: Prob d d
stdnormal = Prob $ \(Tree r _) -> r

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
newtype Meas d a = Meas {unMeas :: WriterT (Sum d) (Prob d) a }
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

randomTreeNormal :: RandomGen g => g -> Tree Double
randomTreeNormal g = fmap invnormcdf (randomTree g)

{- | 'runProb' runs a probability deterministically, given a source of randomness -}
runProb :: Prob d a -> Tree d -> a
runProb (Prob a) = a

runMeas :: Meas d a -> Tree d -> (a,d)
runMeas (Meas m) t = let (x,Sum w) = runProb (runWriterT m) t in (x,w)

-- A minimal example with one dimension. 

minExample :: (Floating d) => Meas d d
minExample = do
   x <- sample stdnormal
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
     let r = runProb (runWriterT (unMeas p)) rs
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
       let (result,score) = runProb (runWriterT (unMeas p)) rs in
       (result,getSum score) : (go $ fmap (learn $ tangent $ getSum score) rs)

    learn dr (N x dx) = N (op x (alpha * M.findWithDefault 0 key dr)) dx
      where key = head (M.keys dx)

prettyAscent :: Int -> Double ->  Meas (Nagata Integer Double) a -> IO a
prettyAscent n alpha p = do
   rss <- gradientAscent alpha p
   forM [0..(n-1)] (\i -> do {let hdr = "* Iteration " ++ show (i + 1) ++ ":" in putStr hdr ; let (result,score) = (rss!!i) in putStrLn ("Log likelihood = " ++ show score) }) 
   return $ fst $  rss !! n

-- {-- | __Metropolis-Hastings__ (MH): Produce a stream of samples, using Metropolis Hastings
--     This is parameterized by a proposal kernel (with acceptance ratio).
-- 
--     The algorithm is as follows:
-- 
--     Top level: produces a stream of samples.
--     * Split the random number generator in two
--     * One part is used as the first seed for the simulation,
--     * and one part is used for the randomness in the MH algorithm.
-- 
--     Then, run 'step' over and over to get a stream of '(tree, result, weight)'s.
--     The stream of seeds is used to produce a stream of result/weight pairs.
-- 
--     NB There are three kinds of randomness in the step function.
--     1. The start tree 't', which is the source of randomness for simulating the
--     program m to start with. This is sort-of the point in the "state space".
--     2. The randomness needed to propose a new tree via the kernel ('g1')
--     3. The randomness needed to decide whether to accept or reject that ('g2')
--     The tree t is an argument and result,
--     but we use a state monad ('get'/'put') to deal with the other randomness '(g, g1, g2)'
-- 
--     Steps of the 'step' function:
--     1. Randomly change some sites, 
--     2. Rerun the model with the new tree, to get a new weight 'w''.
--     3. Compute the MH acceptance ratio. This is the probability of either returning the new seed or the old one.
--     4. Accept or reject the new sample based on the MH ratio.
--     
-- --}
-- 
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
    --let (g3, g4) = split g2
    let t = randomTree g1
    -- Now run step over and over to get a stream of (tree,result,weight)s.
    -- let (samples,_) = trace ("initialW:"++ show w ++ "lengthInitialTrace" ++ show (length (M.keys dw))) (runState (iterateM step t) g2)
    let (samples,_) = runState (iterateM step t) g2
    -- The stream of seeds is used to produce a stream of result/weight pairs.
    return $ map (fst . runMeas m . dualizeTree) samples
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
            let (r, g2') = (random g2)
            put g2'
            if r < min 1 (exp logratio) -- (trace ("-- Ratio: " ++ show logratio) (exp $ logratio))
              then return t' -- trace ("---- Weight: " ++ show w') w')
              else return t -- trace ("---- Weight: " ++ show w) w)

-- | Now here are three MH kernels: lmhKernel, grwKernel and malaKernel

-- | Lightweight MH algorithm from POPL 2023
mutateTreeLMH :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> Tree Double
mutateTreeLMH p g m (Tree a ts) =
  let (a',g') = (random g :: (Double,g)) in
  let (a'',g'') = (random g' :: (Double,g)) in
  if a' < p then Tree (invnormcdf a'') (mutateTreesLMH p g'' m ts) else Tree a (mutateTreesLMH p g' m ts)
mutateTreesLMH :: RandomGen g => Double -> g ->  Meas (Nagata Integer Double) a -> [Tree Double] -> [Tree Double]
mutateTreesLMH p g m (t:ts) = let (g1,g2) = split g in mutateTreeLMH p g1 m t : mutateTreesLMH p g2 m ts

lmhKernel :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
lmhKernel p g m t = 
  let t' = mutateTreeLMH p g m t in
  let (_,N w _) = runMeas m (dualizeTree t) in 
  let (_,N w' _) = runMeas m (dualizeTree t') in 
  (w' - w, t')  

circle x = x - (fromIntegral $ floor x)

-- | Gaussian Random walk
mutateTreeGRW :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> Tree Double
mutateTreeGRW sigma g m (Tree a ts) =
  let (a',g') = (random g :: (Double,g)) in
  Tree (circle (a + sigma * invnormcdf a')) (mutateTreesGRW sigma g' m ts)

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


-- hmcKernel - from MCMC using Hamiltonian dynamics by Neal
-- https://arxiv.org/abs/1206.1901

data LFConfig a = LFConfig {eps :: a , leapfrogSteps :: Int , selected :: Int}

simpleLeapfrog :: Double -> Int -> Meas (Nagata Integer Double) a -> Tree (Nagata Integer Double) -> Tree (Nagata Integer Double) -> [Integer] -> (Tree (Nagata Integer Double), Tree (Nagata Integer Double), [Integer])
simpleLeapfrog _ 0 m q p xs = (q,p, merge xs [])
simpleLeapfrog eps steps m q p xs = let
        q' = gradientStepQ eps p q
        (_,N w dU_dq) = runMeas m q'
        p' = fmap (gradientStepP 1 eps dU_dq) p
        ys = merge xs (M.keys dU_dq)
    in if steps == 1 then (q', p, ys) else simpleLeapfrog eps (steps-1) m q' p' ys

gradientStepQ :: Double -> Tree (Nagata Integer Double) -> Tree (Nagata Integer Double) -> Tree (Nagata Integer Double)
gradientStepQ eps (Tree x xs) (Tree y ys)= let
        (N p _) = x
        (N q dq) = y
        q' = circle (q + eps*p)
        ts = zipWith (gradientStepQ eps) xs ys
      in Tree (N q' dq) ts

gradientStepP :: Double -> Double -> M.Map Integer Double -> Nagata Integer Double -> Nagata Integer Double
gradientStepP a eps dr (N x dx) = N (x + a * (eps * M.findWithDefault 0 key dr)) dx
      where key = head (M.keys dx)



hmcKernel :: forall g a. RandomGen g => LFConfig Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
hmcKernel lfc g m q =
  -- t :: Tree Double
  -- (dualizeTree t) :: Tree (Nagata Integer Double)
  -- (runMeas m (dualizeTree t)) :: (Nagata Integer Double, Nagata Integer Double)
  -- w :: Double
  -- dw :: (Map Integer Double)
  let (LFConfig eps' steps' selected) = lfc in
  let (x, N w dU_dq) = runMeas m (dualizeTree q) in
  let (a, g') = random g :: (Double, g) in
  --let eps = (invnormcdf a)* (eps'/5) + eps' in
  --let eps = a* 0.01 + 0.00075 in 
  let eps = eps' in
  let (b, g'') = random g' :: (Double, g) in
  --let steps = (floor (11*b) - 6)*1 + steps' in
  --let steps = steps' - floor ((fromIntegral steps') * 0.25) + floor ((fromIntegral steps') * 0.5 * b) in
  let steps = steps' in
  let p = randomTreeNormal g'' in 
  let p' = fmap (gradientStepP 0.5 eps dU_dq) (dualizeTree p) in -- p_k - 1/2 epsilon grad log pi (q_k)
  let (q_prop, p'', allSites) = simpleLeapfrog eps steps m (dualizeTree q) p' (M.keys dU_dq) in
  let (x', N w' dU_dq') = runMeas m q_prop in
  -- let p_prop = fmap (\(N x xd) -> (N -x xd)) (fmap (gradientStepP 0.5 eps dU_dq') p'') in
  let p_prop = fmap (gradientStepP 0.5 eps dU_dq') p'' in
  -- let p_prop' = fmap (\(N x xd) -> (N -x xd)) p_prop in 
  -- Calculate log MH ratio
  -- Find the union of the sites with non-zero gradient.
  let sites = merge (M.keys dU_dq) (M.keys dU_dq') in
  -- Find the log ratio for p as the sum of squares.
  -- It's ok to ignore dimensions where both gradients are zero. 
  --let plogratio = trace ("initial: " ++ show (map (lookupTree p) (M.keys dU_dq)) ++ "proposed: " ++ show (map primal (map (lookupTree p_prop) (M.keys dU_dq')))) (Prelude.sum [(lookupTree p i)^2| i <- sites] - Prelude.sum [(primal (lookupTree p_prop i))^2 | i <- sites]) in
  let plogratio = Prelude.sum [lookupTree p i ^2| i <- allSites] - Prelude.sum [primal (lookupTree p_prop i) ^2 | i <- allSites] in
  (w' - w + plogratio/2, fmap primal q_prop) 
  --trace (show [lookupTree p i ^2| i <- M.keys dU_dq] ++ show [primal (lookupTree p_prop i)  | i <- sites] ++ "initial w:"++show w ++ " proposed w " ++ show w' ++ " pdiff: " ++ show (plogratio/2)++" log ratio: "++ show (w' - w + plogratio/2) ++ " ratio "++ show (exp (w' - w + plogratio/2))) (w' - w + plogratio/2 + qlogratio, fmap primal q_prop)


{- -- look ahead hmc with persistence, can be transformed into NP-iMCMC?
-- https://arxiv.org/abs/2211.01100

-- alpha between 0 and 1 - persistence parameter: alpha = 1 no persistence (equivalent to HMC), alpha = 0 full persistence
-- chances :: Integer - look ahead parameter: chances = 1 -> no extra chances
lahmcPersistence :: (LFConfig Double -> Meas (Nagata Integer Double) a -> (Tree Double, Tree Double) -> (Tree Double, Tree Double) -> Bool -> Double -> (Tree Double, Tree Double, Bool))  -> Meas (Nagata Integer Double) a -> LFConfig Double -> Double -> IO [a]
lahmcPersistence laIntegrator m lfc alpha = do
    newStdGen
    g <- getStdGen
    let (g1,g2) = split g :: (StdGen ,StdGen)
    let (g3, g4) = split g2 :: (StdGen, StdGen)
    --let (g3, g4) = split g2
    let q = randomTree g1
    let p = randomTree g3
    let (_,N w dU_dq) = runMeas m (dualizeTree q)
    -- Now run step over and over to get a stream of (tree,result,weight)s.
    let (samples,_) = trace ("initialW:"++ show w ++ "grad" ++ show dU_dq ++ " q " ++ show (map (lookupTree q) (M.keys dU_dq))) $ runState (iterateM step (q, p, True)) g4
    --let (samples,_) = runState (iterateM step (q, p, True)) g4
    -- The stream of seeds is used to produce a stream of result/weight pairs.
    return $ map fst $ map (runMeas m) $ map dualizeTree $ map (\(x, _, _) -> x) $ [(q, p, True)] ++ samples
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where step :: RandomGen g => (Tree Double, Tree Double, Bool) -> State g (Tree Double, Tree Double, Bool)
          step (q_old, p_old, d) = do
            g <- get
            let (g', g'') = split g
            let (r, g_1) = random g'
            let (a :: Double, g_2) = random g_1 
            let (b :: Double, g_3) = random g_2 
            --let eps = (invnormcdf a)* (eps'/5) + eps' in
            put g_3
            let (LFConfig eps steps selected) = lfc
            let eps' = b * eps + 0.00075
            --let steps = (floor (11*b) - 6)*1 + steps' in
            let steps' = floor (a*(fromIntegral steps+1)) + 10
            --let steps = steps' in
            let (x,y, d') = laIntegrator (LFConfig eps' steps' selected) m (q_old, p_old) (q_old, p_old) d r
            let q_new = if (d == d') then x else q_old 
            let p = if (d == d') then y else p_old
            let p' = fmap (\x -> sqrt(1-alpha^2)* x) p
            -- p_new is the modified momemtum: sqrt(1-\alpha^2) * p + \alpha * Normal
            let p_new = mutateTreeGRW (alpha) g'' m p'
            return (q_new, p_new, d')



lookaheadHMC :: Integer -> LFConfig Double -> Meas (Nagata Integer Double) a -> (Tree Double, Tree Double) -> (Tree Double, Tree Double) -> Bool -> Double -> (Tree Double, Tree Double, Bool)
lookaheadHMC 0 lfc m (q_0, p_0) (q, p) d r = (q_0, p_0, not d)
lookaheadHMC chances lfc m (q_0, p_0) (q, p) d r =
    let (logRatio, q_prop, p') = if d then (hmcKernel2 lfc m (q_0, p_0) (q, p)) else (hmcKernel2 lfc m (q_0, p_0) (q, (fmap (\x -> -x) p))) in
    let p_prop = if d then p' else (fmap (\x -> -x) p') in
    if (log r < logRatio) then (q_prop, p_prop, d) else (lookaheadHMC (chances-1) lfc m (q_0, p_0) (q_prop, p_prop) d r)




hmcKernel2 :: LFConfig Double -> Meas (Nagata Integer Double) a -> (Tree Double, Tree Double) -> (Tree Double, Tree Double) -> (Double, Tree Double, Tree Double)
hmcKernel2 lfc m (q_0, p_0) (q, p)=
  let (LFConfig eps steps selected) = lfc in
  let (_,N w dU_dq) = runMeas m (dualizeTree q) in
  let p' = fmap (gradientStepP 0.5 eps dU_dq) (dualizeTree p) in -- p_k - 1/2 epsilon grad log pi (q_k)
  let (q_prop, p'', _) = simpleLeapfrog eps steps m (dualizeTree q) p' [] in
  let (_,N w' dU_dq') = runMeas m q_prop in
  -- let p_prop = fmap (\(N x xd) -> (N -x xd)) (fmap (gradientStepP 0.5 eps dU_dq') p'') in
  let p_prop = (fmap (gradientStepP 0.5 eps dU_dq') p'') in
  -- Calculate log MH ratio
  -- Find the union of the sites with non-zero gradient.
  -- let sites = trace (show (M.keys dU_dq) ++ "proposed:" ++ show (M.keys dU_dq')) (merge (M.keys dU_dq) (M.keys dU_dq')) in
  let (_,N w_0 dU_dq_0) = runMeas m (dualizeTree q_0) in
  let sites = merge (M.keys dU_dq_0) (M.keys dU_dq') in
  -- Find the log ratio for p as the sum of squares.
  -- It's ok to ignore dimensions where both gradients are zero. 
  --let plogratio = trace (show sites ++ "initial: " ++ show (map (lookupTree p_0) (M.keys dU_dq_0)) ++ "proposed: " ++ show (map primal (map (lookupTree p_prop) (M.keys dU_dq')))) (Prelude.sum [(lookupTree p_0 i)^2| i <- sites] - Prelude.sum [(primal (lookupTree p_prop i))^2 | i <- sites]) in
  let plogratio = Prelude.sum [(lookupTree p_0 i)^2| i <- sites] - Prelude.sum [(primal (lookupTree p_prop i))^2 | i <- sites] in
  (w' - w_0 + plogratio/2, fmap primal q_prop, fmap primal p_prop) 
  --trace ("initial w:"++show w_0 ++ " proposed w " ++ show w' ++ " pdiff: " ++ show (plogratio/2)++" log ratio: "++ show (w' - w_0 + plogratio/2) ++ " ratio "++ show (w' - w_0 + plogratio/2)) (w' - w_0 + plogratio/2, fmap primal q_prop, fmap primal p_prop)
 -}


-- updateQP :: (Floating a, Ord a, Show a) => (a -> a) -> a -> (a, a) -> (a, a)
-- updateQP dV_dq eps (q, p) = let
--         p_half = p - eps/2 * (dV_dq q) -- half step
--         q' = q + eps * p_half -- full step
--         p' = p_half - eps/2 * (dV_dq q') -- finish half step
--     in (q', p')

-- simpleLeapfrog :: (Floating a, Ord a, Show a) => (a -> a) -> (LFConfig a) -> (a, a) -> (a, a)
-- simpleLeapfrog dV_dq (LFConfig eps steps selected) (q, p) = let
--         qps = take steps $ iterate (updateQP dV_dq eps) (q, p)
--         qs = map fst qps
--         ps = map snd qps
--     -- TODO make use of selected instead of just taking the last one
--     in (last qs, last ps)

-- hmcProposal :: (Floating a, Ord a, Show a) => (LFConfig a) -> (a, a) -> (a, a)
-- hmcProposal lfc (q, p) = let
--         (q', p') = simpleLeapfrog dV_dq lfc (q, p)
--     in (q', -p')

-- hmcAcceptReject :: (RandomGen g, Floating a, Ord a, Random a, Show a) => LFConfig a -> ((a, a), g) -> ((a, a), g)
-- hmcAcceptReject lfc ((q, _), rng) = let
--         (p, rng') = normal rng -- assume the proposal distribution for momentum is N(0, 1)
--         (new_q, new_p) = hmcProposal lfc (q, p)
--         (u, rng'') = randomR (0.0, 1.0) rng'
--         (q', p') = if (log u) < (-(h new_q new_p) + (h q p)) then (new_q, new_p) else (q, p)
--     in ((q', p'), rng'')

-- -- | Gaussian Random walk
-- mutateTreeHMC :: forall g a. RandomGen g => (LFConfig a) -> g -> Meas (Nagata Integer Double) a -> Tree Double -> Tree Double
-- mutateTreeHMC lfc g m (Tree p ts) =
--   let (a', g') = (random g :: (Double,g)) in
--   let (new_q, new_p) = hmcProposal lfc (q, p) in
--   Tree (new_q, new_p) (mutateTreeHMC lfc g' m ts)

-- mutateTreesHMC :: RandomGen g => Double -> g ->  Meas (Nagata Integer Double) a -> [Tree Double] -> [Tree Double]
-- mutateTreesHMC lfc g m (t:ts) = let (g1,g2) = split g in mutateTreeHMC lfc g1 m t : mutateTreesGRW lfc g2 m ts

-- hmcKernel :: forall g a. RandomGen g => Double -> g -> Meas (Nagata Integer Double) a -> Tree Double -> (Double, Tree Double)
-- hmcKernel tau g m t =
--   -- t :: Tree Double
--   -- (dualizeTree t) :: Tree (Nagata Integer Double)
--   -- (runMeas m (dualizeTree t)) :: (Nagata Integer Double, Nagata Integer Double)
--   -- w :: Double
--   -- dw :: (Map Integer Double)

--   let (_,N w dw) = runMeas m (dualizeTree t) in
--   let t'' = mutateTreeHMC (sqrt(2 * tau)) g m t in -- X_k + 2\tau Normal 
--   let t' = fmap (gradientStep dw) (dualizeTree t'') in -- X_k + tau grad log pi (X_k) + 2\tau Normal 
--   let (_,N w' dw') = runMeas m t' in
--   -- Calculate log MH ratio
--   -- The q(x'|x) requires calculating the l2 norm of the whole space, which is infinite-dimensional.
--   -- But the ratio of the q's will cancel in all dimensions
--   -- except where one or the other gradients is non-zero.
--   -- Find the union of the sites with non-zero gradient.
--   let sites = merge (M.keys dw) (M.keys dw') in
--   -- Find the log ratio as the sum of squares.
--   -- It's ok to ignore dimensions where both gradients are zero. 
--   let qlogratio = (1/(4*tau))* (Prelude.sum [(lookupTree t i - primal (lookupTree t' i) - tau * (M.findWithDefault 0 i dw'))^2 | i <- sites] 
--                                 - Prelude.sum [(primal (lookupTree t' i) - lookupTree t i - tau * (M.findWithDefault 0 i dw))^2 | i <- sites]) in
--   (w' - w - qlogratio , fmap primal t') 
--   where 
--     gradientStep dr (N x dx) = N (x + (tau * M.findWithDefault 0 key dr)) dx
--       where key = head (M.keys dx) 



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

