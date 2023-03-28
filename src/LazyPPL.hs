{-# LANGUAGE GeneralizedNewtypeDeriving, 
    ScopedTypeVariables,
    RankNTypes, BangPatterns, DeriveFunctor #-}
module LazyPPL where

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

{- | This file defines
    1. Two monads: 'Prob' (for probabilities) and 'Meas' (for unnormalized probabilities)
    2. Three inference methods: 'lwis' (likelihood weighted importance sampling)
    3. 'mh' (Metropolis-Hastings algorithm based on lazily mutating parts of the tree at random)
    3. 'mh1' (Single-site Metropolis-Hastings algorithm, akin to Wingate et al. It is only this that uses GHC.Exts.Heap and System.IO.Unsafe.)
-}

-- TOM: Compared to the original definitions, 
--      the types are now all parameterized in the type of numbers d
--      instead of having Double hardwired.

-- | A 'Tree' is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles
-- | Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
-- | Often people would just use a list or stream instead of a tree.
-- | But a tree allows us to be lazy about how far we are going all the time.
data Tree d = Tree d [Tree d]
  deriving (Functor, Show)

-- | A probability distribution over a is 
-- | a function 'Tree -> a'
-- | The idea is that it uses up bits of the tree as it runs
newtype Prob d a = Prob (Tree d -> a)

-- | Two key things to do with trees:
-- | Split tree splits a tree in two (bijectively)
-- | Get the label at the head of the tree and discard the rest
splitTree :: Tree d -> (Tree d , Tree d)
splitTree (Tree r (t : ts)) = (t , Tree r ts)

uniform :: Prob d d
uniform = Prob $ \(Tree r _) -> r


-- | Probabilities for a monad.
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

{- | An unnormalized measure is represented by a probability distribution over pairs of a weight and a result -}
-- newtype Meas d a = Meas (WriterT (Product (Log d)) (Prob d) a)
--  deriving(Functor, Applicative, Monad)
newtype Meas d a = Meas {unMeas :: (WriterT (Sum d) (Prob d) a) }
  deriving(Functor, Applicative, Monad)

{- | The two key methods for Meas are sample (from a probability) and score (aka factor, weight) -}
score :: Floating d => d -> Meas d ()
-- score r = Meas $ tell $ Product $ (Exp . log) r -- (if r==0 then exp(-300) else r)
score r = Meas $ tell $ Sum $ log r -- (if r==0 then exp(-300) else r)

-- scoreLog :: Log d -> Meas d ()
scoreLog :: d -> Meas d ()
scoreLog r = Meas $ tell $ Sum r

-- scoreProductLog :: Product (Log d) -> Meas d ()
--scoreProductLog :: Product (Log d) -> Meas d ()
--scoreProductLog r = Meas $ tell r

sample :: Num d => Prob d a -> Meas d a
sample p = Meas $ lift p

{- | Preliminaries for the simulation methods. Generate a tree with uniform random labels
    This uses 'split' to split a random seed -}
randomTree :: RandomGen g => g -> Tree Double
randomTree g = let (a,g') = random g in Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree Double]
randomTrees g = let (g1,g2) = split g in randomTree g1 : randomTrees g2

{- | 'runProb' runs a probability deterministically, given a source of randomness -}
runProb :: Prob d a -> Tree d -> a
runProb (Prob a) = a

-- A minimal example with one dimension. Generates the triangle-shaped histogram. 

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
     
-- -------------------------------------------------------------------------------
-- A variant of minExample that scores based on the distance from 0.5

trivialExample :: (Floating d) => Meas d d
trivialExample =
  do x <- sample uniform
     score ((0.5 - x) * (0.5 - x))
     return x

-- -------------------------------------------------------------------------------

-- infinite sequence of uniform samples
iiduniform :: Prob d [d]
iiduniform = do 
  x <- uniform
  xs <- iiduniform
  return $ x : xs

-- scale an infinite sequence of uniform samples by a random value, "a";
-- use these as steps in a random forwards-only walk 
-- use score to maximize the distance after the first 10 steps 
-- in consequence, "a" will be approx 1. 
minExampleNonParam :: (Floating d) => Meas d d
minExampleNonParam = do
  a <- sample uniform
  steps <- sample $ (map (a *)) <$> iiduniform
  let xs = scanl1 (+) steps
  let d10 = xs !! 10
  score d10 
  return d10

testNonParam :: IO ()
testNonParam = gradientAscent 100 0.01 minExampleNonParam

-- | Gradient ascent implementation that takes a number of iterations, a learning factor and
--   a measurement program. It maximizes the measurement program's score.
gradientAscent ::Show a => Int -> Double -> Meas (Nagata Integer Double) a -> IO ()
gradientAscent = gradientOptimize (+) 

gradientDescent :: Show a => Int -> Double -> Meas (Nagata Integer Double) a -> IO ()
gradientDescent = gradientOptimize (-) 

gradientOptimize :: Show a => (Double -> Double -> Double) -> Int -> Double -> Meas (Nagata Integer Double) a -> IO ()
gradientOptimize op n alpha p =
  do newStdGen
     g <- getStdGen
     let rs = dualizeTree (randomTree g)
     go n rs
  where
    go 0 rs = return ()
    go i rs = 
     do let hdr = "* Iteration " ++ show (n - i + 1) ++ ":"
        putStr hdr
        let (result,score) = (runProb (runWriterT (unMeas p)) rs)
        putStrLn (" Result = " ++ show result ++ "; Log likelihood = " ++ show (primal $ getSum score))-- (primal objective))
        let s = getSum score
        go (i-1) (fmap (learn (tangent s)) rs)

    learn dr (N x dx) = N (min (max 0 (op x (alpha * M.findWithDefault 0 key dr))) 1) dx
      where key = head (M.keys dx) 

-- Typical example: gradientAscent 30 0.01 (primal <$> minExample)
-- Typical example: gradientAscent 30 0.01 (primal <$> trivialExample)

-- {- | 'weightedsamples' runs a probability measure and gets out a stream of (result,weight) pairs -}
-- weightedsamples :: forall a d. Meas d a -> IO [(a,Log d)]
-- weightedsamples (Meas m) =
--   do
--     let helper :: Prob d [(a, Product (Log d))]
--         helper = do
--           (x, w) <- runWriterT m
--           rest <- helper
--           return $ (x, w) : rest
--     newStdGen
--     g <- getStdGen
--     let rs = randomTree g
--     let xws = runProb helper rs
--     return $ map (\(x,w) -> (x, getProduct w)) xws
-- 
-- {- | Likelihood weighted importance sampling first draws n weighted samples,
--     and then samples a stream of results from that regarded as an empirical distribution -}
-- lwis :: Int -> Meas d a -> IO [a]
-- lwis n m = do
--   xws <- weightedsamples m
--   let xws' = take n $ accumulate xws 0
--   let max = snd $ last xws'
--   newStdGen
--   g <- getStdGen
--   let rs = (randoms g :: [Double])
--   return $ map (\r -> fst $ head $ filter (\(x, w) -> w >= Exp (log r) * max) xws') rs
-- 
-- accumulate ((x, w) : xws) a = (x, w + a) : (x, w + a) : accumulate xws (w + a)
-- accumulate [] a = []
-- 
-- {-- | __Metropolis-Hastings__ (MH): Produce a stream of samples, using Metropolis Hastings
--     We use 'mutatetree p' to propose different distributions.
--     If p = 1/dimension then this is a bit like single-site lightweight MH.
--     (Wingate, Stuhlmuller, Goodman, AISTATS 2011.) 
--     If p = 1 then this is like multi-site lightweight MH 
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
--     2. The randomness needed to propose a new tree ('g1')
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
-- mh :: forall a. Double -> Meas a -> IO [(a,Product (Log Double))]
-- mh p (Meas m) = do
--     -- Top level: produce a stream of samples.
--     -- Split the random number generator in two
--     -- One part is used as the first seed for the simulation,
--     -- and one part is used for the randomness in the MH algorithm.
--     newStdGen
--     g <- getStdGen
--     let (g1,g2) = split g
--     let t = randomTree g1
--     let (x, w) = runProb (runWriterT m) t
--     -- Now run step over and over to get a stream of (tree,result,weight)s.
--     let (samples,_) = runState (iterateM step (t,x,w)) g2
--     -- The stream of seeds is used to produce a stream of result/weight pairs.
--     return $ map (\(_,x,w) -> (x,w)) samples
--     {- NB There are three kinds of randomness in the step function.
--     1. The start tree 't', which is the source of randomness for simulating the
--     program m to start with. This is sort-of the point in the "state space".
--     2. The randomness needed to propose a new tree ('g1')
--     3. The randomness needed to decide whether to accept or reject that ('g2')
--     The tree t is an argument and result,
--     but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
--     where step :: RandomGen g => (Tree,a,Product (Log Double)) -> State g (Tree,a,Product (Log Double))
--           step (t, x, w) = do
--             -- Randomly change some sites
--             g <- get
--             let (g1, g2) = split g
--             let t' = mutateTree p g1 t
--             -- Rerun the model with the new tree, to get a new
--             -- weight w'.
--             let (x', w') = runProb (runWriterT m) t'
--             -- MH acceptance ratio. This is the probability of either
--             -- returning the new seed or the old one.
--             let ratio = getProduct w' / getProduct w
--             let (r, g2') = random g2
--             put g2'
--             if r < min 1 (exp $ ln ratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
--               then return (t', x', w') -- trace ("---- Weight: " ++ show w') w')
--               else return (t, x, w) -- trace ("---- Weight: " ++ show w) w)
-- 
-- 
-- -- | Replace the labels of a tree randomly, with probability p
-- mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
-- mutateTree p g (Tree a ts) =
--   let (a',g') = (random g :: (Double,g)) in
--   let (a'',g'') = random g' in
--   Tree (if a'<p then a'' else a) (mutateTrees p g'' ts)
-- mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
-- mutateTrees p g (t:ts) = let (g1,g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts
-- 
-- 
-- 
-- {- | Irreducible form of 'mh'. Takes 'p' like 'mh', but also 'q', which is the chance of proposing an all-sites change. -}
-- 
-- mhirreducible :: forall a. Double -> Double -> Meas a -> IO [(a,Product (Log Double))]
-- mhirreducible p q (Meas m) = do
--     -- Top level: produce a stream of samples.
--     -- Split the random number generator in two
--     -- One part is used as the first seed for the simulation,
--     -- and one part is used for the randomness in the MH algorithm.
--     newStdGen
--     g <- getStdGen
--     let (g1,g2) = split g
--     let t = randomTree g1
--     let (x, w) = runProb (runWriterT m) t
--     -- Now run step over and over to get a stream of (tree,result,weight)s.
--     let (samples,_) = runState (iterateM step (t,x,w)) g2
--     -- The stream of seeds is used to produce a stream of result/weight pairs.
--     return $ map (\(t,x,w) -> (x,w)) samples
--     {- NB There are three kinds of randomness in the step function.
--     1. The start tree 't', which is the source of randomness for simulating the
--     program m to start with. This is sort-of the point in the "state space".
--     2. The randomness needed to propose a new tree ('g1')
--     3. The randomness needed to decide whether to accept or reject that ('g2')
--     The tree t is an argument and result,
--     but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
--     where step :: RandomGen g => (Tree,a,Product (Log Double)) -> State g (Tree,a,Product (Log Double))
--           step (t,x,w) = do
--             -- Randomly change some sites
--             g <- get
--             let (g1,g2) = split g
--             -- Decide whether to resample all sites (r<q) or just some of them
--             let (r,g1') = random g1
--             let t' = if r<q then randomTree g1' else mutateTree p g1' t
--             -- Rerun the model with the new tree, to get a new
--             -- weight w'.
--             let (x', w') = runProb (runWriterT m) t'
--             -- MH acceptance ratio. This is the probability of either
--             -- returning the new seed or the old one.
--             let ratio = getProduct w' / getProduct w
--             let (r,g2') = random g2
--             put g2'
--             if r < min 1 (exp $ ln ratio) then return (t',x',w') else return (t,x,w)
-- 
-- 
-- 
-- {-- | Single-site Metropolis-Hastings --}
-- 
-- -- | A partial tree, used for examining finite evaluated subtrees of the working infinite
-- -- | random tree.
-- data PTree = PTree (Maybe Double) [Maybe PTree] deriving Show
-- 
-- type Site = [Int]
-- type Subst = M.Map Site Double
-- 
-- getSites :: PTree -> [Site]
-- getSites p = getSites' p []
-- 
-- getSites' :: PTree -> Site  -> [Site]
-- getSites' (PTree (Just v) ts) site = site : concat [ getSites' t (n:site)  | (n, Just t) <- zip [0..] ts ]
-- getSites' (PTree Nothing  ts) site = concat [ getSites' t (n:site) | (n, Just t) <- zip [0..] ts ]
-- 
-- mutateNode :: Tree -> Site -> Double -> Tree
-- mutateNode (Tree _ ts) []     d = Tree d ts
-- mutateNode (Tree v ts) (n:ns) d = Tree v $ take n ts ++ mutateNode (ts!!n) ns d : drop (n+1) ts
-- 
-- mutateNodes :: Tree -> Subst -> Tree
-- mutateNodes = M.foldrWithKey (\k d t -> mutateNode t k d)
-- 
-- randomElement :: RandomGen g => g -> [a] -> (g, a)
-- randomElement g xs = if null xs then error "0d sample space" else (g', xs !! n)
--   where (n, g') = uniformR (0, length xs - 1) g
--                -- ^^^^^^^^ 
--                -- Depending on the version of `random` this can be either uniformR or randomR.
-- 
-- mh1 :: forall a. NFData a => Meas a -> IO [(a, Product (Log Double))]
-- mh1 (Meas m) = do
--     newStdGen
--     g <- getStdGen
--     let (gTree,g') = split g
--         tree = randomTree gTree
--         (x0, w0) = runProb (runWriterT m) tree
--         p = unsafePerformIO $ do { evaluate (rnf x0); evaluate (rnf w0) ; trunc tree }
--         samples = map (\(_,_,_,_,s) -> s) $ iterate step (gTree, g', M.empty, p, (x0, w0))
--     return samples
--   where step :: RandomGen g => (g, g, Subst, PTree, (a, Product (Log Double))) -> (g, g, Subst, PTree, (a, Product (Log Double)))
--         step (treeSeed, seed, sub, ptree, (x,w)) =
--             let (seed1, seed2) = split seed
--                 sites = getSites ptree
--                 (seed1', randSite) = (\(x,y) -> (x, reverse y)) $ randomElement seed1 sites
--                 (newNode :: Double, seed1'') = random seed1'
--                 (u :: Double, _) = random seed1'' 
--                 sub' = M.insert randSite newNode sub
--                 t' = mutateNodes (randomTree treeSeed) sub'
--                 (x',w') = runProb (runWriterT m) t'
--                 ptree' = unsafePerformIO $ do { evaluate (rnf x'); evaluate (rnf w'); trunc t' }
--                 sites' = getSites ptree'
--                 alpha = fromIntegral (length sites) / fromIntegral (length sites')
--                       * exp (ln (getProduct w' / getProduct w))
--             in if u <= alpha
--               then (treeSeed, seed2, sub', ptree', (x',w'))
--               else (treeSeed, seed2, sub,  ptree,  (x,w))
-- 
-- -- Functions for truncating a tree.
-- getGCClosureData b = do c <- getBoxedClosureData b
--                         case c of
--                           BlackholeClosure _ n -> getGCClosureData n
--                           -- SelectorClosure _ n -> getGCClosureData n
--                           _ -> return c
-- 
-- helperT :: Box -> IO PTree
-- helperT b = do
--   c <- getGCClosureData b
--   case c of
--     ConstrClosure _ [n,l] [] _ _ "Tree" ->
--       do  n' <- getGCClosureData n
--           l' <- getGCClosureData l
--           -- performGC
--           case (n',l') of
--             (ConstrClosure {dataArgs = [d], name = "D#"}, ConstrClosure {name = ":"}) ->
--               do  l'' <- helperB l
--                   return $ PTree (Just $ unsafeCoerce d) l''
--             (ThunkClosure {}, ConstrClosure {name = ":"}) ->
--               do  l'' <- helperB l
--                   return $ PTree Nothing l''
--             (ConstrClosure {dataArgs = [d], name = "D#"}, ThunkClosure {}) ->
--                   return $ PTree (Just $ unsafeCoerce d) []
--             (SelectorClosure {}, ThunkClosure {}) ->
--               return $ PTree Nothing []
--             (SelectorClosure {}, ConstrClosure {name = ":"}) ->
--               do  l'' <- helperB l
--                   return $ PTree Nothing l''
--             _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
--     ThunkClosure {} -> return $ PTree Nothing []
-- 
-- helperB :: Box -> IO [Maybe PTree]
-- helperB b = do
--   c <- getGCClosureData b
--   case c of
--     ConstrClosure _ [n,l] [] _ _ ":" ->
--       do  n' <- getGCClosureData n
--           l' <- getGCClosureData l
--           case (n',l') of
--             (ConstrClosure {name = "Tree"}, ConstrClosure {name = ":"}) ->
--               do  n'' <- helperT n
--                   l'' <- helperB l
--                   return $ Just n'' : l''
--             (ConstrClosure {name = "Tree"}, ThunkClosure {}) ->
--               do  n'' <- helperT n
--                   return [Just n'']
--             (ThunkClosure {}, ConstrClosure {name = ":"}) ->
--               do  l'' <- helperB l
--                   return $ Nothing : l''
--             (ThunkClosure {}, ThunkClosure {}) ->
--               return [] -- alternatively, Nothing : []
--             _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
--     ThunkClosure {} -> return []
-- 
-- trunc :: Tree -> IO PTree
-- trunc t = helperT $ asBox t
-- 
-- {-- | Useful function which thins out a list. --}
-- every :: Int -> [a] -> [a]
-- every n xs = case drop (n -1) xs of
--   (y : ys) -> y : every n ys
--   [] -> []
-- 
-- iterateNM :: Int -> (a -> IO a) -> a -> IO [a]
-- iterateNM 0 f a = return []
-- iterateNM n f a = do
--   a' <- f a
--   as <- iterateNM (n -1) f a'
--   return $ a : as
-- 
-- -- | Take eagerly from a list and print the current progress. 
-- takeWithProgress :: Int -> [a] -> IO [a]
-- takeWithProgress n = helper n n
--   where
--     helper :: Int -> Int -> [a] -> IO [a]
--     helper _ i _ | i <= 0 = return []
--     helper _ _ []        = return []
--     helper n i ((!x):xs)    = do
--       putStrLn $ "Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%"
--       xs' <- helper n (i-1) xs
--       return $ x : xs'
-- 
-- -- | Take as eagerly as possible from a list of tuples and print the current progress. 
-- hardTakeWithProgress :: NFData a => Int -> [a] -> IO [a]
-- hardTakeWithProgress n = helper n n
--   where
--     helper :: NFData a =>  Int -> Int -> [a] -> IO [a]
--     helper _ i _ | i <= 0 = return []
--     helper _ _ []        = return []
--     helper n i (x:xs)    = do
--       putStrLn $ x `deepseq` ("Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%")
--       xs' <- helper n (i-1) xs
--       return $ x : xs'
-- 
-- takeEager :: Int -> [a] -> [a]
-- takeEager n = helper n n
--   where
--     helper :: Int -> Int -> [a] -> [a]
--     helper _ i _ | i <= 0 = []
--     helper _ _ []        = []
--     helper n i ((!x):xs) = x : helper n (i-1) xs
-- 
-- takeEveryDrop :: Int -> Int -> Int -> [a] -> [a]
-- takeEveryDrop nTake nEvery nDrop stream = 
--   take nTake $ every nEvery $ drop nDrop stream
-- 
-- takeEagerEveryDrop :: Int -> Int -> Int -> [a] -> [a]
-- takeEagerEveryDrop nTake nEvery nDrop stream = 
--   takeEager nTake $ every nEvery $ drop nDrop stream
-- 
-- takeProgressEveryDrop :: Int -> Int -> Int -> [a] -> IO [a]
-- takeProgressEveryDrop nTake nEvery nDrop stream = 
--   takeWithProgress nTake $ every nEvery $ drop nDrop stream
-- 
-- maxWeightElement :: Ord w => [(a, w)] -> a
-- maxWeightElement aws =
--   let maxw = maximum $ map snd aws
--       (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
--   a
-- 
-- maxWeightPair :: Ord w => [(a, w)] -> (a, w)
-- maxWeightPair aws =
--   let maxw = maximum $ map snd aws
--       (Just a) = L.lookup maxw $ map (\(a, w) -> (w, a)) aws in
--   (a, maxw)
-- 
-- -- | An example probability distribution.
-- exampleProb :: Prob Double
-- exampleProb = do
--   choice <- uniform
--   w <- uniform
--   x <- uniform
--   y <- uniform
--   z <- uniform
--   case floor (choice * 4.0) of
--     0 -> return w
--     1 -> return x
--     2 -> return y
--     3 -> return z
-- 
