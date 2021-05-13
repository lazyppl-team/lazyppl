{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyPPL where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Numeric.Log
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe

{--
    This file defines
    * Two monads: Prob (for probabilities) and Meas (for unnormalized probabilities)
    * Two inference methods: lwis and mh
--}

-- A "Tree" is a lazy, infinitely wide and infinitely deep tree, labelled by Doubles
-- Our source of randomness will be a Tree, populated by uniform [0,1] choices for each label.
-- Often people would just use a list or stream instead of a tree.
-- But a tree allows us to be lazy about how far we are going all the time.
data Tree = Tree Double [Tree]

-- A probability distribution over a is a state transformer over trees
-- ie a function Tree -> (a , Tree)
-- The idea is that it uses up bits of the tree as it runs
newtype Prob a = Prob (State Tree a)

-- Two key things to do with trees:
-- Split tree splits a tree in two (bijectively)
-- Get the label at the head of the tree and discard the rest
splitTree :: Tree -> (Tree , Tree)
splitTree (Tree r (t : ts)) = (t , Tree r ts)

uniform :: Prob Double
uniform = Prob $
      do ~(Tree r (t:ts)) <- get
         put t
         return r


-- Probabilities for a monad.
-- Sequencing is done by splitting the tree
-- and using different bits for different computations.
instance Monad Prob where
  return a = Prob $ return a
  (Prob m) >>= f = Prob $
                        do g <- get
                           let (g1,g2) = splitTree g
                           put g1
                           x <- m
                           put g2
                           let (Prob m') = f x
                           m'
instance Functor Prob where fmap = liftM
instance Applicative Prob where {pure = return ; (<*>) = ap}

{-- An unnormalized measure is represented by a probability distribution over pairs of a weight and a result --}
newtype Meas a = Meas (WriterT (Product (Log Double)) Prob a)
  deriving(Functor, Applicative, Monad)

{-- The two key methods for Meas are sample (from a probability) and score (aka factor, weight) --}
score :: Double -> Meas ()
score r = Meas $ tell $ Product $ (Exp . log) $ (if r==0 then exp(-300) else r)

scorelog :: Double -> Meas ()
scorelog r = Meas $ tell $ Product $ Exp $ (if r==0 then exp(-300) else r)

sample :: Prob a -> Meas a
sample p = Meas $ lift p

{-- Preliminaries for the simulation methods. Generate a tree with uniform random labels
    This uses SPLIT to split a random seed --}
randomTree :: RandomGen g => g -> Tree
randomTree g = let (a,g') = random g in Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree]
randomTrees g = let (g1,g2) = split g in (randomTree g1) : (randomTrees g2)

{-- Run prob runs a probability deterministically, given a source of randomness --}
runProb :: Prob a -> Tree -> a
runProb (Prob a) rs = fst $ runState a rs

{-- weightedsamples runs a probability measure and gets out a stream of (result,weight) pairs --}
weightedsamples :: forall a. Meas a -> IO [(a,Log Double)]
weightedsamples (Meas m) =
                    do let helper :: Prob [(a,(Product (Log Double)))]
                           helper = do
                             (x, w) <- runWriterT m
                             rest <- helper
                             return $ (x,w) : rest
                       g <- getStdGen
                       let rs = randomTree g
                       let xws = runProb helper rs
                       return $ map (\(x,w) -> (x,getProduct w)) xws

{-- Likelihood weighted importance sampling first draws n weighted samples,
    and then samples a stream of results from that regarded as an empirical distribution --}
lwis :: Int -> Meas a -> IO [a]
lwis n m =        do xws <- weightedsamples m
                     let xws' = take n $ accumulate xws 0
                     let max = snd $ last xws'
                     g <- getStdGen
                     let rs = (randoms g :: [Double])
                     return $ map (\r -> fst $ head $ filter (\(x,w)-> w>=(Exp $ log r) * max) xws') rs
accumulate ((x,w):xws) a = (x,w+a):(accumulate xws (w+a))
accumulate [] a = []



{-- MH: Produce a stream of samples, using Metropolis Hastings
    We use (mutatetree p) to propose different distributions.
    If p = 1/dimension then this is a bit like single-site lightweight MH.
    If p = 1 thimport Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe
en this is like multi-site lightweight MH --}
mh :: forall a. Double -> Meas a -> IO [(a,Product (Log Double))]
mh p (Meas m) = do
     -- Top level: produce a stream of samples.
     -- Split the random number generator in two
     -- One part is used as the first seed for the simulation,
     -- and one part is used for the randomness in the MH algorithm.
     g <- getStdGen
     let (g1,g2) = split g
     let t = randomTree g1
     let (x, w) = runProb (runWriterT m) t
     -- Now run step over and over to get a stream of (tree,result,weight)s.
     let (samples,_) = runState (iterateM step (t,x,w)) g2
     -- The stream of seeds is used to produce a stream of result/weight pairs.
     return $ map (\(t,x,w) -> (x,w)) $ samples
     -- NB There are three kinds of randomness in the step function.
     -- * The start tree t, which is the source of randomness for simulating the
     --   program m to start with. This is sort-of the point in the "state space".
     -- * The randomness needed to propose a new tree (g1)
     -- * The randomness needed to decide whether to accept or reject that (g2)
     -- The tree t is an argument and result,
     -- but we use a state monad (get/put) to deal with the other randomness (g,g1,g2)
     where step :: RandomGen g => (Tree,a,Product (Log Double)) -> State g (Tree,a,Product (Log Double))
           step (t,x,w) = do
             -- Randomly change some sites
             g <- get
             let (g1,g2) = split g
             let t' = mutateTree p g1 t
             -- Rerun the model with the new tree, to get a new
             -- weight w'.
             let (x', w') = runProb (runWriterT m) t'
             -- MH acceptance ratio. This is the probability of either
             -- returning the new seed or the old one.
             let ratio = (getProduct w') / (getProduct w )
             let (r,g2') = random g2
             put g2'
             if r < (min 1 (exp $ ln $ ratio)) then return (t',x',w') else return (t,x,w)


-- Replace the labels of a tree randomly, with probability p
mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
mutateTree p g (Tree a ts) = let (a',g') = (random g :: (Double,g)) in let (a'',g'') = random g' in
                                  Tree (if a'<p then a'' else a) (mutateTrees p g'' ts)
mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
mutateTrees p g (t:ts) = let (g1,g2) = split g in (mutateTree p g1 t) : (mutateTrees p g2 ts)



{-- Useful function which thins out a list. --}
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


{-- Stochastic memoization.
    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If a is finite, we could just sample all values of a in advance
    and avoid unsafePerformIO.
    If it is countably infinite, there probably also implementation tricks.
--}
memoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
memoize f = Prob $ do g <- get
                      let ( (Tree _ gs), g2) = splitTree g
                      put g2
                      return $ unsafePerformIO $ do
                                ref <- newIORef Data.Map.empty
                                return $ \x -> unsafePerformIO $ do
                                          m <- liftM (Data.Map.lookup x) (readIORef ref)
                                          case m of
                                              Just y -> return y
                                              Nothing -> do let (Prob m) = f x
                                                            n <- readIORef ref
                                                            let (y,_) = runState m (gs !! (1 + size n))
                                                            modifyIORef' ref (Data.Map.insert x y)
                                                            return y
