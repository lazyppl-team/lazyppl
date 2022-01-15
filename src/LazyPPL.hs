{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module LazyPPL where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random hiding (uniform)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, state , put, get, runState, runStateT, StateT)
import Numeric.Log

import GHC.Exts.Heap
import System.Mem
import Unsafe.Coerce
import Data.Maybe

import qualified Data.Map as M

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
                       newStdGen      
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
                     newStdGen
                     g <- getStdGen
                     let rs = (randoms g :: [Double])
                     return $ map (\r -> fst $ head $ filter (\(x,w)-> w>=(Exp $ log r) * max) xws') rs
accumulate ((x,w):xws) a = (x,w+a):(accumulate xws (w+a))
accumulate [] a = []

{-- MH: Produce a stream of samples, using Metropolis Hastings
    We use (mutatetree p) to propose different distributions.
    If p = 1/dimension then this is a bit like single-site lightweight MH.
    (Wingate, Stuhlmuller, Goodman, AISTATS 2011.) 
    If p = 1 then this is like multi-site lightweight MH 
--}

mh :: forall a. Double -> Meas a -> IO [(a,Product (Log Double))]
mh p (Meas m) = do
     -- Top level: produce a stream of samples.
     -- Split the random number generator in two
     -- One part is used as the first seed for the simulation,
     -- and one part is used for the randomness in the MH algorithm.
     newStdGen
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

{-- Single-site Metropolis-Hastings --}

-- A partial tree, used for examining finite evaluated subtrees of the working infinite
-- random tree.
data PTree = PTree (Maybe Double) [Maybe PTree] deriving Show

flatten :: PTree -> [Double]
flatten (PTree (Just x) xs)  = x : (concatMap flatten (catMaybes xs))
flatten (PTree Nothing xs)  = concatMap flatten (catMaybes xs)

-- Functions for truncating a tree.
getGCClosureData b = do c <- getBoxedClosureData b
                        case c of 
                          BlackholeClosure _ n -> getGCClosureData n
                          -- SelectorClosure _ n -> getGCClosureData n
                          _ -> return c

helperT :: Box -> IO PTree
helperT b = do
  c <- getGCClosureData b
  case c of
    ConstrClosure _ [n,l] [] _ _ "Tree" -> 
      do n' <- getGCClosureData n
         l' <- getGCClosureData l
         -- performGC
         case (n',l') of
           (ConstrClosure {dataArgs = [d], name = "D#"}, ConstrClosure {name = ":"}) ->
             do l'' <- helperB l
                return $ PTree (Just $ unsafeCoerce d) l'' 
           (ThunkClosure {}, ConstrClosure {name = ":"}) -> 
             do l'' <- helperB l
                return $ PTree Nothing l'' 
           (ConstrClosure {}, ThunkClosure {}) -> undefined
           (ThunkClosure {}, ThunkClosure {}) -> undefined
           (SelectorClosure {}, ThunkClosure {}) ->
             return $ PTree Nothing [] 
           (SelectorClosure {}, ConstrClosure {name = ":"}) ->
             do l'' <- helperB l
                return $ PTree Nothing l'' 
           _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
    ThunkClosure {} -> return $ PTree Nothing []

helperB :: Box -> IO [Maybe PTree]
helperB b = do
  c <- getGCClosureData b
  case c of
    ConstrClosure _ [n,l] [] _ _ ":" -> 
      do n' <- getGCClosureData n
         l' <- getGCClosureData l
         case (n',l') of
           (ConstrClosure {name = "Tree"}, ConstrClosure {name = ":"}) ->
             do n'' <- helperT n
                l'' <- helperB l
                return $ (Just n'') : l''
           (ConstrClosure {name = "Tree"}, ThunkClosure {}) ->
             do n'' <- helperT n
                return $ (Just n'') : []
           (ThunkClosure {}, ConstrClosure {name = ":"}) ->
             do l'' <- helperB l
                return $ Nothing : l''
           (ThunkClosure {}, ThunkClosure {}) ->
             return $ [] -- alternatively, Nothing : []
           _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
    ThunkClosure {} -> return []

trunc :: Tree -> IO PTree
trunc t = helperT $ asBox t

-- Applying substitutions to a PTree from a Map.
subst :: M.Map Double Double -> PTree -> PTree
subst ctx (PTree (Just x) xs) = PTree (Just $ M.findWithDefault x x ctx) (map (fmap (subst ctx)) xs)
subst ctx (PTree Nothing xs)  = PTree Nothing (map (fmap (subst ctx)) xs)

-- ProbCtx carries around a Map of node replacements in addition to the Tree from Prob.
newtype ProbCtx a = ProbCtx (State (M.Map Double Double,Tree) a)

instance Monad ProbCtx where
  return a = ProbCtx $ return a
  (ProbCtx m) >>= f = ProbCtx $
                        do (ctx,t) <- get
                           let (t1,t2) = splitTree t
                           put (ctx,t1)
                           x <- m
                           put (ctx,t2)
                           let (ProbCtx m') = f x
                           m'
instance Functor ProbCtx where fmap = liftM
instance Applicative ProbCtx where {pure = return ; (<*>) = ap}


-- runProbCtx runs a probability deterministically, given a source of randomness
-- and a "substition context/environment" which takes care of modifying specific
-- nodes when needed. 
runProbCtx :: ProbCtx a -> (M.Map Double Double, Tree) -> a
runProbCtx (ProbCtx a) (ctx,rs) = fst $ runState a (ctx,rs)

-- Defining the uniform distribution for ProbCtx. Slightly annoying that there's no 
-- seemingly easy way to describe it as a function of `uniform`.
uniformC :: ProbCtx Double
uniformC = ProbCtx $
      do (ctx,~(Tree r (t:ts))) <- get
         put (ctx,t)
         if M.member r ctx
         then return $ ctx M.! r
         else return r

-- An example probability distribution.
example :: ProbCtx Double
example = do choice <- uniformC
             w <- uniformC
             x <- uniformC
             y <- uniformC
             z <- uniformC
             case floor (choice * 4.0) of
               0 -> return w 
               1 -> return x  
               2 -> return y  
               3 -> return z  

mh1 :: forall a. (Show a) => ProbCtx a -> IO ()
mh1 pc = do
    newStdGen
    g <- getStdGen
    let (g1,g2) = split g
    let t = randomTree g1
    let x = runProbCtx pc (M.fromList [], t)
    putStrLn $ "Initial sample: " ++ show x -- we need this line to force evaluation of the line above
    trunc t >>= \s -> putStrLn $ "Initial tree: " ++ show s
    putStrLn "==="
    iterateNM 5 step (x,t,M.fromList [],g2)
    return ()
    where step :: RandomGen g => (a, Tree, M.Map Double Double, g) -> IO (a, Tree, M.Map Double Double, g)
          step (a,t,ctx,r) = do ptree <- trunc t
                                let plist = flatten ptree
                                let (r1,r2) = split r
                                let (i :: Double, r') = random r1 -- a random node from the evaluated nodes
                                let (v' :: Double, _) = random r2 -- the newly generated random node
                                let v = plist !! (floor $ i * (fromIntegral $ length plist))
                                let ctx' = M.alter (\_ -> Just v') v ctx
                                let a' = runProbCtx pc (ctx', t)
                                putStrLn $ show $ subst ctx ptree
                                putStrLn $ "Context: " ++ show ctx
                                putStrLn $ "Replacee: " ++ show (M.findWithDefault v v ctx)
                                putStrLn $ "Replacer: " ++ show v'
                                putStrLn $ "Sample: " ++ show a
                                putStrLn "---"
                                return (a',t,ctx',r2)
                       

{-- Useful function which thins out a list. --}
every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

iterateNM :: Int -> (a -> IO a) -> a -> IO [a]
iterateNM 0 f a = return []
iterateNM n f a = do a' <- f a
                     as <- iterateNM (n-1) f a'
                     return $ a : as

