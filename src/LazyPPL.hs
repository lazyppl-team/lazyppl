{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module LazyPPL where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random hiding (uniform)
import qualified System.Random as R
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
      do  ~(Tree r (t:ts)) <- get
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

flatten :: PTree -> [[Int]]
flatten pt = go1 0 [] pt
  where
    go1 :: Int -> [Int] -> PTree -> [[Int]]
    go1 n path (PTree (Just x) xs) = (n:path) : (go2 n (n:path) xs) 
    go1 n path (PTree Nothing xs) = go2 n (n:path) xs

    go2 :: Int -> [Int] -> [Maybe PTree] -> [[Int]]
    go2 n path [] = []
    go2 n path ((Just t):ts) = (go1 n path t) ++ go2 (n+1) path ts
    go2 n path (Nothing:ts) = go2 (n+1) path ts

{-
Example of `flatten` working:

pt = PTree Nothing
           [ Just (PTree (Just 0.346)
                         [ Just (PTree Nothing
                                       [ Nothing
                                       , Just (PTree (Just 0.5)
                                                     []
                                              )
                                       ]
                                )
                         ]
                  )
           , Nothing
           , Just (PTree (Just 0.487)
                         []
                  )
           ]

> flatten pt
[[0,0],[1,0,0,0],[2,0]]

The paths are in reverse (e.g. 0.487 is found by taking the 0th node, and then going to
its 2nd child).
-}

type Site = [Int]
type Subst = M.Map Site Double

-- you need to provide the Site in reverse order
mutateNode :: Tree -> Site -> Double -> Tree
mutateNode t [] d = t
mutateNode (Tree x ts) [0] d = Tree d ts
mutateNode (Tree x ts) ms d = Tree x ((take n ts) ++ [mutateNode (ts!!n) ns d] ++ (drop (n+1) ts))
  where n:ns = reverse ms

mutateNodes :: Tree -> Subst -> Tree
mutateNodes tree sub = M.foldrWithKey (\k d t -> mutateNode t k d) tree sub

randomElement :: RandomGen g => g -> [a] -> (g, a)
randomElement g xs = if (length xs == 0) then error "0d sample space" else (g', xs !! n)
  where (n, g') = randomR (0, length xs - 1) g
               -- ^^^^^^^ In the old version of the `random` package that
               -- we are using, the function is called randomR. In newer
               -- versions it is called uniformR.

mh1 :: forall a. Int -> Meas a -> IO [(a, Product (Log Double))]
mh1 n (Meas m) = do
    newStdGen
    g <- getStdGen
    let (gTree,g') = split g
    let tree = randomTree gTree
    let (x0, w0) = runProb (runWriterT m) tree
    w0 `seq` return ()
    p <- trunc tree
    samples <- map (\(_,_,_,_,s) -> s) <$> iterateNM n step (gTree, g', M.empty, p, (x0, w0))
    return samples
  where step :: RandomGen g => (g, g, Subst, PTree, (a, Product (Log Double))) ->
                            IO (g, g, Subst, PTree, (a, Product (Log Double)))
        step (treeSeed, seed, sub, ptree, (x,w)) =
            do let (seed1, seed2) = split seed
               let sites = flatten ptree
               let (seed1', randSite) = randomElement seed1 sites
               let (newNode :: Double, seed1'') = random seed1'
               let (u :: Double, _) = random seed1'' -- the 'u' from Luke's notes
               let sub' = M.insert randSite newNode sub
               let t' = mutateNodes (randomTree treeSeed) sub'               
               let (x',w') = runProb (runWriterT m) t'
               w' `seq` return ()
               ptree' <- trunc t'
               let sites' = flatten ptree'
               let alpha = (fromIntegral (length sites) / fromIntegral (length sites'))
                         * (exp $ ln $ ((getProduct w') / (getProduct w)))
               if u <= alpha
               then return (treeSeed, seed2, sub', ptree', (x',w'))
               else return (treeSeed, seed2, sub,  ptree,  (x,w))

protoMh1 :: (Show a) => Prob a -> IO ()
protoMh1 p = do newStdGen
                g <- getStdGen
                let (g1,g2) = split g
                let t = randomTree g1
                let x = runProb p t
                x `seq` return () -- evaluates x to whnf 
                putStrLn "Initial random tree:"
                trunc t >>= \s -> putStrLn $ show s
                putStrLn "Initial value:"
                putStrLn $ show x
                putStrLn "-----"
                iterateNM 10 step (g1, g2, M.empty)
                return ()
  where step :: RandomGen g => (g, g, Subst) -> IO (g, g, Subst)
        step (treeSeed, seed, sub) =
            do let (seed1, seed2) = split seed
               -- `t` is the original tree, along with whatever mutations that
               -- have been made so far.
               let t = mutateNodes (randomTree treeSeed) sub
               let x = runProb p t
               x `seq` return ()
               ptree <- trunc t
               -- We select a random site as well as its new value.
               let (seed1', randSite) = randomElement seed1 (flatten ptree)
               let (newNode :: Double, seed1'') = random seed1'
               putStrLn "Start tree:"
               trunc t >>= \s -> putStrLn $ show s
               -- `sub'` is the updated list of mutations/substitutions.
               let sub' = M.insert randSite newNode sub
               -- `t'` is the new tree under these substitutions.
               let t' = mutateNodes (randomTree treeSeed) sub'
               (runProb p t') `seq` return () 
               -- Debug information to show all the sites we consider.
               putStrLn $  "All sites: " ++ show (map reverse $ flatten ptree)
               putStrLn $  "Picking site: " ++ show (reverse randSite)
               putStrLn "Modified tree:"
               trunc t' >>= \s -> putStrLn $ show s
               putStrLn "\n*****\n"
               -- For the next iteration pass the same original seed, which uniquely
               -- determines the original tree, along with a new seed for doing the
               -- random calculations, and the updated key-value store of the mutations
               -- we make to the tree.
               return (treeSeed, seed2, sub')

-- Swaraj: Apologies for all the comments below. It is scrap work + previous iterations
-- of my single-site MH stuff. Keeping it here for now just to be able to go back to it
-- when needed; will properly document/archive later. This is the private dev branch after all...

  --                        ptree <- trunc t
  --                        let (g1', randPath) = randomElement g1 (flatten ptree)
  --                        let (newNode :: Double, g1'') = random g1'
  --                        putStrLn "Start tree:"
  --                        trunc t >>= \s -> putStrLn $ show s
  --                        -- performGC
  --                        let t' = mutateNode t randPath newNode
  --                        -- performGC
  --                        (runProb p t') `seq` return () 
  --                        putStrLn $  "All paths: " ++ show (map reverse $ flatten ptree)
  --                        putStrLn $  "Picking path: " ++ show (reverse randPath)
  --                        putStrLn "Modified tree:"
  --                        trunc t' >>= \s -> putStrLn $ show s
  --                        putStrLn "\n*****\n"
  --                        return (g2, t')

-- test :: IO ( )
-- test = do newStdGen
--           g <- getStdGen
--           let t = randomTree g
--           iterateNM 4 step (g,3,-1)
--           iterateNM 4 step (g,0,1)
--           return ()
--   where step :: RandomGen g => (g,Int,Int) -> IO (g,Int,Int)
--         step (g,n,inc) = do let t' = randomTree g
--                             let x' = runProb (four n) t'
--                             x' `seq` return ()
--                             trunc t' >>= \s -> putStrLn $ show s
--                             return $ (g, n + inc, inc)

-- four n = do x <- uniform
--             y <- uniform
--             z <- uniform
--             w <- uniform
--             return $ [x,y,z,w] !! n

-- mh1 :: (Show a) => Prob a -> IO ()
-- mh1 p = do newStdGen
--            g <- getStdGen
--            let (g1,g2) = split g
--            let t = randomTree g1
--            let x = runProb p t
--            x `seq` return () -- evaluates x to whnf 
--            putStrLn "Initial random tree:"
--            trunc t >>= \s -> putStrLn $ show s
--            putStrLn "Initial value:"
--            putStrLn $ show x
--            putStrLn "-----"
--            iterateNM 10 step (g2, t)
--            return ()
--   where step :: RandomGen g => (g, Tree) -> IO (g, Tree)
--         step (g, t) = do let (g1,g2) = split g
--                          -- (runProb p t) `seq` return () 
--                          ptree <- trunc t
--                          let (g1', randPath) = randomElement g1 (flatten ptree)
--                          let (newNode :: Double, g1'') = random g1'
--                          putStrLn "Start tree:"
--                          trunc t >>= \s -> putStrLn $ show s
--                          -- performGC
--                          let t' = mutateNode t randPath newNode
--                          -- performGC
--                          (runProb p t') `seq` return () 
--                          putStrLn $  "All paths: " ++ show (map reverse $ flatten ptree)
--                          putStrLn $  "Picking path: " ++ show (reverse randPath)
--                          putStrLn "Modified tree:"
--                          trunc t' >>= \s -> putStrLn $ show s
--                          putStrLn "\n*****\n"
--                          return (g2, t')

-- mh1 :: forall a. (Show a) => ProbCtx a -> IO ()
-- mh1 pc = do
--     newStdGen
--     g <- getStdGen
--     let (g1,g2) = split g
--     let t = randomTree g1
--     let x = runProbCtx pc (M.fromList [], t)
--     putStrLn $ "Initial sample: " ++ show x -- we need this line to force evaluation of the line above
--     trunc t >>= \s -> putStrLn $ "Initial tree: " ++ show s
--     putStrLn "==="
--     iterateNM 10 step (x,t,M.fromList [],g2)
--     return ()
--     where step :: RandomGen g => (a, Tree, M.Map Double Double, g) -> IO (a, Tree, M.Map Double Double, g)
--           step (a,t,ctx,r) = do ptree <- trunc t
--                                 let plist = flatten ptree
--                                 let (r1,r2) = split r
--                                 let (i :: Double, r') = random r1 -- a random node from the evaluated nodes
--                                 let (v' :: Double, _) = random r2 -- the newly generated random node
--                                 let v = plist !! (floor $ i * (fromIntegral $ length plist))
--                                 let ctx' = M.alter (\_ -> Just v') v ctx
--                                 let a' = runProbCtx pc (ctx', t)
--                                 putStrLn $ show $ subst ctx ptree
--                                 putStrLn $ "Context: " ++ show ctx
--                                 putStrLn $ "Replacee: " ++ show (M.findWithDefault v v ctx)
--                                 putStrLn $ "Replacer: " ++ show v'
--                                 putStrLn $ "Sample: " ++ show a
--                                 putStrLn "---"
--                                 return (a',t,ctx',r2)

-- test :: IO ()
-- test = do
--   newStdGen
--   g <- getStdGen
--   let t = randomTree g
--   let x = runProb exampleProb t
--   putStrLn $ show x 
--   trunc t >>= \s -> putStrLn $ show s
--   putStrLn "==="
--   let g = mutateNode t [0,0] 0.9
--   let x = runProb exampleProb g
--   putStrLn $ show x
--   trunc g >>= \s -> putStrLn $ show s
--   return ()

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
exampleProb :: Prob Double
exampleProb = do 
             choice <- uniform
             w <- uniform
             x <- uniform
             y <- uniform
             z <- uniform
             case floor (choice * 4.0) of
               0 -> return w 
               1 -> return x  
               2 -> return y  
               3 -> return z  

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

