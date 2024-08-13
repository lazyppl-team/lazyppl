{-# LANGUAGE ScopedTypeVariables
 #-}
module LazyPPL.SingleSite (mh1) where

import LazyPPL
import Control.Monad.Trans.Writer

import Data.Monoid
import Numeric.Log
import qualified Data.Map as M

import System.Random hiding (uniform)

import Control.DeepSeq
import Control.Exception (evaluate)

import System.IO.Unsafe
import Unsafe.Coerce

import GHC.Exts.Heap


{-- | Single-site Metropolis-Hastings --}

-- | A partial tree, used for examining finite evaluated subtrees of the working infinite
-- | random tree.
data PTree = PTree (Maybe Double) [Maybe PTree] deriving Show

type Site = [Int]
type Subst = M.Map Site Double

getSites :: PTree -> [Site]
getSites p = getSites' p []

getSites' :: PTree -> Site  -> [Site]
getSites' (PTree (Just v) ts) site = site : concat [ getSites' t (n:site)  | (n, Just t) <- zip [0..] ts ]
getSites' (PTree Nothing  ts) site = concat [ getSites' t (n:site) | (n, Just t) <- zip [0..] ts ]

mutateNode :: Tree -> Site -> Double -> Tree
mutateNode (Tree _ ts) []     d = Tree d ts
mutateNode (Tree v ts) (n:ns) d = Tree v $ take n ts ++ mutateNode (ts!!n) ns d : drop (n+1) ts

mutateNodes :: Tree -> Subst -> Tree
mutateNodes = M.foldrWithKey (\k d t -> mutateNode t k d)

randomElement :: RandomGen g => g -> [a] -> (g, a)
randomElement g xs = if null xs then error "0d sample space" else (g', xs !! n)
  where (n, g') = uniformR (0, length xs - 1) g
               -- ^^^^^^^^ 
               -- Depending on the version of `random` this can be either uniformR or randomR.

mh1 :: forall a. NFData a => Meas a -> IO [(a, Product (Log Double))]
mh1 (Meas m) = do
    newStdGen
    g <- getStdGen
    let (gTree,g') = split g
        tree = randomTree gTree
        (x0, w0) = runProb (runWriterT m) tree
        p = unsafePerformIO $ do { evaluate (rnf x0); evaluate (rnf w0) ; trunc tree }
        samples = map (\(_,_,_,_,s) -> s) $ iterate step (gTree, g', M.empty, p, (x0, w0))
    return samples
  where step :: RandomGen g => (g, g, Subst, PTree, (a, Product (Log Double))) -> (g, g, Subst, PTree, (a, Product (Log Double)))
        step (treeSeed, seed, sub, ptree, (x,w)) =
            let (seed1, seed2) = split seed
                sites = getSites ptree
                (seed1', randSite) = (\(x,y) -> (x, reverse y)) $ randomElement seed1 sites
                (newNode :: Double, seed1'') = random seed1'
                (u :: Double, _) = random seed1'' 
                sub' = M.insert randSite newNode sub
                t' = mutateNodes (randomTree treeSeed) sub'
                (x',w') = runProb (runWriterT m) t'
                ptree' = unsafePerformIO $ do { evaluate (rnf x'); evaluate (rnf w'); trunc t' }
                sites' = getSites ptree'
                alpha = fromIntegral (length sites) / fromIntegral (length sites')
                      * exp (ln (getProduct w' / getProduct w))
            in if u <= alpha
              then (treeSeed, seed2, sub', ptree', (x',w'))
              else (treeSeed, seed2, sub,  ptree,  (x,w))

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
      do  n' <- getGCClosureData n
          l' <- getGCClosureData l
          -- performGC
          case (n',l') of
            (ConstrClosure {dataArgs = [d], name = "D#"}, ConstrClosure {name = ":"}) ->
              do  l'' <- helperB l
                  return $ PTree (Just $ unsafeCoerce d) l''
            (ThunkClosure {}, ConstrClosure {name = ":"}) ->
              do  l'' <- helperB l
                  return $ PTree Nothing l''
            (ConstrClosure {dataArgs = [d], name = "D#"}, ThunkClosure {}) ->
                  return $ PTree (Just $ unsafeCoerce d) []
            (SelectorClosure {}, ThunkClosure {}) ->
              return $ PTree Nothing []
            (SelectorClosure {}, ConstrClosure {name = ":"}) ->
              do  l'' <- helperB l
                  return $ PTree Nothing l''
            _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
    ThunkClosure {} -> return $ PTree Nothing []

helperB :: Box -> IO [Maybe PTree]
helperB b = do
  c <- getGCClosureData b
  case c of
    ConstrClosure _ [n,l] [] _ _ ":" ->
      do  n' <- getGCClosureData n
          l' <- getGCClosureData l
          case (n',l') of
            (ConstrClosure {name = "Tree"}, ConstrClosure {name = ":"}) ->
              do  n'' <- helperT n
                  l'' <- helperB l
                  return $ Just n'' : l''
            (ConstrClosure {name = "Tree"}, ThunkClosure {}) ->
              do  n'' <- helperT n
                  return [Just n'']
            (ThunkClosure {}, ConstrClosure {name = ":"}) ->
              do  l'' <- helperB l
                  return $ Nothing : l''
            (ThunkClosure {}, ThunkClosure {}) ->
              return [] -- alternatively, Nothing : []
            _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
    ThunkClosure {} -> return []

trunc :: Tree -> IO PTree
trunc t = helperT $ asBox t

