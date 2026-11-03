{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables, CPP
 #-}
module LazyPPL.SingleSite.Internal where

import LazyPPL
import LazyPPL.Internal.Sampler (randomLabel, checkedLogWeight)
import Control.Monad.Trans.Writer

import Data.Monoid
import Numeric.Log
import qualified Data.Map as M

import System.Random hiding (uniform)

import Control.DeepSeq
import Control.Exception (evaluate)

import System.IO.Unsafe
#ifdef LAZYPPL_BROWSER
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek, sizeOf)
#else
import Unsafe.Coerce
#endif


import Data.IORef
import GHC.Exts (Ptr(..), unpackClosure#, indexArray#, sizeofArray#, Int(I#))
import GHC.Exts.Heap.Utils (dataConNames)
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
    g <- newStdGen
    let (gTree,g') = split g
        tree = randomTree gTree
        (x0, w0) = checkedSample (runProb (runWriterT m) tree)
        p = unsafePerformIO $ do { evaluate (rnf x0); evaluate (rnf w0) ; trunc tree }
        samples = map (\(_,_,_,_,s) -> s) $ iterate (mhStep (Meas m)) (gTree, g', M.empty, p, (x0, w0))
    return samples

-- | One transition of 'mh1'. The state holds the seed of the initial tree, the
-- proposal seed, the accepted substitutions, the evaluated sites, and the
-- current sample with its weight.
mhStep :: forall a g. (NFData a, RandomGen g) => Meas a -> (g, g, Subst, PTree, (a, Product (Log Double))) -> (g, g, Subst, PTree, (a, Product (Log Double)))
mhStep (Meas m) (treeSeed, seed, sub, ptree, (x,w)) =
            let (seed1, seed2) = split seed
                sites = getSites ptree
            -- A measure that reads no random site has a one-point sample space:
            -- there is no site to mutate, so the chain keeps its sample.
            in if null sites
              then (treeSeed, seed2, sub,  ptree,  (x,w))
              else
                let (seed1', randSite) = (\(x,y) -> (x, reverse y)) $ randomElement seed1 sites
                    (newNode :: Double, seed1'') = randomLabel seed1'
                    (u :: Double, _) = randomLabel seed1''
                    sub' = M.insert randSite newNode sub
                    t' = mutateNodes (randomTree treeSeed) sub'
                    (x',w') = checkedSample (runProb (runWriterT m) t')
                    ptree' = unsafePerformIO $ do { evaluate (rnf x'); evaluate (rnf w'); trunc t' }
                    sites' = getSites ptree'
                    alpha = fromIntegral (length sites) / fromIntegral (length sites')
                          * exp (ln (getProduct w' / getProduct w))
                -- The reverse proposal cannot select a site absent from the proposed tree.
                -- A move is accepted with probability min 1 alpha, so u < alpha; with
                -- u <= alpha, u = 0 would accept a move to weight 0. u is a label, in
                -- (0,1): random's 1 would reject a move with alpha = 1.
                in if reverse randSite `elem` sites' && (getProduct w == 0 || u < alpha)
                  then (treeSeed, seed2, sub', ptree', (x',w'))
                  else (treeSeed, seed2, sub,  ptree,  (x,w))

-- A product of valid weights can still overflow to a logarithm of +Infinity.
checkedSample :: (a, Product (Log Double)) -> (a, Product (Log Double))
checkedSample (v, Product l) = (v, Product (checkedLogWeight "mh1" l))

-- Functions for truncating a tree.
getGCClosureData inspect b = do
  c <- inspect b
  case c of
    BlackholeClosure _ n -> getGCClosureData inspect n
    _ -> return c

helperT :: (Box -> IO Closure) -> Box -> IO PTree
helperT inspect b = do
  c <- getGCClosureData inspect b
  case c of
    ConstrClosure _ [n,l] [] _ _ "Tree" ->
      do  n' <- getGCClosureData inspect n
          l' <- getGCClosureData inspect l
          let missing :: IO x
              missing = return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
          -- The label is a site once it is evaluated, and the children were
          -- demanded once the list is evaluated; each field is read on its own,
          -- so a node with both fields unevaluated has no site.
          label <- case n' of
#ifdef LAZYPPL_BROWSER
            ConstrClosure {dataArgs = ds, name = "D#"} -> Just <$> doubleFromWords ds
#else
            ConstrClosure {dataArgs = [d], name = "D#"} -> return (Just (unsafeCoerce d))
#endif
            ThunkClosure {} -> return Nothing
            SelectorClosure {} -> return Nothing
            _ -> missing
          children <- case l' of
            ConstrClosure {name = ":"} -> helperB inspect l
            ThunkClosure {} -> return []
            _ -> missing
          return $ PTree label children
    ThunkClosure {} -> return $ PTree Nothing []

#ifdef LAZYPPL_BROWSER
-- A Double occupies one 64-bit word or two 32-bit words, in host byte order.
doubleFromWords :: [Word] -> IO Double
doubleFromWords ws
  | length ws * sizeOf (0 :: Word) /= sizeOf (0 :: Double) =
      fail "Unexpected Double heap payload size"
  | otherwise = alloca $ \ptr -> do
      pokeArray (castPtr ptr) ws
      peek ptr
#endif

helperB :: (Box -> IO Closure) -> Box -> IO [Maybe PTree]
helperB inspect b = do
  c <- getGCClosureData inspect b
  case c of
    ConstrClosure _ [n,l] [] _ _ ":" ->
      do  n' <- getGCClosureData inspect n
          l' <- getGCClosureData inspect l
          case (n',l') of
            (ConstrClosure {name = "Tree"}, ConstrClosure {name = ":"}) ->
              do  n'' <- helperT inspect n
                  l'' <- helperB inspect l
                  return $ Just n'' : l''
            (ConstrClosure {name = "Tree"}, ThunkClosure {}) ->
              do  n'' <- helperT inspect n
                  return [Just n'']
            (ThunkClosure {}, ConstrClosure {name = ":"}) ->
              do  l'' <- helperB inspect l
                  return $ Nothing : l''
            (ThunkClosure {}, ThunkClosure {}) ->
              return [] -- alternatively, Nothing : []
            _ -> return $ error $ "Missing case:\n" ++ show n' ++ "\n" ++ show l'
    ThunkClosure {} -> return []

trunc :: Tree -> IO PTree
trunc t = do
  cache <- newIORef M.empty
  helperT (getCachedClosureData cache) (asBox t)


-- Constructor descriptions are immutable during this traversal.
getCachedClosureData :: IORef (M.Map (Ptr StgInfoTable) (String, String, String)) -> Box -> IO Closure
getCachedClosureData cache (Box x) =
  case unpackClosure# x of
    (# infoAddress, heapRep, pointers #) -> do
      let infoPointer = Ptr infoAddress
          boxes = [case indexArray# pointers i of { (# p #) -> Box p }
                  | I# i <- [0 .. I# (sizeofArray# pointers) - 1]]
          names = do
            known <- readIORef cache
            case M.lookup infoPointer known of
              Just result -> return result
              Nothing -> do
                result <- dataConNames infoPointer
                writeIORef cache (M.insert infoPointer result known)
                return result
      table <- peekItbl infoPointer
      case tipe table of
        TSO -> return (UnsupportedClosure table)
        STACK -> return (UnsupportedClosure table)
        _ -> getClosureDataFromHeapRepPrim names (const (return Nothing)) table heapRep boxes

