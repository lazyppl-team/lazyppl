{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

module Distr.Memoization (MonadMemo, memoize, generalmemoize, memrec) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, get, put, runState, state)
import Data.IORef
import Data.List
import Data.Map (empty, insert, keys, lookup, size)
import Debug.Trace
import LazyPPL
import System.IO.Unsafe

class (Monad m) => MonadMemo m a where
  memoize :: (a -> m b) -> m (a -> b)
  default memoize :: (Enum a) => (a -> m b) -> m (a -> b)
  memoize f = 
    do
      t <- ini 0 (f . toEnum)
      return $ \x -> look t (fromEnum x)

{-- Basic trie based integer-indexed memo table.
    NB Currently ignores negative integers --}
data BinTree a = Branch a (BinTree a) (BinTree a)

ini :: (Monad m) => Int -> (Int -> m a) -> m (BinTree a)
ini n f = do x <- f n; l <- ini (2 * n + 1) f; r <- ini (2 * n + 2) f; return $ Branch x l r

look :: BinTree a -> Int -> a
look (Branch x l r) 0 = x
look (Branch _ l r) n = if even n then look r (n `div` 2) else look l (n `div` 2)

instance (Monad m) => MonadMemo m Int

instance (Monad m, MonadMemo m a, MonadMemo m b) => MonadMemo m (a, b) where
  memoize f = fmap uncurry $ memoize $ \x -> memoize $ \y -> f (x, y)

{-- Stochastic memoization.
    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If a is finite, we could just sample all values of a in advance
    and avoid unsafePerformIO.
    If it is countably infinite, there probably also are implementation tricks.
--}
generalmemoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
generalmemoize f = Prob $ \(Tree _ gs) ->
  unsafePerformIO $ do
    ref <- newIORef Data.Map.empty
    return $ \x -> unsafePerformIO $ do
      m <- liftM (Data.Map.lookup x) (readIORef ref)
      case m of
        Just y -> return y
        Nothing -> do
          n <- readIORef ref
          let y = runProb (f x) (gs !! (1 + size n))
          modifyIORef' ref (Data.Map.insert x y)
          return y

{-- Stochastic memoization for recursive functions.
    Applying 'memoize' to a recursively defined function only memoizes at the
    top-level: recursive calls are calls to the non-memoized function.
    'memrec' is an alternative implementation which resolves recursion and
    memoization at the same time, so that recursive calls are also memoized.
--}
memrec :: Ord a => Show a => ((a -> b) -> (a -> Prob b)) -> Prob (a -> b)
memrec f =
  Prob $ \(Tree _ gs) ->
    unsafePerformIO $ do
      ref <- newIORef Data.Map.empty
      let memoized_fixpoint = \x -> unsafePerformIO $ do
            m <- liftM (Data.Map.lookup x) (readIORef ref)
            case m of
              Just y -> return y
              Nothing -> do
                n <- readIORef ref
                let fix = f memoized_fixpoint
                let y = runProb (fix x) (gs !! (1 + size n))
                modifyIORef' ref (Data.Map.insert x y)
                return y
      return memoized_fixpoint
