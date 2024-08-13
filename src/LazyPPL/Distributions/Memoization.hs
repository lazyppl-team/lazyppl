{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}

{- | Stochastic memoization for the `LazyPPL` library.
Stochastic memoization is a useful primitive for programming non-parametric models. 
It has type @(a -> Prob b) -> Prob (a -> b)@, and can be thought of as simultaneously sampling results for all possible arguments, in a lazy way. 

When @a@ is enumerable, this amounts to converting a stream of probabilities to a random stream, which we can do by sampling each probability once. 

This module provides:

* A general type-class `MonadMemo` for monads @m@ that support memoization at certain argument types @a@. 

* A default trie-based implementation when @a@ is enumerable, and a curry-based implementation when @a@ is a pair type. 

* A general implementation `generalmemoize` for probability, using memo-tables.

* A memoized recursion combinator, `memrec`. 


For illustrations, see the [graph example](https://lazyppl-team.github.io/GraphDemo.html), [clustering](https://lazyppl-team.github.io/ClusteringDemo.html), [additive clustering](https://lazyppl-team.github.io/AdditiveClusteringDemo.html), or the [infinite relational model](https://lazyppl-team.github.io/IrmDemo.html). 
-}

module LazyPPL.Distributions.Memoization (MonadMemo, memoize, generalmemoize, memrec) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, get, put, runState, state)
import Data.IORef
import Data.List
import Data.Map (empty, insert, keys, lookup, size)
import Debug.Trace
import LazyPPL 
import System.IO.Unsafe

{-| Type class for memoizable argument types @a@ under a monad @m@ -}
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

{-| Implementation for enumerable types using tries -}
instance (Monad m) => MonadMemo m Int

{-| Implementation for pair types using currying -}
instance (Monad m, MonadMemo m a, MonadMemo m b) => MonadMemo m (a, b) where
  memoize f = fmap uncurry $ memoize $ \x -> memoize $ \y -> f (x, y)

{-| A general memoization method when @m@ is a probability monad.

    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If @a@ is finite, we could just sample all values of @a@ in advance
    and avoid unsafePerformIO. If @a@ is enumerable, we can use the trie method. 
-}
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
