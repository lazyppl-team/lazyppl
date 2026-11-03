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

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map (empty, insert, lookup, size)
import Data.Tuple (Solo(MkSolo))
import LazyPPL
import System.IO.Unsafe

{-| Type class for memoizable argument types @a@ under a monad @m@.

The default 'memoize', for an enumerable @a@, samples an infinite trie of results inside @m@, so it returns only in a lazy monad such as 'Prob'. A strict monad such as 'Maybe' or 'IO' would never return the memo function, so give no instance for one. -}
class (Monad m) => MonadMemo m a where
  memoize :: (a -> m b) -> m (a -> b)
  default memoize :: (Enum a) => (a -> m b) -> m (a -> b)
  memoize f = 
    do
      t <- ini 0 (f . toEnum)
      return $ \x -> look t (fromEnum x)

{-- Basic trie based integer-indexed memo table.
    Negative indices are rejected. --}
data BinTree a = Branch a (BinTree a) (BinTree a)

ini :: (Monad m) => Int -> (Int -> m a) -> m (BinTree a)
ini n f = do x <- f n; l <- ini (2 * n + 1) f; r <- ini (2 * n + 2) f; return $ Branch x l r

look :: BinTree a -> Int -> a
look tree n
  | n < 0 = error "memoize: negative indices are not supported"
  | otherwise = follow tree (reverse (path (toInteger n + 1)))
  where
    path :: Integer -> [Bool]
    path 1 = []
    path k = odd k : path (k `div` 2)
    follow (Branch x _ _) [] = x
    follow (Branch _ l r) (goRight : rest) = follow (if goRight then r else l) rest

{-| Implementation for enumerable types using tries, in the lazy 'Prob' monad. Indices must be non-negative. -}
instance MonadMemo Prob Int

{-| Implementation for pair types using currying -}
instance (Monad m, MonadMemo m a, MonadMemo m b) => MonadMemo m (a, b) where
  memoize f = fmap uncurry $ memoize $ \x -> memoize $ \y -> f (x, y)

{-| A general memoization method when @m@ is a probability monad.

    We use unsafePerformIO to maintain
    a table of calls that have already been made.
    If @a@ is finite, we could just sample all values of @a@ in advance
    and avoid unsafePerformIO. If @a@ is enumerable, we can use the trie method. 

    The returned function holds hidden state: see 'LazyPPL.runProb' for its use outside a model.
-}
generalmemoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
generalmemoize f = memrec (const f)

{-| Stochastic memoization for recursive functions.
    Applying 'memoize' to a recursively defined function only memoizes at the
    top-level: recursive calls are calls to the non-memoized function.
    'memrec' is an alternative implementation which resolves recursion and
    memoization at the same time, so that recursive calls are also memoized.

    The returned function holds hidden state: see 'LazyPPL.runProb' for its use outside a model.
-}
memrec :: Ord a => ((a -> b) -> (a -> Prob b)) -> Prob (a -> b)
memrec f =
  Prob $ \(Tree _ gs) ->
    unsafePerformIO $ do
      ref <- newIORef Data.Map.empty
      -- One atomic step looks up the key and, for a new key, stores its result
      -- with the next unused subtree, so two threads never share a subtree or
      -- lose an entry. The result stays lazy (MkSolo keeps atomicModifyIORef'
      -- from forcing it), so a recursive call evaluates after the step.
      let memoized_fixpoint = \x -> unsafePerformIO $ do
            MkSolo y <- atomicModifyIORef' ref $ \table ->
              case Data.Map.lookup x table of
                Just y -> (table, MkSolo y)
                Nothing ->
                  let y = runProb (f memoized_fixpoint x) (gs !! (1 + size table))
                  in (Data.Map.insert x y table, MkSolo y)
            return y
      return memoized_fixpoint
