{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

{- | LazyPPL is a library for Bayesian probabilistic programming. It supports lazy use of probability, and we provide new Metropolis-Hastings simulation algorithms to allow this. Laziness appears to be a good paradigm for non-parametric statistics. 

Reference paper: [Affine Monads and Lazy Structures for Bayesian Programming](https://arxiv.org/abs/2212.07250). POPL 2023.

Illustrations: [https://lazyppl-team.github.io](https://lazyppl-team.github.io).


LazyPPL is inspired by recent ideas in synthetic probability theory and synthetic measure theory, such as [quasi-Borel spaces](https://ncatlab.org/nlab/show/quasi-Borel+space) and [Markov categories](https://ncatlab.org/nlab/show/Markov+category). LazyPPL is inspired by many other languages, including [Church](http://v1.probmods.org), [Anglican](https://probprog.github.io/anglican/), and [Monad-Bayes](https://hackage.haskell.org/package/monad-bayes). Monad-Bayes now includes a LazyPPL-inspired simulation algorithm.

This module defines

    1. Two monads: `Prob` (for probability measures) and `Meas` (for unnormalized measures), with interface `uniform`, `sample`, `score`. 

    2. Monte Carlo inference methods produce samples from an unnormalized measure. We provide three inference methods: 

        a. 'mh' (Metropolis-Hastings algorithm based on lazily mutating parts of the tree at random).

        b. 'mhirreducible', which randomly restarts for a properly irreducible Metropolis-Hastings kernel.

        c. 'wis' (simple reference weighted importance sampling)

        See also "LazyPPL.SingleSite" in @lazyppl-singlesite@ for a separate single-site Metropolis-Hastings algorithm via "GHC.Exts.Heap" and "System.IO.Unsafe".

    3. Various useful helpful functions.

    A typical usage would be

@   
    import LazyPPL (Prob, Meas, uniform, sample, score, mh, every)
@

    Most of the structure here will not be needed in typical models. We expose more of the structure for more experimental uses. 

The "LazyPPL.Distributions" module provides many useful distributions, and further non-parametric distributions are in "LazyPPL.Distributions.DirichletP", "LazyPPL.Distributions.GP", "LazyPPL.Distributions.IBP", and "LazyPPL.Distributions.Memoization". 


-}

module LazyPPL
    ( -- * Rose tree type
      --
      -- | Our source of randomness will be an infinitely wide and deep lazy [rose tree](https://en.wikipedia.org/wiki/Rose_tree), regarded as initialized with uniform choices from the open interval (0,1) for each label.
      Tree(Tree),
      -- * Monads
      Prob(Prob), Meas(Meas),
      -- * Basic interface
      --
      -- | There are three building blocks for measures: `uniform` for probability measures; `sample` and `score` for unnormalized measures. Combined with the monad structure, these give all s-finite measures.
      uniform, sample, score, scoreLog, scoreProductLog,
      -- * Standard generator interface
      --
      -- | `ProbGen` is the stock `StatefulGen` view of `Prob`, for the bounded samplers of `random`.
      ProbGen(ProbGen),
      -- * Monte Carlo simulation
      --
      -- | The `Meas` type describes unnormalized measures. Monte Carlo simulation allows us to sample from an unnormalized measure. Our main Monte Carlo simulator is `mh`. 
      mh, mhirreducible, weightedsamples, wis, 
      -- * Useful functions
      every, randomTree, runProb) where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad.ST (runST)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Monoid
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Random hiding (uniform)
import System.Random.Stateful (StatefulGen(..), fillByteArrayST)
import qualified System.Random.MWC.Distributions as MWC
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import LazyPPL.Internal.History (History, newHistory, appendMutation, labelAt)
import LazyPPL.Internal.Sampler (randomLabel, checkedLogWeight, labelIndex)
import Numeric.Log


{- | A `Tree` here is a lazy, infinitely wide and infinitely deep rose tree, labelled by Doubles.
-}
data Tree = Tree Double [Tree]
-- Often people would just use a list or stream instead of a tree.
-- But a tree allows us to be lazy about how far we are going all the time.

{- | A probability distribution over a is 
a function @ Tree -> a @.

We can think of this as the law of a random variable, indexed by the source of randomness, which is `Tree`. 

According to the monad implementation, a program uses up bits of the tree as it runs. The tree being infinitely wide and deep allows for lazy computation.-}
newtype Prob a = Prob (Tree -> a)

-- | Split tree splits a tree in two (bijectively)
splitTree :: Tree -> (Tree , Tree)
splitTree (Tree r (t : ts)) = (t , Tree r ts)


-- | Sequencing is done by splitting the tree
-- and using different bits for different computations.
-- 
-- This monad structure is strongly inspired by the probability monad of [quasi-Borel space](https://ncatlab.org/nlab/show/quasi-Borel+space#probability_distributions). 
instance Monad Prob where
  return a = Prob $ const a
  (Prob m) >>= f = Prob $ \g ->
    let (g1, g2) = splitTree g
        (Prob m') = f (m g1)
    in m' g2
instance Functor Prob where fmap = liftM
instance Applicative Prob where {pure = return ; (<*>) = ap}

{- | An unnormalized measure is represented by a probability distribution over pairs of a weight and a result. -}
newtype Meas a = Meas (WriterT (Product (Log Double)) Prob a)
  deriving(Functor, Applicative, Monad)

{- | A stateful generator view of a probability computation, for the stock
sampling algorithms of @random@ that take a 'StatefulGen', such as
'uniformRM'.

Precondition: the tree must provide a child for every requested word; the
library's trees always do (a hand-built tree that runs out of children raises
\"LazyPPL: random tree exhausted\").

'randomLabel' puts every label on the midpoint grid (k + 1/2) / 2^52 with k a
uniform 52-bit integer, and 'labelIndex' recovers k exactly, so each label
supplies 52 uniform bits. 'uniformWord64' joins the label at its tree node
with the label of the node's first child — a subtree that 'Prob' sequencing
reserves to this primitive — into 64 uniform bits; the smaller word methods
use one label. Every request a stock algorithm makes, including the repeated
draws of its rejection loops, reads a distinct tree coordinate under the
monad structure of 'Prob', and running the same computation on the same tree
gives the same words. -}
data ProbGen = ProbGen

instance StatefulGen ProbGen Prob where
  uniformWord64 ProbGen = Prob $ \(Tree r ts) -> case ts of
    (Tree c _ : _) -> (labelIndex r `shiftL` 12) .|. (labelIndex c `shiftR` 40)
    [] -> treeExhausted
  {-# INLINE uniformWord64 #-}
  uniformWord32 ProbGen = Prob $ \(Tree r _) -> fromIntegral (labelIndex r `shiftR` 20)
  {-# INLINE uniformWord32 #-}
  -- The class default delegates to a frozen generator (System.Random.Internal
  -- 1.3.1, uniformByteArrayM's default signature), which a tree coordinate has
  -- no notion of. The stock fillByteArrayST (System.Random.Stateful, under
  -- "Helper functions for creating instances") fills the array little-endian
  -- from successive words, drawn here from successive children through an
  -- STRef; the empty list is matched explicitly, so no byte stays
  -- uninitialised.
  uniformByteArrayM isPinned n ProbGen = Prob $ \(Tree _ gs) -> runST $ do
    ref <- newSTRef gs
    fillByteArrayST isPinned n $ do
      ts <- readSTRef ref
      case ts of
        (t : ts') -> do
          writeSTRef ref ts'
          pure (treeWord t)
        [] -> treeExhausted
  {-# INLINE uniformByteArrayM #-}

-- One clear error when a hand-built tree runs out of children.
treeExhausted :: a
treeExhausted = error "LazyPPL: random tree exhausted"
{-# NOINLINE treeExhausted #-}

-- One tree word per node, as uniformWord64 makes it.
treeWord :: Tree -> Word64
treeWord (Tree r ts) = case ts of
  (Tree c _ : _) -> (labelIndex r `shiftL` 12) .|. (labelIndex c `shiftR` 40)
  [] -> treeExhausted
{-# INLINE treeWord #-}

{- | A uniform sample from the open interval (0,1) is a building block for probability distributions. It is never 0 or 1, so an inverse transform such as @-log u@ stays finite.

This is implemented by getting the label at the head of the tree and discarding the rest.-}
uniform :: Prob Double
uniform = Prob $ \(Tree r _) -> r

-- | Regard a probability measure as an unnormalized measure.
sample :: Prob a -> Meas a
sample p = Meas $ lift p

{- | A one point measure with a given score (or weight, or mass, or likelihood), which should be a non-negative real number.

A score of 0 describes impossibility. Negative and non-finite scores raise an error.-}
score :: Double -> Meas ()
score r = scoreLog (Exp (log r))

{- | 'score' with the weight given by its natural logarithm, so the weight may exceed the range of 'Double'. A logarithm of negative infinity is weight 0; a NaN or positive-infinite logarithm raises an error. -}
scoreLog :: Log Double -> Meas ()
scoreLog r = scoreProductLog (Product r)

-- | 'scoreLog' with the weight wrapped in 'Product', the form in which 'Meas' accumulates weights.
scoreProductLog :: Product (Log Double) -> Meas ()
scoreProductLog (Product r) = checkedLogWeight "score" r `seq` Meas (tell (Product r))


{- | Generate a tree with labels drawn uniformly from the open interval (0,1).

    This uses 'split' to split a random seed. -}
randomTree :: RandomGen g => g -> Tree
randomTree g = let (a,g') = randomLabel g in Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree]
randomTrees g = let (g1,g2) = split g in randomTree g1 : randomTrees g2

{- | 'runProb' runs a probability deterministically, given a source of randomness.

Some results of 'runProb' hold hidden state: a counter of "LazyPPL.Distributions.Counter", a
restaurant of 'LazyPPL.Distributions.IBP.newRestaurant', a function of
'LazyPPL.Distributions.Memoization.generalmemoize' or 'LazyPPL.Distributions.Memoization.memrec',
and the random functions of 'LazyPPL.Distributions.GP.wiener' and 'LazyPPL.Distributions.GP.gp'.
Inside a model, bind such a value with @>>=@ and use it there. Outside a model, evaluate the value
itself once, with 'Control.Exception.evaluate', before an IO action that runs more than once (the
body of a loop or of a thread) uses it:

> f <- evaluate (runProb wiener tree)
> ys <- mapM (\x -> evaluate (f x)) xs

Bound with @let@ instead, or returned inside a function from a 'Prob' computation, such a value can
lose its state when an IO action that runs more than once uses it: GHC may copy the binding into
the action, so that every run of the action starts from new state, and a counter then returns the
same number to every read. Evaluating a pair or a list does not evaluate the values inside it, so
evaluate each value with hidden state. @cabal run lazyppl:test:hidden-state-test -- known-limit@
runs these unsupported uses. -}
runProb :: Prob a -> Tree -> a
runProb (Prob a) = a

{- | Runs an unnormalized measure and gets out a stream of (result,weight) pairs.

These are not samples from the renormalized distribution, just plain (result,weight) pairs. This is useful when the distribution is known to be normalized already. -}
weightedsamples :: forall a. Meas a -> IO [(a,Log Double)]
weightedsamples (Meas m) =
  do
    let helper :: Prob [(a, Product (Log Double))]
        helper = do
          (x, w) <- runWriterT m
          rest <- helper
          return $ (x, w) : rest
    g <- newStdGen
    let rs = randomTree g
    let xws = runProb helper rs
    return $ map (\(x,w) -> (x, checkedLogWeight "weightedsamples" (getProduct w))) xws

{- | Weighted importance sampling first draws n weighted samples,
    and then samples a stream of results from that, regarded as an empirical distribution. Sometimes called "likelihood weighted importance sampling". 

This is a reference implementation. It will not usually be very efficient at all, but may be useful for debugging. Raises an error if @n@ is less than 1 or all @n@ samples have weight 0.
 -}
wis :: Int -- ^ @n@, the number of samples to base on, at least 1
    -> Meas a -- ^ @m@, the measure to normalize
    -> IO [a] -- ^ Returns a stream of samples
wis n _ | n <= 0 = error "wis: the number of samples must be positive"
wis n m = do
  xws <- weightedsamples m
  let particles = V.fromList (take n xws)
      weights = VU.fromList (map (ln . snd) (V.toList particles))
  g <- newStdGen
  if VU.all (== (-1 / 0)) weights then error "wis: every sample has weight 0"
    else return $ map (\tree -> fst (particles V.! runProb (MWC.logCategorical weights ProbGen) tree)) (randomTrees g)

{- | Produce a stream of samples, using Metropolis Hastings simulation.

   The weights are also returned. Often the weights can be discarded, but sometimes we may search for a sample of maximum score.

   The algorithm works as follows. 

   At each step, we randomly change some sites (nodes in the tree). 
   We then accept or reject these proposed changes, using a probability that is determined by the weight of the measure at the new tree. 
   If rejected, we repeat the previous sample. 

    This kernel is related to the one introduced by [Wingate, Stuhlmuller, Goodman, AISTATS 2011](http://proceedings.mlr.press/v15/wingate11a/wingate11a.pdf), but it is different in that it works when the number of sites is unknown. Moreover, since a site is a path through the tree, the address is more informative than a number, which avoids some addressing issues. 

    When 1/@p@ is roughly the number of used sites, then this will be a bit like "single-site lightweight" MH.
    If @p@ = 1 then this is "multi-site lightweight" MH.

    __Tip:__ if @m :: Prob a@ then use @fmap (map fst) (mh 1 $ sample m)@ to get a stream of samples from a probability distribution without conditioning. 
--}

mh :: forall a.
   Double -- ^ The chance @p@ of changing any site, in [0,1]
   -> Meas a -- ^ The unnormalized measure to sample from
   -> IO [(a,Product (Log Double))] -- ^ Returns a stream of (result,weight) pairs
mh p = metropolis p Nothing

-- A label that the model never reads stays unevaluated, and its delayed
-- mutations keep the earlier trees alive. So every 32 accepted mutateTree
-- proposals we swap the tree for a checkpoint with the same labels, which
-- labelAt computes from the stored seeds. The seed store in
-- LazyPPL.Internal.History appends one 16-byte record per accepted mutateTree
-- proposal, and its vector doubles when full. An accepted all-sites change
-- starts a new store.
metropolis :: Double -> Maybe Double -> Meas a -> IO [(a, Product (Log Double))]
metropolis p refresh (Meas m)
  -- The comparisons are false for NaN, so NaN is rejected too.
  | not (p >= 0 && p <= 1) = error ("mh: the chance p of changing a site must be in [0,1], got " ++ show p)
  | Just q <- refresh, not (q > 0 && q <= 1) =
      error ("mhirreducible: the chance q of an all-sites change must be in (0,1], got " ++ show q)
  | otherwise = do
    g <- newStdGen
    let (g1, g2) = split g
        t = randomTree g1
        (x, w) = checkedSample (runProb (runWriterT m) t)
    history <- newHistory g1
    let go generator tree value weight history count = unsafeInterleaveIO $ do
          let (proposalSeed, acceptanceSeed) = split generator
              (fresh, seed) = case refresh of
                Nothing -> (False, proposalSeed)
                Just q -> let (r, next) = randomLabel proposalSeed
                          in (r < q, next)
              proposed = if fresh then randomTree seed else mutateTree p seed tree
              (value', weight') = checkedSample (runProb (runWriterT m) proposed)
              ratio = getProduct weight' / getProduct weight
              (r, nextGenerator) = randomLabel acceptanceSeed
          -- At weight 0 the ratio can be 0/0 = NaN and the test then fails,
          -- so we accept every move until the weight is positive. r is a label, in
          -- (0,1), as for the tree: random's 1 would reject a move of ratio 1 and
          -- an all-sites change at q = 1.
          if getProduct weight == 0 || r < min 1 (exp (ln ratio))
            then do
              (history', count') <- if fresh
                then do h <- newHistory seed; pure (h, 0)
                else do appendMutation history count seed; pure (history, count+1)
              let tree' = if count' > 0 && count' `rem` 32 == 0
                            then checkpoint p history' count'
                            else proposed
              tree' `seq` pure ()
              rest <- go nextGenerator tree' value' weight' history' count'
              pure ((value', weight') : rest)
            else do
              rest <- go nextGenerator tree value weight history count
              pure ((value, weight) : rest)
    go g2 t x w history 0
  where
    -- A product of valid weights can still overflow to a logarithm of +Infinity.
    checkedSample (v, Product l) = (v, Product (checkedLogWeight "mh" l))

checkpoint :: Double -> History -> Int -> Tree
checkpoint p history count = node []
  where
    node path = Tree (labelAt p history count path) (children path 0)
    children path index = node (index:path) : children path (index+1)


-- | Replace the labels of a tree randomly, with probability p
mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
mutateTree p g (Tree a ts) =
  let (a',g') = randomLabel g in
  let (a'',g'') = randomLabel g' in
#ifdef LAZYPPL_BROWSER
  -- Wasm has a 32-bit address space, so we evaluate each visited label to stop
  -- its thunk from keeping the old trees alive.
  let label = if a'<p then a'' else a in
  label `seq` Tree label (mutateTrees p g'' ts)
#else
  Tree (if a'<p then a'' else a) (mutateTrees p g'' ts)
#endif
mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
#ifdef LAZYPPL_BROWSER
mutateTrees p g (t:ts) =
  let (g1,g2) = split g
      t' = mutateTree p g1 t
  in t' `seq` (t' : mutateTrees p g2 ts)
#else
mutateTrees p g (t:ts) = let (g1,g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts
#endif



{- | Irreducible form of 'mh'. Takes @p@ like 'mh', but also @q@ in (0,1], which is the chance of proposing an all-sites change. Irreducibility means that, asymptotically, the sequence of samples will converge in distribution to the renormalized version of @m@, provided that the total weight of @m@ is positive and finite and that computing each weight terminates. The parts of a sample that are never demanded may be infinite and stay unevaluated. With @q = 0@ there would be no all-sites change, and so no such guarantee; @q = 0@ raises an error. 

The kernel in `mh` is not formally irreducible in the usual sense, although it is an open question whether this is a problem for asymptotic convergence in any definable model. In any case, convergence is only asymptotic, and so it can be helpful to use `mhirreducible` is that in some situations.

Roughly this avoids `mh` getting stuck in one particular mode, although it is a rather brutal method.

 -}

mhirreducible :: forall a.
              Double -- ^ The chance @p@ of changing any given site, in [0,1]
              -> Double -- ^ The chance @q@ of doing an all-sites change, in (0,1]
              -> Meas a -- ^ The unnormalized measure @m@ to sample from
              -> IO [(a,Product (Log Double))] -- ^ Returns a stream of (result,weight) pairs
mhirreducible p q = metropolis p (Just q)


{- | Useful function which thins out a stream of results, as is common in Markov Chain Monte Carlo simulation.

@every n xs@ returns only the elements at indices that are multiples of n.--}
every :: Int -> [a] -> [a]
every n xs = case drop (n -1) xs of
  (y : ys) -> y : every n ys
  [] -> []

