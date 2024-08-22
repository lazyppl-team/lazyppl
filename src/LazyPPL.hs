{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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

        See also the SingleSite module for a separate single-site Metropolis-Hastings algorithm via GHC.Exts.Heap and System.IO.Unsafe.

    3. Various useful helpful functions.

    A typical usage would be

@   
    import LazyPPL (Prob, Meas, uniform, sample, score, mh, every)
@

    Most of the structure here will not be needed in typical models. We expose more of the structure for more experimental uses. 

The `Distributions` module provides many useful distributions, and further non-parametric distributions are in `Distributions.DirichletP`, `Distributions.GP`, `Distr.IBP`, and `Distr.Memoization`. 


-}

module LazyPPL
    ( -- * Rose tree type
      --
      -- | Our source of randomness will be an infinitely wide and deep lazy [rose tree](https://en.wikipedia.org/wiki/Rose_tree), regarded as initialized with uniform [0,1] choices for each label.
      Tree(Tree),
      -- * Monads
      Prob(Prob), Meas(Meas),
      -- * Basic interface
      --
      -- | There are three building blocks for measures: `uniform` for probability measures; `sample` and `score` for unnormalized measures. Combined with the monad structure, these give all s-finite measures.
      uniform, sample, score,
      -- * Monte Carlo simulation
      --
      -- | The `Meas` type describes unnormalized measures. Monte Carlo simulation allows us to sample from an unnormalized measure. Our main Monte Carlo simulator is `mh`. 
      mh, mhirreducible, weightedsamples, wis, 
      -- * Useful functions
      every, randomTree, runProb) where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import System.Random hiding (uniform)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Lazy (State, state , put, get, runState)
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

{- | A uniform sample is a building block for probability distributions.

This is implemented by getting the label at the head of the tree and discarding the rest.-}
uniform :: Prob Double
uniform = Prob $ \(Tree r _) -> r

-- | Regard a probability measure as an unnormalized measure.
sample :: Prob a -> Meas a
sample p = Meas $ lift p

{- | A one point measure with a given score (or weight, or mass, or likelihood), which should be a positive real number.

A score of 0 describes impossibility. To avoid numeric issues, we encode it as @ exp(-300) @ instead.-}
score :: Double -> Meas ()
score r = Meas $ tell $ Product $ (Exp . log) (if r==0 then exp(-300) else r)

scoreLog :: Log Double -> Meas ()
scoreLog r = Meas $ tell $ Product r

scoreProductLog :: Product (Log Double) -> Meas ()
scoreProductLog r = Meas $ tell r


{- | Generate a tree with uniform random labels.

    This uses 'split' to split a random seed. -}
randomTree :: RandomGen g => g -> Tree
randomTree g = let (a,g') = random g in Tree a (randomTrees g')
randomTrees :: RandomGen g => g -> [Tree]
randomTrees g = let (g1,g2) = split g in randomTree g1 : randomTrees g2

{- | 'runProb' runs a probability deterministically, given a source of randomness. -}
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
    newStdGen
    g <- getStdGen
    let rs = randomTree g
    let xws = runProb helper rs
    return $ map (\(x,w) -> (x, getProduct w)) xws

{- | Weighted importance sampling first draws n weighted samples,
    and then samples a stream of results from that, regarded as an empirical distribution. Sometimes called "likelihood weighted importance sampling". 

This is a reference implementation. It will not usually be very efficient at all, but may be useful for debugging. 
 -}
wis :: Int -- ^ @n@, the number of samples to base on
    -> Meas a -- ^ @m@, the measure to normalize
    -> IO [a] -- ^ Returns a stream of samples
wis n m = do
  xws <- weightedsamples m
  let xws' = take n $ accumulate xws 0
  let max = snd $ last xws'
  newStdGen
  g <- getStdGen
  let rs = (randoms g :: [Double])
  return $ map (\r -> fst $ head $ filter (\(x, w) -> w >= Exp (log r) * max) xws') rs
  where accumulate ((x, w) : xws) a = (x, w + a) : (x, w + a) : accumulate xws (w + a)
        accumulate [] a = []

{- | Produce a stream of samples, using Metropolis Hastings simulation.

   The weights are also returned. Often the weights can be discarded, but sometimes we may search for a sample of maximum score.

   The algorithm works as follows. 

   At each step, we randomly change some sites (nodes in the tree). 
   We then accept or reject these proposed changes, using a probability that is determined by the weight of the measure at the new tree. 
   If rejected, we repeat the previous sample. 

    This kernel is related to the one introduced by [Wingate, Stuhlmuller, Goodman, AISTATS 2011](http://proceedings.mlr.press/v15/wingate11a/wingate11a.pdf), but it is different in that it works when the number of sites is unknown. Moreover, since a site is a path through the tree, the address is more informative than a number, which avoids some addressing issues. 

    When 1/@p@ is roughly the number of used sites, then this will be a bit like "single-site lightweight" MH.
    If @p@ = 1 then this is "multi-site lightweight" MH.

    __Tip:__ if @m :: Prob a@ then use @map fst <$> (mh 1 $ sample m)@ to get a stream of samples from a probability distribution without conditioning. 
--}
{-- The algorithm is as follows:

    Top level: produces a stream of samples.
    * Split the random number generator in two
    * One part is used as the first seed for the simulation,
    * and one part is used for the randomness in the MH algorithm.

    Then, run 'step' over and over to get a stream of '(tree, result, weight)'s.
    The stream of seeds is used to produce a stream of result/weight pairs.

    NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g, g1, g2)'

    Steps of the 'step' function:
    1. Randomly change some sites, 
    2. Rerun the model with the new tree, to get a new weight 'w''.
    3. Compute the MH acceptance ratio. This is the probability of either returning the new seed or the old one.
    4. Accept or reject the new sample based on the MH ratio.  
--}
mh :: forall a.
   Double -- ^ The chance @p@ of changing any site
   -> Meas a -- ^ The unnormalized measure to sample from
   -> IO [(a,Product (Log Double))] -- ^ Returns a stream of (result,weight) pairs
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
    return $ map (\(_,x,w) -> (x,w)) samples
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where step :: RandomGen g => (Tree,a,Product (Log Double)) -> State g (Tree,a,Product (Log Double))
          step (t, x, w) = do
            -- Randomly change some sites
            g <- get
            let (g1, g2) = split g
            let t' = mutateTree p g1 t
            -- Rerun the model with the new tree, to get a new
            -- weight w'.
            let (x', w') = runProb (runWriterT m) t'
            -- MH acceptance ratio. This is the probability of either
            -- returning the new seed or the old one.
            let ratio = getProduct w' / getProduct w
            let (r, g2') = random g2
            put g2'
            if r < min 1 (exp $ ln ratio) -- (trace ("-- Ratio: " ++ show ratio) ratio))
              then return (t', x', w') -- trace ("---- Weight: " ++ show w') w')
              else return (t, x, w) -- trace ("---- Weight: " ++ show w) w)


-- | Replace the labels of a tree randomly, with probability p
mutateTree :: forall g. RandomGen g => Double -> g -> Tree -> Tree
mutateTree p g (Tree a ts) =
  let (a',g') = (random g :: (Double,g)) in
  let (a'',g'') = random g' in
  Tree (if a'<p then a'' else a) (mutateTrees p g'' ts)
mutateTrees :: RandomGen g => Double -> g -> [Tree] -> [Tree]
mutateTrees p g (t:ts) = let (g1,g2) = split g in mutateTree p g1 t : mutateTrees p g2 ts



{- | Irreducible form of 'mh'. Takes @p@ like 'mh', but also @q@, which is the chance of proposing an all-sites change. Irreducibility means that, asymptotically, the sequence of samples will converge in distribution to the renormalized version of @m@. 

The kernel in `mh` is not formally irreducible in the usual sense, although it is an open question whether this is a problem for asymptotic convergence in any definable model. In any case, convergence is only asymptotic, and so it can be helpful to use `mhirreducible` is that in some situations.

Roughly this avoids `mh` getting stuck in one particular mode, although it is a rather brutal method.

 -}

mhirreducible :: forall a.
              Double -- ^ The chance @p@ of changing any given site
              -> Double -- ^ The chance @q@ of doing an all-sites change
              -> Meas a -- ^ The unnormalized measure @m@ to sample from
              -> IO [(a,Product (Log Double))] -- ^ Returns a stream of (result,weight) pairs
mhirreducible p q (Meas m) = do
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
    return $ map (\(t,x,w) -> (x,w)) samples
    {- NB There are three kinds of randomness in the step function.
    1. The start tree 't', which is the source of randomness for simulating the
    program m to start with. This is sort-of the point in the "state space".
    2. The randomness needed to propose a new tree ('g1')
    3. The randomness needed to decide whether to accept or reject that ('g2')
    The tree t is an argument and result,
    but we use a state monad ('get'/'put') to deal with the other randomness '(g,g1,g2)' -}
    where step :: RandomGen g => (Tree,a,Product (Log Double)) -> State g (Tree,a,Product (Log Double))
          step (t,x,w) = do
            -- Randomly change some sites
            g <- get
            let (g1,g2) = split g
            -- Decide whether to resample all sites (r<q) or just some of them
            let (r,g1') = random g1
            let t' = if r<q then randomTree g1' else mutateTree p g1' t
            -- Rerun the model with the new tree, to get a new
            -- weight w'.
            let (x', w') = runProb (runWriterT m) t'
            -- MH acceptance ratio. This is the probability of either
            -- returning the new seed or the old one.
            let ratio = getProduct w' / getProduct w
            let (r,g2') = random g2
            put g2'
            if r < min 1 (exp $ ln ratio) then return (t',x',w') else return (t,x,w)



{- | Useful function which thins out a stream of results, as is common in Markov Chain Monte Carlo simulation.

@every n xs@ returns only the elements at indices that are multiples of n.--}
every :: Int -> [a] -> [a]
every n xs = case drop (n -1) xs of
  (y : ys) -> y : every n ys
  [] -> []

