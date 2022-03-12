---
title: Discussion of program transformations and laziness
---


<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
module ControlFlowDemo where

import LazyPPL
import Distr
import Control.Monad
import Graphics.Matplotlib
\end{code}
</details>
<br>

We consider a simple but slightly contrived model to illustrate how control flow transformations and laziness can be advantageous. Our first prior on pairs of booleans.
\begin{code}
prior1 :: Prob (Bool,Bool)
prior1 = 
  do x <- bernoulli 0.5
     y <- if x then bernoulli 0.4 else bernoulli 0.7
     return (x,y)
\end{code}
Our model conditions on the two booleans being equal. This means that if `x` changes, `y` has to change too, and vice versa. 
\begin{code}
model :: Prob (Bool,Bool) -> Meas Bool
model prior =
  do (x,y) <- sample prior
     score (if x==y then 1 else 0)
     return x
\end{code}
The posterior probability of `(model prior1)` returning true is 4/7. 

Since `x` and `y` are correlated, it is not a good idea to use a naive Metropolis Hastings simulation. But this is an extreme example, and less extreme examples might appear in more complex probabilistic programs that do benefit from Metropolis Hastings simulation, where the correlations are not known in advance. 

Here is a second implementation of the prior. It is mathematically equivalent, and computationally equivalent on each run, because of laziness. But it works better with Metropolis Hastings because it explicitly uses different sites for the two different conditional branches. 
\begin{code}
prior2 :: Prob (Bool,Bool)
prior2 =
  do x <- bernoulli 0.5
     ytrue <- bernoulli 0.4
     yfalse <- bernoulli 0.7
     return (if x then (x,ytrue) else (x,yfalse)) 
\end{code}
Of course, we could solve this particular program analytically. The point is rather that we have a universal program transformation manipulation that could be applied automatically. 

The following chart shows that the expected sample size (the ratio between the variance of a single true sample and the variance among the generated samples) is much better with this second implementation of the prior, although Metropolis Hastings is still far from optimal for this contrived problem.
![](images/controlflow-ess.png)

In single-site Metropolis Hastings (`mh1`), the first implementation of the prior is especially bad, because _all_ proposals will be rejected except perhaps one.

<br>

<details class="code-details">
<summary>(Code for effective sample size)</summary>
\begin{code}
-- Take a sampler (m) and calculate the mean, variance and effective sample size
-- for the estimator that returns the probability of return true.
-- k is the number of samples to use in the estimator.
-- n is the number of runs to average over to approximate the mean, variance and ess.
meanVarESS :: IO [(Bool,a)] -> Int -> Int -> IO (Double,Double,Double)
meanVarESS m n k =
  do xwss <- replicateM n m
     let as = map ((\x -> (fromIntegral x) / (fromIntegral k)). length . filter id . map fst . take k) xwss
     let mean = (sum as) / (fromIntegral n) -- samplemean
     let var = (sum (map (\x -> (x - mean)^2) as)) / (fromIntegral n) -- samplevariance
     let sigmasquared = mean - mean^2 -- estimated variance for one true sample
     return (mean,var,sigmasquared/var)
\end{code}
</details>
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}     
plotESS = 
  do let xs = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,30,40,50,60,70,80,90,100]
     -- A rejection sampler where the scores are known to be 0 or 1.
     let rejectionsampler m = fmap (filter (\(_,w) -> w>0.00001)) $ weightedsamples $ m
     -- Tests to plot
     as <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (mh 0.33 $ model prior1) 1000 
     bs <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (mh1 $ model prior1) 1000 
     cs <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (mh 0.33 $ model prior2) 1000 
     ds <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (mh1 $ model prior2) 1000
     es <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (mh 1 $ model prior1) 1000
     fs <- forM xs $ fmap (\(_,_,ess)->ess) . meanVarESS (rejectionsampler $ model prior1) 1000
     file "images/controlflow-ess.png" $
       plot xs bs @@ [o2 "label" "mh1 with prior1"] %
       plot xs ds @@ [o2 "label" "mh1 with prior2"] %
       plot xs as @@ [o2 "label" "mh 0.3 with prior1"] %
       plot xs cs @@ [o2 "label" "mh 0.3 with prior2"] %
       plot xs es @@ [o2 "label" "all sites mh"] %
       plot xs fs @@ [o2 "label" "rejection sampling"] %
       ylim (1::Double) (6::Double) %
       xlabel "Samples" % ylabel "Effective sample size" % legend @@ [o2 "loc" "upper right"]

main = plotESS
\end{code}
</details>
