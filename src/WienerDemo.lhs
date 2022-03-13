---
title: Wiener process regression in LazyPPL
---

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
module WienerDemo where
import LazyPPL
import Distr

import Data.List
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe

import Graphics.Matplotlib hiding (density)
\end{code}
</details>
<br>

We can define a random function `wiener :: Prob(Double -> Double)`{.haskell} which describes a Wiener process.
This requires an infinite number of random choices, first because it is defined for arbitrarily large `x`, but also it needs an infinite number of random choices in each finite interval, because it is no-where differentiable. This is all dealt with using laziness. We can't plot this unbounded, undifferentiable function precisely, but when we plot it with a fixed resolution and viewport, the necessary finite random choices are triggered. 
![](images/wiener-prior.svg)
<details class="code-details">
<summary>(Plotting code)</summary>
\begin{code}
plotWienerPrior =
  do
    fws <- mh 1 $ sample wiener
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.1..6]) $ map fst $ take 100 $ fws
    plotCoords "images/wiener-prior.svg" [] xys (-7) 7 0.2
\end{code}
</details>
<br>
We will use this random function as a prior for Bayesian regression, as in <a href="Regression.html">the other regression examples</a>. Here is our example data set: 
\begin{code}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]
\end{code}
And here is our model where we combine a Wiener function `g` plus a start point `a`.
(We could also have built it with the second-order `regress` function from <a href="Regression.html">the other regression examples</a>.)
\begin{code}
example :: Meas (Double -> Double)
example = do g <- sample wiener
             a <- sample $ normal 0 3
             let f x = a + 2 * (g x)
             mapM (\(x,y) -> score $ normalPdf (f x) 0.3 y) dataset
             return f
\end{code}
We can now sample from the unnormalized distribution, using Metropolis-Hastings. 
Because of laziness, the values of the functions will be sampled at different times,
some only when we come to plot the functions. 
\begin{code}
plotWienerRegression =
  do
    fws <- mh 0.1 example
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.1..6]) $ map fst $ take 100 $ every 1000 $ drop 10000 $ fws
    plotCoords "images/wiener-reg.svg" dataset xys (-2) 10 0.1
\end{code}
![](images/wiener-reg.svg)

The Wiener function itself is defined by using a Brownian bridge and a hidden memo table.
Although it uses hidden state, it is still safe, i.e. statistically commutative and discardable. 
<details class="code-details">
<summary>Definition of Wiener function</summary>
\begin{code}
wiener :: Prob (Double -> Double)
wiener = Prob $ \(Tree r gs) ->
                   unsafePerformIO $ do
                                 ref <- newIORef Data.Map.empty
                                 modifyIORef' ref (Data.Map.insert 0 0)
                                 return $ \x -> unsafePerformIO $ do
                                        table <- readIORef ref
                                        case Data.Map.lookup x table of
                                             Just y -> do {return y}
                                             Nothing -> do let lower = do {l <- findMaxLower x (keys table) ;
                                                                           v <- Data.Map.lookup l table ; return (l,v) }
                                                           let upper = do {u <- find (> x) (keys table) ;
                                                                           v <- Data.Map.lookup u table ; return (u,v) }
                                                           let m = bridge lower x upper
                                                           let y = runProb m (gs !! (1 + size table))
                                                           modifyIORef' ref (Data.Map.insert x y)
                                                           return y

bridge :: Maybe (Double,Double) -> Double -> Maybe (Double,Double) -> Prob Double
-- not needed since the table is always initialized with (0, 0)
-- bridge Nothing y Nothing = if y==0 then return 0 else normal 0 (sqrt y) 
bridge (Just (x,x')) y Nothing = normal x' (sqrt (y-x))
bridge Nothing y (Just (z,z')) = normal z' (sqrt (z-y))
bridge (Just (x,x')) y (Just (z,z')) = normal (x' + ((y-x)*(z'-x')/(z-x))) (sqrt ((z-y)*(y-x)/(z-x)))

findMaxLower :: Double -> [Double] -> Maybe Double 
findMaxLower d [] = Nothing
findMaxLower d (x:xs) = let y = findMaxLower d xs in
                       case y of 
                           Nothing -> if x < d then Just x else Nothing 
                           Just m -> do 
                                          if x > m && x < d then Just x else Just m 
\end{code}
</details>
<details class="code-details">
<summary>Graphing routines</summary>
\begin{code}
plotCoords :: String -> [(Double,Double)] -> [[(Double,Double)]] -> Double -> Double -> Double -> IO ()
plotCoords filename dataset xyss ymin ymax alpha = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a xys -> a % plot (map fst xys) (map snd xys) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" alpha, o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim ymin ymax) xyss
        putStrLn "Done."
        return ()
    

main :: IO ()
main = do { plotWienerPrior ; plotWienerRegression } 
\end{code}
</details>