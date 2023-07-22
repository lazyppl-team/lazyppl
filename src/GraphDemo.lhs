---
title: Graph inference
---

We discuss graph inference. The key idea is to work against an abstract interface 
with a type `Vertex` of vertices, a distribution `new` on `Vertex`, and an edge relation
`edge :: Vertex -> Vertex -> Bool`. 

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module RegressionDemo where
import LazyPPL
import Distr
import Control.Monad
import Data.List
import Data.Maybe
import Graphics.Matplotlib
import Distr.Memoization

import Graphics.Matplotlib hiding (density)

instance MonadMemo Prob Double where
  memoize = generalmemoize
\end{code}
</details>

Graph inference problem
-----------------------
Consider the following graph. We will ask, which kinds of vertices and distribution on vertices likely generated it? 

![](images/graph-data.svg)

<details class="code-details">
<summary>Adjacency matrix</summary>
\begin{code}
test :: [[Bool]]
test = map (map $ \x -> x==1)
 [[0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
  [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
  [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0], 
  [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0], 
  [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0], 
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0], 
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1], 
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0], 
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0]]
\end{code}
</details>

Graph basics 
---------

We define a class of random graphs, that include a parameter `p`, a type `v` of vertices, and `new` and `edge` functions. 
\begin{code}
class Show v => RandomGraph p v | p -> v where
  new :: p -> Prob v
  edge :: p -> v -> v -> Bool
\end{code}
The first kind of instance is a [geometric graph on a sphere](https://en.wikipedia.org/wiki/Random_geometric_graph), parameterized by `n :: Int` and `theta :: Double`. 
Vertices are uniformly distributed on the sphere, and there is an edge between two vertices if their angle is less than `theta`. 

\begin{code}
data SphGrph = SG Int Double
data SphVert = SV [Double]

instance Show SphVert where
  show (SV (x:y:xs)) = show x ++ "," ++ show (SV (y:xs))
  show (SV (x:[])) = show x

instance RandomGraph SphGrph SphVert where
  new (SG d theta) = 
   do
     xs <- replicateM d $ normal 0 1 
     let r = sqrt $ sum $ map (^ 2) xs
     return $ SV $ map (/ r) xs
  edge (SG n theta) (SV xs) (SV ys) = theta > (acos $ sum $ (zipWith (*) xs ys))
\end{code}
Here, we have encoded the sphere using Eucildean coordinates. 
We are generating the points uniformly, by normalizing coordinates from a d-variate normal distribution. 
(This implementation detail could be hidden, by putting this in a separate file and not exporting the `SV` constructor.)

Another kind of instance is specified by a [graphon](https://en.wikipedia.org/wiki/Graphon), which is a function $[0,1]^2\to [0,1]$.  
\begin{code}
data Graphon = Graphon (Double -> Double -> Double)

data GraphonVertex = GV Double (Double->Bool)

instance Show GraphonVertex where
  show (GV x _) = show x

instance RandomGraph Graphon GraphonVertex where
  new (Graphon f) = do
    x <- uniform
    p <- memoize $ \y -> bernoulli $ sqrt $ f x y
    return $ GV x p
  edge _ (GV x p) (GV y q) = p y && q x
\end{code}
The Erdos-Renyi graphon is a constantly grey graphon.
\begin{code}
erdosRenyi r = Graphon (\_ _ -> r)
\end{code}
We also have coproducts of instances:
\begin{code}
instance (RandomGraph a v,RandomGraph b w) => RandomGraph (Either a b) (Either v w) where
  new (Left a) = do {x <- new a ; return $ Left x}
  new (Right a) = do {x <- new a ; return $ Right x}
  edge (Left p) (Left x) (Left y) = edge p x y
  edge (Right p) (Right x) (Right y) = edge p x y 
  edge _ _ _ = False
\end{code}

Graph inference
---------------

We define a function that generates a graph from any given implementation of the `RandomGraph` interface. 

\begin{code}
ngraph :: RandomGraph p v => Int -> p -> Prob ([[Bool]],String)
ngraph n p = do vs <- replicateM n $ new p
                let matrix = [[edge p v w | w <- vs] | v <- vs]
                let csv = concat [show v ++ "\n" | v <- vs]
                return (matrix,csv)
\end{code}

We suppose that matrix `a` was generated by the same process as `b` but where the chance of error is `r`.
The `like` function scores by this error, and returns the number of correct cells in the adjacency matrix. 
\begin{code}
like :: Double -> [[Bool]] -> [[Bool]] -> Meas Int
like r a b = do
   let n = sum [if a!!i!!j==b!!i!!j then 1 else 0 | i <- [0..length a-1] , j <- [0..(i-1)]]
   score $ r ^ n * (1-r) ^ ((floor $ 0.5 * fromIntegral (length a * (length a-1)))-n)
   return n
\end{code}

<details class="code-details">
<summary>Optimized uniform sampling</summary>
\begin{code}
superuniformbounded n a b = do
   xs <- replicateM n uniform
   let x = sum xs
   return $ x * (b - a) + a
\end{code}
</details>

A statistical model that describes the distribution on `theta` on a sphere, conditioned on the graph at the beginning of this page. 
\begin{code}
exampleA = do
     theta <- sample (superuniformbounded 3 0 pi)
     let p = SG 3 theta
     (x,csv) <- sample (ngraph 15 p)
     n <- like 0.999 test x
     return ((n,theta),(x,csv))
\end{code}

<details class="code-details">
<summary>Metropolis Hastings call, prints a CSV of the coordinates of the points.</summary>
\begin{code}
testA = do
     tws <- mh 0.1 exampleA
     let tws' = take 20 $ every 1000 $ tws
     forM tws' $ print . fst . fst
     putStrLn $ snd $ snd $ fst $ last tws'
\end{code}
</details>

This produces the following points on the sphere, matching the graph:
<iframe height=500 width=100% src="images/graph-sphere.html"> </iframe>


Inferring between different kinds of graph
---------
We check that the chance of an edge is the same in the Erdos-Renyi graph with `r=0.5` and the geometric graph with `theta=pi/2`.
\begin{code}
exampleB = do
     b <- sample $ bernoulli 0.5
     let p = if b then Left (erdosRenyi 0.5) else Right (SG 2 (pi / 2))
     c <- sample $ do {a <- new p ; b <- new p ; return $ edge p a b}
     if c then score 1 else score 0
     return b
\end{code}     
<details class="code-details">
<summary>Metropolis Hastings call.</summary>
\begin{code}
testB = do
      bws <- mh 1 $ exampleB
      plotHistogram "graph-edge.svg" $ map fst $ take 100000 bws
\end{code}
</details>
![](images/graph-edge.svg)

Next: If we see a triangle, what is the chance that we are in the Erdos-Renyi graph?

The following code tests whether we have a triangle. 

\begin{code}
isTriangle :: RandomGraph p v => p -> Prob Bool
isTriangle p = do
           x <- new p
           y <- new p
           z <- new p
           return (edge p x y && edge p y z && edge p x z)
\end{code}

Now, given that we have a triangle, did it come from a sphere or from the Erdos-Renyi graph? 
We do this by rejecting the non-triangles. 
\begin{code}
exampleC = do
     b <- sample $ bernoulli 0.5
     let p = if b then Left (erdosRenyi 0.5) else Right (SG 2 (pi / 2))
     c <- sample $ isTriangle p
     if c then score 1 else score 0
     return b
\end{code}

<details class="code-details">
<summary>Metropolis Hastings call.</summary>
\begin{code}
testC = do
      bws <- mh 1 $ exampleC
      plotHistogram "graph-triangle.svg" $ map fst $ take 100000 bws
\end{code}
</details>
We see that it is more likely to have come from the Erdos-Renyi graph. 
![](images/graph-triangle.svg)



<details class="code-details">
<summary>Plotting code.</summary>
\begin{code}
graphToDot :: [[Bool]] -> String
graphToDot g = let vs = [0..length g -1] in
               "graph {node[shape=point] \n edge[len=0.5] \n " ++
               concat [show v ++ "\n" | v <- vs] ++ 
               concat [show v ++ " -- " ++ show w ++ "\n" | v <- vs , w <- [0..v] , g !! v !! w]
               ++ "}"


plotHistogram :: (Show a , Eq a, Ord a) => String -> [a] -> IO ()
plotHistogram filename xs = do
 putStrLn $ "Generating " ++ filename ++ "..."
 let categories = sort $ nub xs
 let counts = map (\c -> length $ filter (==c) xs) categories
 file filename $ bar (map show categories) $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
 putStrLn $ show $ zip categories $ map (\n -> (fromIntegral n)/(fromIntegral $ length xs)) counts
 putStrLn $ "Done."

-- Plot histographs of chances of triangles in different graphs.
testD = do
      onER <- mh 1 $ sample $ isTriangle $ erdosRenyi    0.5
      onCircle <- mh 1 $ sample $ isTriangle $ SG 2 (pi/2)
      onSphere <- mh 1 $ sample $ isTriangle $ SG 3 (pi/2)
      onSphereFour <- mh 1 $ sample $ isTriangle $ SG 4 (pi/2)
      plotHistogram "tris-ER" $ map fst $ take 10000 onER
      plotHistogram "tris-s1" $ map fst $ take 10000 onCircle
      plotHistogram "tris-s2" $ map fst $ take 10000 onSphere
      plotHistogram "tris-s3" $ map fst $ take 10000 onSphereFour


main = do {testA; testB; testC}

\end{code}

</details>