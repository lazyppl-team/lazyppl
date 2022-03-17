---
title: Feature finding using an Indian Buffet Process
---

A demonstration of using Indian Buffet Process for feature finding.
    Following 
    [A Nonparametric Bayesian Method for Inferring Features From Similarity Judgments](https://cocosci.princeton.edu/tom/papers/adclusnips.pdf). 
    Navarro and Griffiths. NeurIPS 2006.

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>

> {-# LANGUAGE ScopedTypeVariables #-}

> module AdditiveClusteringDemo where

> import LazyPPL
> import Distr
> import Distr.IBP
> import Distr.Memoization

> import Data.List
> import Control.Monad
> import Data.Monoid

> import Data.Array
> import Data.Maybe
> import Graphics.Matplotlib

</details>

Additive clustering is a method for assigning features to a set of 
of objects using similarity data. The number of possible features is
unknown and inferred using the Indian Buffet Process. Each feature has an associated 
"saliency" weight indicating how much it contributes to the similarity 
coefficient. We also infer the saliency weights.   


Data set
---



> data Country = China | Cuba | Germany | Indonesia | Iraq | Italy | Jamaica | Japan | Libya | Nigeria | Philippines | Russia | Spain | US | Vietnam | Zimbabwe deriving (Eq, Ord, Show, Enum, Bounded, Ix)

> countries :: [Country]
> countries = [China .. Zimbabwe]

> similarityData :: Array (Country,Country) Double

<details class="code-details">
<summary>Data from experiment</summary>

> similarityData = array ((minBound,minBound),(maxBound,maxBound)) [((i,j),if i > j then (countriesDataset !! (fromEnum i) !! (fromEnum j)) else if i == j then 1 else (countriesDataset !! (fromEnum j) !! (fromEnum i))) | i <- countries , j <- countries]
>  where
>  countriesDataset :: [[Double]] =
>   [[ ],
>   [0.11],
>   [0.06, 0.04],
>   [0.43, 0.06, 0.03],
>   [0.06, 0.32, 0.04, 0.14],
>   [0.02, 0.09, 0.70, 0.02, 0.03],
>   [0.02, 0.59, 0.02, 0.14, 0.04, 0.10],
>   [0.69, 0.01, 0.26, 0.35, 0.03, 0.06, 0.03],
>   [0.03, 0.32, 0.01, 0.04, 0.70, 0.04, 0.11, 0.01],
>   [0.01, 0.12, 0.01, 0.04, 0.20, 0.03, 0.31, 0.01, 0.45],
>   [0.42, 0.12, 0.01, 0.87, 0.09, 0.02, 0.17, 0.31, 0.05, 0.04],
>   [0.51, 0.35, 0.55, 0.01, 0.13, 0.22, 0.02, 0.17, 0.05, 0.02, 0.03],
>   [0.02, 0.37, 0.58, 0.03, 0.04, 0.90, 0.20, 0.04, 0.04, 0.03, 0.04, 0.15],
>   [0.30, 0.11, 0.42, 0.03, 0.06, 0.20, 0.12, 0.46, 0.02, 0.04, 0.01, 0.43, 0.20],
>   [0.60, 0.12, 0.03, 0.55, 0.12, 0.01, 0.05, 0.45, 0.10, 0.03, 0.57, 0.08, 0.02, 0.12],
>   [0.01, 0.08, 0.01, 0.11, 0.15, 0.02, 0.29, 0.01, 0.31, 0.83, 0.08, 0.01, 0.02, 0.01, 0.03]]

Data set taken from [Representing Stimulus Similarity](https://digital.library.adelaide.edu.au/dspace/handle/2440/21902), PhD thesis, D. J. Navarro. Department of Psychology
University of Adelaide. December, 2002

</details>


Additive clustering model
----

Our additive clustering models has three parameters (`alpha`, `lambda1` and `lambda2`) and produces a mapping from countries to lists of features. We use an Indian Buffet Process abstraction, so we imagine that each country arrives as a customer at a buffet, and takes some dishes, which are the features. 

> additiveClustering :: Double -> Double -> Double -> Array (Country,Country) Double -> Meas (Array Country [Dish])
> additiveClustering alpha lambda1 lambda2 similarityData = do
>     (restaurant :: Restaurant) <- sample $ newRestaurant alpha

Each dish is randomly assigned a weight, which is how important it is to the similarities between countries. 

>     (weights :: Dish -> Double) <- sample $ memoize (\d -> gamma lambda1 lambda2)

Each country is assigned a list of dishes, by regarding it as a new customer in the restaurant. 

>     (features :: Array Country [Dish])
>            <- listArray (minBound,maxBound) <$>
>               (replicateM (fromEnum (maxBound :: Country) + 1) $
>               sample $ newCustomer restaurant)

We define a function that calculates the similarity of two countries given the dishes that they share.

>     let similarity :: Country -> Country -> Double
>         similarity i j = sum [weights a | a <- features!j, a `elem` features!i]

We score based on how closely this similarity matches the experimental data.

>     mapM_ (\(i, j) -> score $ normalPdf (similarityData!(i,j)) 0.1 (similarity i j)) [ (i, j) | i <- countries, j <- countries, j < i ]

We return the feature assignment. 

>     return features

Simulation
---

We sample from this using Metropolis Hastings simulation, and plot some MAP features.

> test = do
>   samples <- mhirreducible 0.2 0.01 $ additiveClustering 2 6 0.3 similarityData
>   return $ plotFeatureMatrix $ maxap $ take 100000 samples

![Inferred feature matrices.](images/additiveclustering-matrix.svg)

This is a highly multi-modal distribution. For the three runs, we typically have different numbers of features, and they are different. But in each case they admit a human interpretation, for example geographic, history, development status. 

<details class="code-details">
<summary>Plotting routines</summary>

> maxap samples =
>         let maxw = (maximum $ map snd samples) in
>         fromJust $ Data.List.lookup maxw $ map (\(z,w) -> (w,z)) samples 

> plotFeatureMatrix :: (Ix x,Enum x,Bounded x,Eq y) => Array x [y] -> Matplotlib
> plotFeatureMatrix features = do
>   let ys = nub $ concat $ elems features
>   let matrix = [[ if elem y (features ! x) then fromJust (elemIndex y ys) else 0 | y <- ys] | x <- [minBound..maxBound] ]
>   matShow matrix @@ [o2 "cmap" $ raw "twilight"] % yticks (map fromEnum countries) % ytickLabels (map (raw . show) countries) 

> combinePlots filename fs = do
>   putStrLn $ "Generating " ++ filename ++ "..."
>   file filename $ foldl (\p i -> p % setSubplot i % fs !! i) (subplots @@ [o2 "ncols" (length fs), o2 "sharey" True, o2 "gridspec_kw" $ lit "{'width_ratios': [1, 1, 1]}"]) [0..(length fs -1)] 
>   putStrLn "Done."

> main = do { ps <- replicateM 3 test ; combinePlots "images/additiveclustering-matrix.svg" ps }

</details>