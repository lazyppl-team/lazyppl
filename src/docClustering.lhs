

An example of non-parametric clustering using a Dirichlet process. 

<details class="code-details">
<summary>Extensions and imports for this Literate Haskell file</summary>
\begin{code}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clustering where
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Default.Class
import Data.List
import Data.Monoid
import Distr
import Distr.DirichletP
import Distr.Memoization
import LazyPPL
import Numeric.Log
import Graphics.Matplotlib hiding (density)
\end{code}
</details>


A generic Chinese-Restaurant clustering program. The parameters are:

*  xs :: [a], a data set  
*  pparam :: Prob b, from which we sample a parameter for each cluster 
*  like :: b -> a -> Double, parameterized likelihood function, 
      which we use to score the cluster assignment for each data point 

\begin{code}
cluster :: [a] -> Prob b -> (b -> a -> Double) -> Meas [(a, Double, b)]
cluster xs pparam like =
  do
    rest <- sample $ newRestaurant 0.3          
                                      -- sample a distribution from a Dirichlet process  
    param <- sample $ memoize $ const pparam    
                                      -- lazily sample an infinite list of cluster parameters  
    color <- sample $ memoize $ const $ uniformbounded 0.2 1
                                      -- lazily sample an infinite list of colors 
    -- for each data point, sample a cluster and return the corresponding color and parameter  
    mapM
      ( \x -> do
          i <- sample $ newCustomer rest   
          score $ like (param i) x
          return (x, color i, param i)
      )
      xs
\end{code}

We illustrate this on an example synthetic data set:       

> dataset = [(7.7936387, 7.469271), (5.3105156, 7.891521), (5.4320135, 5.135559),
>            (7.3844196, 7.478719), (6.7382938, 7.476735), (0.6663453, 4.460257), 
>            (3.2001898, 2.653919), (2.1231227, 3.758051), (3.3734472, 2.420528), 
>            (0.4699408, 1.835277)]



We define a version of the normal distribution ('nnormal'), this is equivalent
to sampling from a normal distribution but it uses two random seeds. 
With our multi-site Metropolis-Hastings implementation, this will perhaps 
encourage a bit more exploration. On the other hand with single-site MH it will 
likely take more time. 

> nnormal x s = do x1 <- normal x ((sqrt 15 * s * s) / 4); x2 <- normal 0 (s / 4); return $ x1 + x2

We try clustering on the synthetic data set above. 

The code below is the model. The return value is a list where each data point is tagged with a color 
and the parameters for the cluster we assigned it (coordinates of mean and standard deviation).  
\begin{code}
example :: Meas [((Double, Double), Double, (Double, Double, Double))]
example =
  cluster
    dataset
    (do x <- nnormal 5 4; y <- nnormal 5 4; prec <- gamma 2 4; return (x, y, 1 / sqrt prec))
    (\(x, y, s) (x', y') -> normalPdf x s x' * normalPdf y s y')
\end{code}

Then the inference code is as follows:
\begin{code}
infer =
  do
    xycws' <- mh1 example
    let xycws = take 5000 xycws'
    let maxw = (maximum $ map snd xycws :: Product (Log Double))
    let (Just xyc) = Data.List.lookup maxw $ map (\(z, w) -> (w, z)) xycws
    plot_coords "clustering.svg" xyc -- for illustration we plot the MAP sample


main :: IO ()
main = do { infer }
\end{code}

This produces the following cluster asssignment:  

![](../clustering.svg)


<details class="code-details">
<summary>Code for plotting the data points and clusters.</summary>
\begin{code} 
plot_coords :: String -> [((Double, Double), Double, (Double, Double, Double))] -> IO ()
plot_coords filename dataset = do 
  let starterplot = figure @@ [o1 0]
       % setSizeInches 8 8 
       % axes @@ [o1 [0.1, 0.1, 0.65, 0.65]]
  let gaussians = (foldl (\p (c,x,y,s) -> mplBivarNormal x y s c p) starterplot (nub $ map (\(_,c,(x,y,s))->(c,x,y,s)) dataset))
  let plot = foldl 
             (\p -> \((x,y),c,_) -> let c' = hsv (c * 365) 1 1 in 
                         p % scatter [x] [y] @@ [o2 "color" [channelRed c',channelGreen c',channelBlue c']])
             gaussians
             dataset
  file filename plot
  putStrLn $ "generating " ++ filename ++ "... done."

mplBivarNormal :: Double -> Double -> Double -> Double -> Matplotlib -> Matplotlib
mplBivarNormal mux muy sigma c p =
          p % imshow ws @@ [o2 "interpolation" "bilinear"
               ,o2 "origin" "lower"
               ,o2 "extent" [0::Double, 8, 0, 8]]
                 where delta = 0.025::Double
                       xs = [0.0+delta..8.0]
                       ys = [0.0+delta..8.0]
                       r = channelRed(hsv (c * 365) 1 1)
                       g = channelGreen(hsv (c * 365) 1 1)
                       b = channelBlue(hsv (c * 365) 1 1)
                       ws = [[[r,g,b,pdfBivariateNormal x y sigma sigma mux muy 0.0] | x <- xs] | y <- ys]

pdfBivariateNormal x y sigmax sigmay mux muy sigmaxy =
  1/(2*pi*sigmax*sigmay*(sqrt(1-rho^2)))*exp(-z/(2*(1-rho^2)))
  where rho = sigmaxy/(sigmax*sigmay)
        z = (x-mux)^2/sigmax^2-(2*rho*(x-mux)*(y-muy))/(sigmax*sigmay)+(y-muy)^2/sigmay^2
\end{code}
</details>

