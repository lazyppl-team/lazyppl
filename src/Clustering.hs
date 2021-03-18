module Clustering where
import LazyPPL
import Distr
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB

import Numeric.Log

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe

import Data.Default.Class
import Control.Lens
import Data.Monoid

import Debug.Trace

{-- Stick breaking breaks the unit interval into an 
    infinite number of parts (lazily) --}
stickBreaking :: Double -> Double -> Prob [Double]
stickBreaking alpha lower =
  do r <- beta 1 alpha
     let v = r * (1 - lower)
     vs <- stickBreaking alpha v
     return (v : vs)

{-- We can then define the Dirichlet Process --}
iid :: Prob a -> Prob [a]
iid p = do {r <- p ; rs <- iid p ; return $ r : rs}

dp :: Double -> Prob a -> Prob (Prob a)
dp alpha p = do xs <- iid p
                vs <- stickBreaking alpha 0
                return $ do r <- uniform 
                            return $ xs !! (fromJust $ findIndex (> r) (scanl1 (+) vs))

{-- Return a distribution over distributions 
    saying what the chance of getting the
    first, second, third bit of stick is etc --}
crp :: Double -> Prob (Prob Int) 
crp alpha = do vs <- stickBreaking alpha 0
               return $ do r <- uniform 
                           return $ fromJust $ findIndex (> r) (scanl1 (+) vs)

{-- A generic stick-breaking-based clustering program 
    Takes pparam :: Prob b, a distribution on parameters 
          like :: b -> a -> Double, parameterized likelihood function \
    Tags each point with a colour describing the cluster it is in --}
cluster :: [a] -> (Prob b) -> (b -> a -> Double) -> Meas [(a,Double)]
cluster xs pparam like =
        do pcluster <- sample $ crp 1
           param    <- sample $ memoize $ \i -> pparam
           color    <- sample $ memoize $ \i -> uniform 
           mapM (\x -> do {i <- sample pcluster ; score $ like (param i) x ; return (x,(color i))}) xs

-- Example 2d data set
dataset = [(7.7936387,7.469271),(5.3105156,7.891521),(5.4320135,5.135559),(7.3844196,7.478719),(6.7382938,7.476735),(0.6663453,4.460257),(3.2001898,2.653919),(2.1231227,3.758051),(3.3734472,2.420528),(0.4699408,1.835277)]

{-- Example of using the clustering program.
    Here the parameters are the x and y coordinates 
    and standard deviation of the cluster --} 
example :: Meas [((Double,Double),Double)]
example = cluster dataset (do {x <- normal 5 4 ; y <- normal 5 4 ; prec <- gamma 3 2 ; return (x,y,1/(sqrt prec)) })
                  (\(x,y,s) (x',y') -> normalPdf x s x' * normalPdf y s y')

test =
  do
    xycws' <- mh 0.05 example
    let xycws = take 50000 $ xycws'
    let maxw = (maximum $ map snd xycws :: Product (Log Double))
    let (Just xyc) = Data.List.lookup maxw $ map (\(z,w) -> (w,z)) xycws
    plot_coords "clustering.svg" xyc


{-- Stochastic memoization.
    We use unsafePerformIO to maintain 
    a table of calls that have already been made. 
    If a is finite, we could just sample all values of a in advance 
    and avoid unsafePerformIO. 
    If it is countably infinite, there probably also implementation tricks. 
--} 
memoize :: Ord a => (a -> Prob b) -> Prob (a -> b)
memoize f = Prob $ do g <- get
                      let ( (Tree _ gs), g2) = splitTree g
                      put g2
                      return $ unsafePerformIO $ do
                                ref <- newIORef Data.Map.empty
                                return $ \x -> unsafePerformIO $ do
                                          m <- liftM (Data.Map.lookup x) (readIORef ref)
                                          case m of
                                              Just y -> return y
                                              Nothing -> do let (Prob m) = f x 
                                                            n <- readIORef ref
                                                            let (y,_) = runState m (gs !! (1 + size n))
                                                            modifyIORef' ref (Data.Map.insert x y)
                                                            return y
 


{-- GRAPHING ROUTINES --}
plot_coords :: String -> [((Double,Double),Double)] -> IO ()
plot_coords filename xycs =
  let my_dots = map (\((x,y),c) -> let rgb = hsv (c * 360) 1 1 in toPlot $ plot_points_style .~ filledCircles 4 (opaque $ sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb))
                      $ plot_points_values .~ [(x,y)]
                      $ def) xycs in
  let my_layout = layout_plots .~ my_dots
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (0,10)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (-2,10)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()
    



main :: IO ()
main = do { test } 
