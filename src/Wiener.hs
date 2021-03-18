module Wiener where
import LazyPPL
import Distr
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names

import Data.List
import Control.Monad
import Control.Monad.State.Lazy (State, state , put, get, runState)
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe

import Data.Default.Class
import Control.Lens
import Data.Monoid

import Debug.Trace


dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

{-- A regression model where we have a Wiener function g plus a start point a. --}
example :: Meas (Double -> Double)
example = do g <- sample wiener
             a <- sample $ normal 0 3
             let f x = a + g x
             mapM (\(x,y) -> score $ normalPdf (f x) 0.3 y) dataset
             return f

-- Note that example returns a random function,
-- and so mh will return a stream of (function,weight) pairs.
-- Because of laziness, the values of the functions will be sampled at different times,
-- some only when we come to plot the functions. 
testWienerRegression =
  do
    fws <- mh 0.1 example
    let xys = map (\f -> map (\x -> (x,f x)) [0,0.25..6]) $ map fst $ take 100 $ every 1000 $ drop 10000 $ fws
    plot_coords "wiener.svg" dataset xys


{-- Random Wiener function (Brownian motion), defined using hidden state and a "Brownian bridge" --} 
wiener :: Prob (Double -> Double)
wiener = Prob $ do g <- get
                   let ( (Tree r gs) , g2) = splitTree g
                   put g2
                   return $ unsafePerformIO $ do
                                 ref <- newIORef Data.Map.empty
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
bridge Nothing y Nothing = if y==0 then return 0 else normal 0 (sqrt y)
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

{-- GRAPHING ROUTINES --}
plot_coords :: String -> [(Double,Double)] -> [[(Double,Double)]] -> IO ()
plot_coords filename dataset xys =
  let graphs  = xys                 in
  let my_lines  = plot_lines_style . line_color .~ blue `withOpacity` 0.1
                $ plot_lines_values .~ graphs $ def in
  let my_dots = plot_points_style .~ filledCircles 4 (opaque black)
              $ plot_points_values .~ dataset
              $ def in               
  let my_layout = layout_plots .~ [toPlot my_lines , toPlot my_dots]
                $ layout_x_axis .
                  laxis_generate .~ scaledAxis def (0,6)
                $ layout_y_axis . laxis_generate .~ scaledAxis def (-2,10)
                $ def in
  let graphic =  toRenderable my_layout in
  do
     putStr ("Generating " ++ filename ++ "...")
     renderableToFile def filename graphic;
     putStrLn (" Done!")
     return ()
    



main :: IO ()
main = do { testWienerRegression } 
