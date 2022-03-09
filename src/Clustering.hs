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



{- | A generic Chinese-Restaurant clustering program
    Takes pparam :: Prob b, a distribution on parameters
          like :: b -> a -> Double, parameterized likelihood function \
    Tags each point with a colour describing the cluster it is in -}
cluster :: [a] -> Prob b -> (b -> a -> Double) -> Meas [(a, Double, b)]
cluster xs pparam like =
  do
    rest <- sample $ newRestaurant 2
    param <- sample $ memoize $ const pparam
    color <- sample $ memoize $ const $ uniformbounded 0.2 1
    mapM
      ( \x -> do
          i <- sample $ newCustomer rest
          score $ like (param i) x
          return (x, color i, param i)
      )
      xs


      

-- | Example 2d data set
dataset = [(7.7936387, 7.469271), (5.3105156, 7.891521), (5.4320135, 5.135559), (7.3844196, 7.478719), (6.7382938, 7.476735), (0.6663453, 4.460257), (3.2001898, 2.653919), (2.1231227, 3.758051), (3.3734472, 2.420528), (0.4699408, 1.835277)]

-- | 'nnormal' is a normal distribution but it uses two seeds,
-- | so perhaps it moves around a little more
-- | and it takes up more time in the single-site MH.
nnormal x s = do x1 <- normal x ((sqrt 15 * s * s) / 4); x2 <- normal 0 (s / 4); return $ x1 + x2

{- | Example of using the clustering program.
    Here the parameters are the x and y coordinates
    and standard deviation of the cluster -}
example :: Meas [((Double, Double), Double, (Double, Double, Double))]
example =
  cluster
    dataset
    (do x <- nnormal 5 4; y <- nnormal 5 4; prec <- gamma 2 4; return (x, y, 1 / sqrt prec))
    (\(x, y, s) (x', y') -> normalPdf x s x' * normalPdf y s y')

test =
  do
    xycws' <- mh1 example
    let xycws = take 5000 xycws'
    let maxw = (maximum $ map snd xycws :: Product (Log Double))
    let (Just xyc) = Data.List.lookup maxw $ map (\(z, w) -> (w, z)) xycws
    plot_coords "clustering.svg" xyc

{-- GRAPHING ROUTINES --}
-- plot_coords :: String -> [((Double, Double), Double, (Double, Double, Double))] -> IO ()
-- plot_coords filename xycs = undefined
--    let my_areas = map (\((_,_),c,(x,y,sigma)) -> let rgb = hsv (c * 360) 0.2 1 in toPlot $ plot_points_style .~ filledCircles (sigma*70) (opaque $ sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb))
--                        $ plot_points_values .~ [(x,y)]
--                        $ def) xycs in

--   let circle x y r = [((- r) * cos (a * 2 * pi / 360) + x, (r * sin (a * 2 * pi / 360) + y, (- r) * sin (a * 2 * pi / 360) + y)) | a <- [0, 0.5 .. 180 :: Double]]
--    in let my_areas =
--             map
--               ( \((_, _), c, (x, y, sigma)) ->
--                   let rgb = hsv (c * 360) 0.15 1
--                    in toPlot $
--                         plot_fillbetween_style .~ solidFillStyle (opaque $ sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)) $
--                           plot_fillbetween_values .~ (circle x y (2 * sigma)) $
--                             def
--               )
--               xycs
--        in let my_dots =
--                 map
--                   ( \((x, y), c, _) ->
--                       let rgb = hsv (c * 360) 1 1
--                        in toPlot $
--                             plot_points_style .~ filledCircles 4 (opaque $ sRGB (channelRed rgb) (channelGreen rgb) (channelBlue rgb)) $
--                               plot_points_values .~ [(x, y)] $
--                                 def
--                   )
--                   xycs
--            in let my_layout =
--                     layout_plots .~ (my_areas ++ my_dots) $
--                       layout_x_axis
--                         . laxis_generate
--                         .~ scaledAxis def (0, 10)
--                         $ layout_y_axis . laxis_generate .~ scaledAxis def (-2, 10) $
--                           def
--                in let graphic = toRenderable my_layout
--                    in do
--                         putStr ("Generating " ++ filename ++ "...")
--                         renderableToFile def filename graphic
--                         putStrLn (" Done!")
--                         return ()

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

main :: IO ()
main = do test
