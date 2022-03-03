{-- Program induction: finding programs that may have noisly generated 
    some data points.
    Inspired by Section 5.3 of An Introduction to Probabilistic Programming
    van de Meent, Paige, Yang, Wood, arxiv:1809.10756
 --}
module ProgramInduction where
import LazyPPL
import Distr

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.State
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens

-- A little programming language for arithmetic expressions and if-then-else

-- A datatype of expressions in one variable
data Expr = Var | Constt Double | Add Expr Expr | Mult Expr Expr
          | IfLess Double Expr Expr

-- pretty printing expressions
instance Show Expr where
  show Var = "x"
  show (Constt r)  = show $ (fromInteger $ round $ r * 10) / 10.0
  show (Add e1 e2) = "(" ++ show e1 ++ " + "++show e2++")"
  show (Mult e1 e2) = "(" ++ show e1 ++ " . "++show e2++")"
  show (IfLess r e1 e2) =
    "(if x<" ++ show (Constt r) ++ " then "++show e1 ++" else "++show e2++")"

-- evaluating expressions, or converting expressions into functions 
eval :: Expr -> Double -> Double
eval Var x = x
eval (Constt r) _ = r
eval (Add e1 e2) x = (eval e1 x) + (eval e2 x)
eval (Mult e1 e2) x = (eval e1 x) * (eval e2 x)
eval (IfLess r e1 e2) x = if x < r then eval e1 x else eval e2 x

-- Two mutually recursive functions coming up to build an expression randomly
-- This one gets a random list of possible expressions, and picks one
randexpr :: Prob Expr
randexpr = do
  options <- randoptions
  i <- categorical [0.3,0.3,0.18,0.18,0.04]
  return $ options !! i

-- This one makes a random list of possible expresions
randoptions :: Prob [Expr]
randoptions =
 do constr <- normal 0 5
    adde1 <- randexpr
    adde2 <- randexpr
    multe1 <- randexpr
    multe2 <- randexpr
    ifr <- normal 0 5
    ife1 <- randexpr
    ife2 <- randexpr
    return 
      [Var , Constt constr , Add adde1 adde2 , Mult multe1 multe2 ,
      IfLess ifr ife1 ife2]

-- This version is how you would do it eagerly, (ok, you would write it with case)
-- but it is really inefficient:
-- the seeds are reused when you switch branches. 
randexpralt :: Prob Expr
randexpralt = do
  i <- categorical [0.3,0.3,0.18,0.18,0.04]
  e <- [return Var ,
        do { n <- normal 0 5 ; return $ Constt n },
        do { e1 <- randexpralt ; e2 <- randexpralt ; return $ Add e1 e2},
        do { e1 <- randexpralt ; e2 <- randexpralt ; return $ Mult e1 e2},
        do { r <- normal 0 5 ; e1 <- randexpralt ; e2 <- randexpralt ; return $ IfLess r e1 e2}]
       !! i        
  return e

-- This small change works much better,
-- and matches randexpr.
-- But it only makes sense when lazy,
-- because es is an infinite thing.
randexpraltbetter :: Prob Expr
randexpraltbetter = do
  i <- categorical [0.3,0.3,0.18,0.18,0.04]
  es <- sequence
     [return Var ,
     do { n <- normal 0 5 ; return $ Constt n },
     do { e1 <- randexpraltbetter ; e2 <- randexpraltbetter ; return $ Add e1 e2},
     do { e1 <- randexpraltbetter ; e2 <- randexpraltbetter ; return $ Mult e1 e2},
     do { r <- normal 0 5 ; e1 <- randexpraltbetter ; e2 <- randexpraltbetter ; return $ IfLess r e1 e2}]
  return $ es !! i


randfun :: Prob (Double -> Double,String)
randfun = do
  e <- randexpr
  return (eval e,show e)

{-- Regression: a random function "prior", 
    and some input/output observations "dataset", 
    which are assumed to be noisy according to "sigma", 
    return a conditioned random function (unnormalized) 
    The prior functions are tagged with a string, so we can 
    print them. 
--} 
regress :: Double -> Prob (a -> Double,String) -> [(a,Double)]
           -> Meas (a->Double,String)
regress sigma prior dataset =
  do (f,s) <- sample prior
     mapM (\(x,y) -> score $ normalPdf (f x) sigma y) dataset
     return (f,s)

{-- A sample dataset --}
dataset :: [(Double, Double)]
dataset = [(0,0.6), (1, 0.7), (2,1.2), (3,3.2), (4,6.8), (5, 8.2), (6,8.4)]

{-- Run a Metropolis Hastings simulation for 5000 steps --}
draw = do fws <- mh 0.1 (regress 0.25 randfun dataset)
          return $ fst $ fws !! 5000

{-- Show three examples --} 
testExprCodeRegression =
  do
    red <- draw
    blue <- draw
    purple <- draw
    plot_funs "expr-reg.svg" dataset [red,blue,purple]



{-- GRAPHING ROUTINES --}
 

-- Plot the points drawn from weighted samples
-- More graphing routines
-- epsilon: smallest y axis difference to worry about
-- delta: smallest x axis difference to worry about
interesting_points :: (Double -> Double) -> Double -> Double -> Double -> Double -> [Double] -> [Double]
interesting_points f lower upper epsilon delta acc =
  if abs(upper - lower) < delta then acc
  else
    let mid = (upper - lower) / 2 + lower in
    if abs((f(upper) - f(lower)) / 2 + f(lower) - f(mid)) < epsilon 
    then acc
    else interesting_points f lower mid epsilon delta (mid : (interesting_points f mid upper epsilon delta acc))
 
sample_fun f = 
--  [ (x, f x) | x <- [(-0.25),(-0.25+0.1)..6.2]]
  let xs = ((-0.25) : (interesting_points f (-0.25) 6.2 0.3 0.001 [6.2])) in
  map (\x -> (x,f x)) xs 

plot_funs :: String -> [(Double,Double)] -> [(Double -> Double,String)] -> IO ()
plot_funs filename dataset funs =
  let my_dots = plot_points_style .~ filledCircles 4 (opaque black)
              $ plot_points_values .~ dataset
              $ def in
  let fes = map (\(f,e) -> (sample_fun f,e)) funs in
  let red_lines  = plot_lines_style . line_color .~ red `withOpacity` 1 
                $ plot_lines_style . line_width .~ 2
                $ plot_lines_values .~ [(fst $ fes !! 0)] $ def in
  let red_font = font_size .~ 15
              $ font_color .~ red `withOpacity` 1
              $ def in
  let red_legend = plot_annotation_hanchor .~ HTA_Left
              $ plot_annotation_values .~ [(1,-0.5,snd $ fes !! 0)]
              $ plot_annotation_style .~ red_font
              $ def in
  let blue_lines  = plot_lines_style . line_color .~ blue `withOpacity` 1 
                $ plot_lines_style . line_width .~ 2
                $ plot_lines_values .~ [(fst $ fes !! 1)] $ def in
  let blue_font = font_size .~ 15
              $ font_color .~ blue `withOpacity` 1
              $ def in
  let blue_legend = plot_annotation_hanchor .~ HTA_Left
              $ plot_annotation_values .~ [(1,-1.3,snd $ fes !! 1)]
              $ plot_annotation_style .~ blue_font
              $ def in
  let purple_lines  = plot_lines_style . line_color .~ purple `withOpacity` 1 
                $ plot_lines_style . line_width .~ 2
                $ plot_lines_values .~ [(fst $ fes !! 2)] $ def in
  let purple_font = font_size .~ 15
              $ font_color .~ purple `withOpacity` 1
              $ def in
  let purple_legend = plot_annotation_hanchor .~ HTA_Left
              $ plot_annotation_values .~ [(1,-2.1,snd $ fes !! 2)]
              $ plot_annotation_style .~ purple_font
              $ def in
  let my_layout = layout_plots .~ [toPlot red_lines , toPlot red_legend, toPlot blue_lines, toPlot blue_legend, toPlot purple_lines, toPlot purple_legend, toPlot my_dots]
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
main = do {  testExprCodeRegression } 
