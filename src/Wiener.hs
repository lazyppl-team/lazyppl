module Wiener where
import LazyPPL
import Distr

import Data.List
import Data.Map (empty,lookup,insert,size,keys)
import Data.IORef
import System.IO.Unsafe

import Graphics.Matplotlib hiding (density)


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
    plotCoords "wiener.svg" dataset xys


{-- Random Wiener function (Brownian motion), defined using hidden state and a "Brownian bridge" --} 
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

{-- GRAPHING ROUTINES --}
plotCoords :: String -> [(Double,Double)] -> [[(Double,Double)]] -> IO ()
plotCoords filename dataset xyss = 
    do  putStrLn $ "Plotting " ++ filename ++ "..."
        file filename $ foldl (\a xys -> a % plot (map fst xys) (map snd xys) @@ [o1 "go-", o2 "linewidth" (0.5 :: Double), o2 "alpha" (0.1 :: Double), o2 "ms" (0 :: Int)]) (scatter (map fst dataset) (map snd dataset) @@ [o2 "c" "black"] % xlim (0 :: Int) (6 :: Int) % ylim (-2 :: Int) (10 :: Int)) xyss
        putStrLn "Done."
        return ()
    

main :: IO ()
main = do { testWienerRegression } 
