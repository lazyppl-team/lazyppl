{- | Gaussian processes in the `LazyPPL` library.

Gaussian processes are random functions. We provide the Wiener process (`wiener`) and a generic Gaussian process (`gp`). 

Although the implementation uses hidden state, it is still safe, i.e. statistically commutative and discardable. 

For illustrations, see the [Gaussian process regression](https://lazyppl-team.github.io/GaussianProcessDemo.html) 
or [Wiener process regression](https://lazyppl-team.github.io/WienerDemo.html). The latter also illustrates composing the Wiener process function to get jump diffusion.
-}


module LazyPPL.Distributions.GP (wiener, gp) where

import LazyPPL (Prob,Tree(Tree),Prob(Prob),runProb)
import LazyPPL.Distributions (normal,iid)
import Numeric.LinearAlgebra hiding (step,size,find)
import Numeric.LinearAlgebra.Data hiding (step,size,find)
import Data.List (find)
import Data.Map (empty,lookup,insert,size,keys,elems,Map)
import Data.IORef
import System.IO.Unsafe
import Prelude hiding ((<>))

{-| [Wiener process](https://en.wikipedia.org/wiki/Wiener_process) (Brownian motion).

This is a random function. 

The Wiener process is implemented by using a Brownian bridge and a hidden memo table. -}
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




{-| [Gaussian process](https://en.wikipedia.org/wiki/Gaussian_process) with given covariance function.

This is a random function.

The function is defined by using linear algebra and a hidden memo table.
Although it uses hidden state, it is still safe, i.e. statistically commutative and discardable. --}
gp :: (Double -> Double -> Double) -- ^ Covariance function
      -> Prob (Double -> Double) -- ^ Returns a random function
gp cov = do ns <- iid $ normal 0 1
            return $ unsafePerformIO $ do
                                 ref <- newIORef Data.Map.empty
                                 modifyIORef' ref (Data.Map.insert 0 0)
                                 return $ \x -> unsafePerformIO $ do
                                        table <- readIORef ref
                                        case Data.Map.lookup x table of
                                             Just y -> do {return y}
                                             Nothing -> do let y = step cov table x $ ns !! (1 + size table)
                                                           modifyIORef' ref (Data.Map.insert x y)
                                                           return y

step :: (Double -> Double -> Double) -> Data.Map.Map Double Double -> Double -> Double -> Double
step cov table x seed =
       let sig11 = cov x x 
           sig12 = matrix (size table) [cov x b | b <-keys table] 
           sig21 = matrix 1 [cov a x | a <-keys table] 
           sig22 = matrix (size table) [cov a b | a <- keys table , b <-keys table] 
           regCoeff = sig12 <> (pinvTol 1E8 sig22)
           mu = (regCoeff <> (matrix 1 $ elems table)) `atIndex` (0,0)
           var = sig11 - ((regCoeff <> sig21) `atIndex` (0,0)) in
       mu + seed * (sqrt $ if var > -0.01 then (abs var) else error (show var))

