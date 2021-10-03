{-# LANGUAGE BangPatterns #-}
module Distr.Counter (Counter,newCounter,readAndIncrement) where

import LazyPPL
import Data.IORef
import System.IO.Unsafe

{--
Some "unsafe" functions for a hidden counter. 
This is useful for implementing some nonparametric models 
like the indian buffet process. 
The function is deterministic but we perform some redundant sampling
so that the function is treated as impure by the compiler and 
thus re-evalued every time.  

The implementation of the counter (IORef Int) is encapsulated.
--}

data Counter = C (IORef Int)

newCounter :: Prob Counter
newCounter = do r <- uniform
                return $ C $ unsafePerformIO $ newIORef (round (r-r))

readAndIncrement :: Counter -> Prob Int 
readAndIncrement (C ref) = do
    r <- uniform
    return $ unsafePerformIO $ do 
        !i <- readIORef ref 
        () <- writeIORef ref (i + 1 + round (r - r))
        return i 


