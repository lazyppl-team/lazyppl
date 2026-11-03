module LazyPPL.Distributions.Counter (Counter,newCounter,readAndIncrement) where

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

-- | A counter that starts at 0. A counter holds hidden state: see 'LazyPPL.runProb' for its use outside a model.
newCounter :: Prob Counter
-- Both operations stay out of the caller's code. Inlined there, full laziness
-- (on at GHC's default -O) gave a counter created and read inside one Prob
-- computation the number 0 at every read (test/HiddenState.hs, -O1 and -O2).
{-# NOINLINE newCounter #-}
newCounter = do r <- uniform
                return $ C $ unsafePerformIO $ newIORef (round (r-r))

-- | The counter's next number: 0, then 1, and so on.
readAndIncrement :: Counter -> Prob Int 
{-# NOINLINE readAndIncrement #-}
readAndIncrement (C ref) = do
    r <- uniform
    -- One atomic step, so two threads never receive the same number.
    return $ unsafePerformIO $ atomicModifyIORef' ref (\i -> (i + 1 + round (r - r), i))


