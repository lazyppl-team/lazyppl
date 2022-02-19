{-# LANGUAGE RecursiveDo #-} 
module TestMemRec where
import LazyPPL
import Distr
import Control.Monad.Fix


-- Implementing a Poisson point process as a function random Int -> Double
-- by writing a memoized recursive program

-- TLDR: We can use 
-- mfix f = Prob $ \w -> fix (\x -> runProb (f x) w)
-- and then we don't need Hugo's memrec.
-- Plus we can use recursive do notation if we want. 

-- First attempt. 
-- Very naive, doesn't work.
-- When we run it, we sometimes get f(n)<f(n-1)...
test1 = do f <- memoize $ \x -> if x == 0 then return 0 else
                                         helper x
           return f                                        
        where helper x = if x==0 then return 0 else
                           do r <- exponential 1.0
                              y <- helper (x-1)
                              return (y + r)

-- Test via
-- do { fs<-weightedsamples (sample test5) ; return $ map (fst (head fs)) [0..9] }

-- We might hope that the canonical lazy mfix does the trick, but it doesn't
mfixlazy f = fix (>>= f)
test3 = mfixlazy (\f -> memoize $ \x -> if x == 0 then return 0 else
                                     do r <- exponential 1.0
                                        return $ f (x-1) + r)

-- With Hugo's memrec, it works. 
test4 = memrec $ \f -> \x -> if x == 0 then return 0 else
                                     do r <- exponential 1.0
                                        return $ f (x-1) + r        

-- Works with altmfix too.
altmfix f = Prob $ \w -> fix (\x -> runProb (f x) w)

test5 = altmfix (\f -> memoize $ \x -> if x == 0 then return 0 else
                                     do r <- exponential 1.0
                                        return $ f (x-1) + r)

-- So maybe we should define: 
instance MonadFix Prob where mfix f = altmfix f

-- And then this works as we might expect
test6 = mdo f <- memoize $ \x -> if x == 0 then return 0 else
                                     do r <- exponential 1.0
                                        return $ f (x-1) + r
            return $ map f [0..9]
-- Test with
-- do { fs<-weightedsamples (sample test7) ; return $ fst (head fs)}

-- Checking how this mfix/mdo works with recursively defined lists. 
test7 = mdo xs <- do { r <- normal 0 1  ; return $ r : ys}
            ys <- do { r <- normal 10 1 ; return $ r : xs}
            return $ take 10 xs
-- Returns a list of alternating numbers. e.g.:
-- [-0.397,9.771,-0.397,9.771,-0.397,9.771,-0.397,9.771,-0.397,9.771]
-- Possibly this is confusing
-- and not what you would expect from recursively defined lists. 


-- But if that wasn't what you wanted then we can always
-- define mutually recursive ones like this.
-- The following returns a list of random numbers,
-- alternating between big and small.
-- I think this is the behaviour we'd get with mfixlazy.
test8 = do let xsprob = do { r <- normal 0 1  ; ys <- ysprob ; return $ r : ys}
               ysprob = do { r <- normal 10 1 ; xs <- xsprob ; return $ r : xs}
           xs <- xsprob
           return $ take 10 xs

-- Just as we can define the undesirable memoized recursion
-- (where the memoization happens after the recursive definition). 
test9 = do let fprob = memoize $ \x -> if x == 0 then return 0 else
                                     do r <- exponential 1.0
                                        f <- fprob
                                        return $ f (x-1) + r
           f <- fprob                                        
           return $ map f [0..9]
