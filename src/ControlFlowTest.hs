{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ControlFlowTest where

import LazyPPL
import Distr

model1 = 
  do x <- bernoulli 0.5
     y <- if x then bernoulli 0.4 else bernoulli 0.7
     return (x,y)
test1 = do { (x,y) <- sample model1 ; score (if x==y then 1 else 0) ; return x }     

run1 = do
  xws <- mh 0.3 test1
  let xs = map fst $ take 1000 $ every 100 xws
  return $ (length $ filter id xs) 

model2=
  do x <- bernoulli 0.5
     ytrue <- bernoulli 0.4
     yfalse <- bernoulli 0.7
     return (if x then (x,ytrue) else (x,yfalse)) 
test2 = do { (x,y) <- sample model1 ; score (if (x==y) then 1 else 0) ; return x }

run2 = do
  xws <- mh 0.3 test2
  let xs = map fst $ take 1000 $ every 100 xws
  return $ (length $ filter id xs) 
