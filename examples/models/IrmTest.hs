{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IrmTest where

import LazyPPL.Distributions
import LazyPPL.Distributions.DirichletP
import LazyPPL.Distributions.Memoization
import LazyPPL

instance (MonadMemo Prob String) where memoize = generalmemoize

type Person = String

-- | Simple Infinite Relational Model Example from Web Church / Prob Mods
-- | A Chinese Restaurant, where tables are social groups
example :: Meas (Bool, Bool)
example = do
  r :: Restaurant <- sample $ newRestaurant 1.0
  -- Chance of people at 'tableA' talking to people at 'tableB'
  near :: ((Table, Table) -> Double) <- sample $ memoize $ \(tableA, tableB) -> beta 0.5 0.5
  -- Assign a table to each person
  table :: (Person -> Table) <- sample $ memoize $ \person -> newCustomer r
  -- function to observe that personA talks to person B
  let talks :: (Person, Person) -> Meas () = \(personA, personB) -> score $ near (table personA, table personB)
  -- function to observe that personA doesn't talk to person B
  let nottalks :: (Person, Person) -> Meas () = \(personA, personB) -> score $ 1 - near (table personA, table personB)
  -- Data set
  mapM_ talks [("tom", "fred"), ("tom", "jim"), ("jim", "fred"), ("mary", "sue"), ("mary", "ann"), ("ann", "sue")]
  mapM_ nottalks [("mary", "fred"), ("mary", "jim"), ("sue", "fred"), ("sue", "tom"), ("ann", "jim"), ("ann", "tom")]
  -- We want to know whether Tom and Fred are at the same table,
  -- and whether Tom and Mary are at the same table.
  return (table "tom" == table "fred", table "tom" == table "mary")

test = do
  bcws <- mh 0.2 example
  let bcs = map fst $ take 100 . every 1000 . drop 10000 $ bcws
  print $ fromIntegral (length $ filter fst bcs) / fromIntegral (length bcs)
  print $ fromIntegral (length $ filter snd bcs) / fromIntegral (length bcs)

main = test

{-- Web Church Program from http://v1.probmods.org/non-parametric-models.html#example-the-infinite-relational-model
Seems to return the wrong result: Says ~35% chance of Tom and Mary in the same group.

(define samples
  (mh-query
   300 10000

   (define class-distribution (DPmem 1.0 gensym))

   (define object->class
     (mem (lambda (object) (class-distribution))))

   (define classes->parameters
     (mem (lambda (class1 class2) (beta 0.5 0.5))))

   (define (talks object1 object2)
     (flip (classes->parameters (object->class object1) (object->class object2))))

   (list (equal? (object->class 'tom) (object->class 'fred))
         (equal? (object->class 'tom) (object->class 'mary)))

   (and (talks 'tom 'fred)
        (talks 'tom 'jim)
        (talks 'jim 'fred)
        (not (talks 'mary 'fred))
        (not (talks 'mary 'jim))
        (not (talks 'sue 'fred))
        (not (talks 'sue 'tom))
        (not (talks 'ann 'jim))
        (not (talks 'ann 'tom))
        (talks 'mary 'sue)
        (talks 'mary 'ann)
        (talks 'ann 'sue)
        )))

(hist (map first samples) "tom and fred in same group?")
(hist (map second samples) "tom and mary in same group?")

--}

{-- Rejection sampler in Web Church for validation
    This returns the same results as LazyPPL mh.

(define (samples)
  (rejection-query

   (define class-distribution (DPmem 1.0 gensym))

   (define object->class
     (mem (lambda (object) (class-distribution))))

   (define classes->parameters
     (mem (lambda (class1 class2) (beta 0.5 0.5))))

   (define (talks object1 object2)
     (flip (classes->parameters (object->class object1) (object->class object2))))

   (list (equal? (object->class 'tom) (object->class 'fred))
         (equal? (object->class 'tom) (object->class 'mary)))

   (and (talks 'tom 'fred)
        (talks 'tom 'jim)
        (talks 'jim 'fred)
        (not (talks 'mary 'fred))
        (not (talks 'mary 'jim))
        (not (talks 'sue 'fred))
        (not (talks 'sue 'tom))
        (not (talks 'ann 'jim))
        (not (talks 'ann 'tom))
        (talks 'mary 'sue)
        (talks 'mary 'ann)
        (talks 'ann 'sue)
        )))

(hist (map first (repeat 200 samples)) "tom and mary in same group?")
(hist (map second (repeat 200 samples)) "tom and mary in same group?")

--}
