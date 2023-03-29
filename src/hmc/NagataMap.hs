{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StrictData #-}
{-# Language DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}


module Nagata where

import Data.List
import Data.Map
import Data.Monoid
import Data.Semigroup

data Vars = X | Y | Z deriving (Eq, Show, Ord)

data Expr v = Zero | One | Plus (Expr v) (Expr v) | Times (Expr v) (Expr v) | Var v
    deriving Show

class Semiring d where
    zero  :: d
    one   :: d
    (⊕)   :: d -> d -> d
    (⊗)   :: d -> d -> d

instance Semiring (Expr v) where
    zero = Zero
    one  = One
    (⊕) = Plus
    (⊗) = Times

instance Num a => Semiring a where
    zero = 0
    one  = 1
    (⊕) = (+)
    (⊗) = (*)

eval :: (Semiring d) => (v -> d) -> Expr v -> d
eval var Zero = zero
eval var (Var v) = var v
eval var One = one
eval var (Plus a b) = eval var a ⊕ eval var b
eval var (Times a b) = eval var a ⊗ eval var b

class MMonoid e where
    z :: e
    (➕)  :: e -> e -> e

class (Semiring d, MMonoid e) => Module d e | e -> d where
    (•)  :: d -> e -> e

class (Module d e) => Kronecker v d e where
    delta  :: v -> e

data d ⋉ e = N {pri :: d , tan :: e} deriving (Show)

instance Functor ((⋉) d) where
    fmap h (N f df) = N f (h df)

instance (Module d e) => Semiring (d ⋉ e) where
    zero = N zero z
    one  = N one z
    (N f df) ⊕ (N g dg) = N (f ⊕ g) (df ➕ dg)
    (N f df) ⊗ (N g dg) = N (f ⊗ g) ((f • dg) ➕ (g • df))

abstractD :: (Kronecker v d e) => (v -> d) -> v -> Expr v -> (d ⋉ e)
abstractD var x0 = eval gen where
    gen x = N (var x) (delta x)

---

-- I. implementation - Dual numbers

-- data Dual v =  Dual v v deriving (Show, Eq)

-- instance (Monoid d) => Monoid (Dual d) where
--     z = Dual z z
--     (Dual a b) ➕ (Dual c d) = Dual (a ➕ c) (b ➕ d)

-- instance (Monoid d) => Monoid (Dual d) where
--     z = Dual z z
--     (Dual a b) ➕ (Dual c d) = Dual (a ➕ c) (b ➕ d)

-- forwardAD :: (Semiring d, Eq v) => (v -> d) -> v -> Expr v -> Dual d
-- forwardAD = abstractD

-- II. implementation - dense functions

-- instance (Semiring d, Eq v) => Module d (v -> d) where
--     f • cont = \x -> f ⊗ cont x

-- instance (Semiring d, Eq v) => Kronecker v d (v -> d) where
--     delta x0 = \x -> if x == x0 then one else zero

-- instance Show (Vars -> Int) where
--     show f = "x -> " ++ show (f X) ++ ", y -> " ++ show (f Y) ++ ", z -> " ++ show (f Z) ++ ", "

-- forwardAD :: (Semiring d, Eq v, Ord v) => (v -> d) -> v -> Expr v -> d ⋉ (v -> d)
-- forwardAD = abstractD

-- III. implementation - maps

-- These are already defined

instance (Semiring d, Ord d, Ord v) => (MMonoid (Map v d)) where
    z = Data.Map.empty
    (➕) = unionWith (⊕)

instance (Semiring d, Eq v, Ord v, Ord d) => Module d (Map v d) where
    t • m = Data.Map.map (⊗ t) m

instance (Semiring d, Eq v, Ord v, Ord d) => Kronecker v d (Map v d) where
    delta x0 = Data.Map.singleton x0 one

forwardAD :: (Semiring d, Eq v, Ord v, Ord d) => (v -> d) -> v -> Expr v -> d ⋉ (Map v d)
forwardAD = abstractD

main :: IO ()
main = do
    let f = Times (Var X) (Plus (Var X) One)
    let x0 = 4
    let res = (abstractD (const x0) X f :: Double ⋉ (Map Vars Double))
    print $ "The numeric derivative at x0=" ++ show x0
    print res


-- f x = (x * (x+1)) = x^2 + x
-- df/dx = 2x + 1