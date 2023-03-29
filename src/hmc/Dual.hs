{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StrictData #-}
{-# Language DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}

import Data.List

data Vars = X | Y | Z deriving (Eq, Show)

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

data Dual v =  Dual v v deriving (Show, Eq)

instance Functor Dual where
    fmap f (Dual x y) = Dual (f x) (f y)


instance Semiring v => Semiring (Dual v) where
    zero = Dual zero zero
    one  = Dual one zero
    (Dual a0 a1) ⊕ (Dual b0 b1) = Dual (a0 ⊕ b0) (a1 ⊕ b1)
    (Dual a0 a1) ⊗ (Dual b0 b1) = Dual (a0 ⊗ b0) ((a0 ⊗ b1) ⊕ (a1 ⊗ b0))

-- derive :: (Eq v) => v -> Expr v -> Dual (Expr v)
-- derive v Zero = Dual zero zero
-- derive v One = Dual one zero
-- derive v (Var w) = Dual (Var w) (if v == w then 1 else 0)
-- derive v (Plus a b) = (derive v a) ⊕ (derive v b)
-- derive v (Times a b) = (derive v a) ⊗ (derive v b)

symbolic :: (Eq v) => v -> Expr v -> Dual (Expr v)
symbolic x0 = eval gen where
    gen x = Dual (Var x) (delta x x0)
    delta x x0 = if x == x0 then One else Zero

forwardAD :: (Eq v, Semiring d) => (v -> d) -> v -> Expr v -> Dual d
forwardAD var x = fmap (eval var) . symbolic x

main :: IO ()
main = do
    let f = Times (Var X) (Plus (Var X) One)
    let x0 = 4
    let res = (forwardAD (const x0) X f :: Dual Int)
    print $ "The numeric derivative at x0=" ++ show x0
    print res
