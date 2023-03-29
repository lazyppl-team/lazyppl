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

module NagataMapNum where

import Data.List
import Data.Map
import Data.Monoid
import Data.Semigroup
import Data.Ratio

data Vars = X | Y | Z deriving (Eq, Show, Ord)

-- data Expr v where
--     Number :: Integer -> (Expr v)
--     Var :: v -> Expr v
--     Plus :: (Expr v) -> (Expr v) -> (Expr v)
--     Times :: (Expr v) -> (Expr v) -> (Expr v)
--     Negate :: (Expr v) -> (Expr v)
--     Recip :: (Expr v) -> (Expr v)
--     Signum :: (Expr v) -> (Expr v)
--     Abs :: (Expr v) -> (Expr v)
--     Pi :: (Expr v)
--     Exp :: (Expr v) -> (Expr v)
--     Log :: (Expr v) -> (Expr v)
--     Sqrt :: (Expr v) -> (Expr v)
--     Pow :: (Expr v) -> (Expr v) -> (Expr v)
--     LogBase :: (Expr v) -> (Expr v) -> (Expr v)
--     Sin :: (Expr v) -> (Expr v)
--     Cos :: (Expr v) -> (Expr v)
--     Tan :: (Expr v) -> (Expr v)
--     Asin :: (Expr v) -> (Expr v)
--     Acos :: (Expr v) -> (Expr v)
--     Atan :: (Expr v) -> (Expr v)
--     Sinh :: (Expr v) -> (Expr v)
--     Cosh :: (Expr v) -> (Expr v)
--     Tanh :: (Expr v) -> (Expr v)
--     Asinh :: (Expr v) -> (Expr v)
--     Acosh :: (Expr v) -> (Expr v)
--     Atanh :: (Expr v) -> (Expr v)


data Expr v = 
    Number Integer |
    Var v |
    Plus (Expr v) (Expr v) |
    Times (Expr v) (Expr v) |
    Negate (Expr v) |
    Recip (Expr v) |
    Signum (Expr v) |
    Abs (Expr v) |
    Pi |
    Exp (Expr v) |
    Log (Expr v) |
    Sqrt (Expr v) |
    Pow (Expr v) (Expr v) |
    LogBase (Expr v) (Expr v) |
    Sin (Expr v) |
    Cos (Expr v) |
    Tan (Expr v) |
    Asin (Expr v) |
    Acos (Expr v) |
    Atan (Expr v) |
    Sinh (Expr v) |
    Cosh (Expr v) |
    Tanh (Expr v) |
    Asinh (Expr v) |
    Acosh (Expr v) |
    Atanh (Expr v)
    deriving (Ord, Eq)

instance Show v => Show (Expr v) where
    show (Number x) = show x
    show (Var v) = show v
    show (Plus e1 e2) = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (Times e1 e2) =  "" ++ (show e1) ++ "*" ++ (show e2) ++ ""
    show (Pow e1 e2) = "" ++ (show e1) ++ "**" ++ (show e2) ++ ""
    show (Abs e) = "|" ++ (show e) ++ "|"
    show (Pi) = "π"
    show (LogBase e1 e2) = "log_" ++ (show e1) ++ " " ++ (show e2)
    show (Recip e) = "1/(" ++ (show e) ++ ")"
    show (Negate e) = "-(" ++ (show e) ++ ")"
    show (Signum e) = "sign(" ++ (show e) ++ ")"
    show (Exp e) = "exp(" ++ (show e) ++ ")"
    show (Log e) = "log(" ++ (show e) ++ ")"
    show (Sqrt e) = "sqrt(" ++ (show e) ++ ")"
    show (Sin e) = "sin(" ++ (show e) ++ ")"
    show (Cos e) = "cos(" ++ (show e) ++ ")"
    show (Tan e) = "tan(" ++ (show e) ++ ")"
    show (Asin e) = "asin(" ++ (show e) ++ ")"
    show (Acos e) = "acos(" ++ (show e) ++ ")"
    show (Atan e) = "atan(" ++ (show e) ++ ")"
    show (Sinh e) = "sinh(" ++ (show e) ++ ")"
    show (Cosh e) = "cosh(" ++ (show e) ++ ")"
    show (Tanh e) = "tanh(" ++ (show e) ++ ")"
    show (Asinh e) = "asinh(" ++ (show e) ++ ")"
    show (Acosh e) = "acosh(" ++ (show e) ++ ")"
    show (Atanh e) = "atanh(" ++ (show e) ++ ")"


instance Num (Expr v) where
    fromInteger  = Number
    (+) = Plus
    (*) = Times
    negate = Negate
    abs = Abs
    signum = Signum

instance Fractional (Expr v) where
    fromRational r = (fromInteger $ numerator r) / (fromInteger $ denominator r)
    recip = Recip

instance Floating (Expr v) where
    pi = Pi
    exp = Exp
    log = Log
    sqrt = Sqrt
    (**) = Pow
    logBase = LogBase 
    sin = Sin
    cos = Cos
    tan = Tan 
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

eval :: (Floating d) => (v -> d) -> Expr v -> d
eval var (Number x) = fromInteger x
eval var (Var v) = var v
eval var (Plus e1 e2) = (eval var e1) + (eval var e2)
eval var (Times e1 e2) =  (eval var e1) * (eval var e2)
eval var (Recip e) = recip (eval var e)
eval var (Negate e) = negate (eval var e)
eval var (Signum e) = signum (eval var e)
eval var (Abs e) = abs (eval var e)
eval var (Pi) = pi
eval var (Exp e) = exp (eval var e)
eval var (Log e) = log (eval var e)
eval var (Sqrt e) = sqrt (eval var e)
eval var (Pow e1 e2) = (eval var e1) ** (eval var e2)
eval var (LogBase e1 e2) = logBase (eval var e1) (eval var e2)
eval var (Sin e) = sin (eval var e)
eval var (Cos e) = cos (eval var e)
eval var (Tan e) = tan (eval var e)
eval var (Asin e) = asin (eval var e)
eval var (Acos e) = acos (eval var e)
eval var (Atan e) = atan (eval var e)
eval var (Sinh e) = sinh (eval var e)
eval var (Cosh e) = cosh (eval var e)
eval var (Tanh e) = tanh (eval var e)
eval var (Asinh e) = asinh (eval var e)
eval var (Acosh e) = acosh (eval var e)
eval var (Atanh e) = atanh (eval var e)

class Semiring d where
    zero  :: d
    one   :: d
    (⊕)   :: d -> d -> d
    (⊗)   :: d -> d -> d

instance Num a => Semiring a where
    zero = 0
    one  = 1
    (⊕) = (+)
    (⊗) = (*)

class MMonoid e where
    z :: e
    (➕)  :: e -> e -> e

class (Semiring d, MMonoid e) => Module d e | e -> d where
    (•)  :: d -> e -> e

class (Module d e) => Kronecker v d e where
    delta  :: v -> e

data d ⋉ e = N {primary :: d , tangent :: e} deriving (Show)

instance Functor ((⋉) d) where
    fmap h (N f df) = N f (h df)

instance (Module d e) => Semiring (d ⋉ e) where
    zero = N zero z
    one  = N one z
    (N f df) ⊕ (N g dg) = N (f ⊕ g) (df ➕ dg) -- how does it resolve <> ?
    (N f df) ⊗ (N g dg) = N (f ⊗ g) ((f • dg) ➕ (g • df))

-- abstractD :: (Kronecker v d e) => (v -> d) -> v -> (Expr v) -> (d ⋉ e)
-- abstractD var x0 = eval gen where
--     gen x = N (var x) (delta x)


instance (Floating d, Ord v) => (MMonoid (Map v d)) where
    z = Data.Map.empty
    (➕) = unionWith (+)

instance (Floating d, Eq v, Ord v) => Module d (Map v d) where
    t • m = Data.Map.map (* t) m

instance (Floating d, Eq v, Ord v) => Kronecker v d (Map v d) where
    delta x0 = Data.Map.singleton x0 one

instance (Ord v, Ord d, Num d) => Num (d ⋉ (Map v d)) where
    fromInteger n   = N (fromInteger n) empty
    N x dx + N y dy = N (x + y) (unionWith (+) dx dy)
    N x dx - N y dy = N (x - y) (unionWith (+) dx (fmap negate dy))
    N x dx * N y dy = N (x * y) (unionWith (+) (fmap (y *) dx) (fmap (x *) dy))
    negate (N x dx) = N (negate x) (fmap negate dx)
    abs (N x dx) = N (abs x) (if x >= 0 then dx else fmap negate dx)
    signum = error "undefined"

instance (Ord v, Ord d, Fractional d) => Fractional (d ⋉ (Map v d)) where
    fromRational r  = N (fromRational r) empty 
    recip (N x dx)  = N (recip x) (fmap ((-x * x)*) dx) 
    N x dx / N y dy = N (x / y) (let z = y * y in unionWith (+) (fmap ((y / z) *) dx) (fmap ((-x / z) *) dy))

instance (Ord v, Ord d, Floating d) => Floating (d ⋉ (Map v d)) where
    pi = N pi empty
    exp (N x dx) = N (exp x) (fmap ((exp x) *) dx)
    log (N x dx) = N (log x) (fmap ((recip x) *) dx)
    sqrt (N x dx) = N (sqrt x) (fmap ((recip (2 * sqrt x)) *) dx)
    -- N x dx ** N y dy  = error "undefined"
    -- logBase x y = error "undefined"
    sin (N x dx) = N (sin x) (fmap ((cos x) *) dx) 
    cos (N x dx) = N (cos x) (fmap ((negate $ sin x) *) dx) 
    tan (N x dx) = N (tan x) (fmap ((recip $ cos x ** 2) *) dx)
    asin = error "undefined"
    acos = error "undefined"
    atan = error "undefined"
    sinh = error "undefined"
    cosh = error "undefined"
    tanh = error "undefined"
    asinh = error "undefined"
    acosh = error "undefined"
    atanh = error "undefined"

forwardAD :: (Floating d, Eq v, Ord v, Ord d) => (v -> d) -> v -> (Expr v) -> (d ⋉ (Map v d))
forwardAD var x0 = eval gen where
    gen x = N (var x) (delta x)

-- data Gaussian a = Gaussian {mu :: a , sigma :: a} deriving (Show)

-- logGaussian1D :: Floating a => Gaussian a -> a -> a
-- logGaussian1D (Gaussian mu sigma) x = -(x - mu) * (x - mu) / (2*sigma*sigma) + (-1/2) * log (2 * pi * sigma)

-- main :: IO ()
-- main = do
--     let f = (Var X) * ((Var X) + 1) :: (Expr Vars)
--     let x0 = 4
--     let res = (forwardAD (const x0) X f :: Double ⋉ (Map Vars Double))
--     print $ "The numeric derivative at x0=" ++ show x0
--     print res

-- main :: IO ()
-- main = do
--     let f = logGaussian1D (Gaussian 0 1) (Var X) :: (Expr Vars)
--     let x0 = (Var X)
--     let res = (forwardAD (const x0) X f :: (Expr Vars) ⋉ (Map Vars (Expr Vars)))
--     print $ "The numeric derivative at x0=" ++ show x0
--     print res
