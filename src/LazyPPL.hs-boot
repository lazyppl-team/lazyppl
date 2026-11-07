module LazyPPL where

import Control.Monad.Trans.Writer
import Data.Monoid
import Numeric.Log

data Tree = Tree Double [Tree]
newtype Prob a = Prob (Tree -> a)
newtype Meas a = Meas (WriterT (Product (Log Double)) Prob a)

instance Monad Prob
instance Functor Prob
instance Applicative Prob
