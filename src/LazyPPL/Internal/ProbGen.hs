{-# LANGUAGE MultiParamTypeClasses #-}

{- | The stock 'StatefulGen' view of 'LazyPPL.Prob', used by 'LazyPPL.wis' to
resample through the bounded samplers of @mwc-random@. This module is internal
and not exposed, so the 1.1.0 interface of the exposed modules is unchanged:
the instance below mentions only the internal 'ProbGen' and the exported
'Prob', and haddock lists it under this hidden module, not under 'LazyPPL'. -}
module LazyPPL.Internal.ProbGen (ProbGen(..)) where

import Control.Monad.ST (runST)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word64)
import {-# SOURCE #-} LazyPPL (Prob(..), Tree(..))
import System.Random.Stateful (StatefulGen(..), fillByteArrayST)

{- | A stateful generator view of a probability computation, for the stock
sampling algorithms of @random@ and @mwc-random@ that take a 'StatefulGen',
such as 'System.Random.MWC.Distributions.logCategorical'.

Precondition: the tree must provide a child for every requested word; the
library's trees always do (a hand-built tree that runs out of children raises
\"LazyPPL: random tree exhausted\").

'LazyPPL.Internal.Sampler.randomLabel' puts every label on the midpoint grid
(k + 1/2) / 2^52 with k a uniform 52-bit integer, and 'labelIndex' recovers k
exactly, so each label supplies 52 uniform bits. 'uniformWord64' joins the
label at its tree node with the label of the node's first child — a subtree
that 'Prob' sequencing reserves to this primitive — into 64 uniform bits; the
smaller word methods use one label. Every request a stock algorithm makes,
including the repeated draws of its rejection loops, reads a distinct tree
coordinate under the monad structure of 'Prob', and running the same
computation on the same tree gives the same words. -}
data ProbGen = ProbGen

-- The 52-bit grid index of a label, the k of the midpoint grid
-- u = (k + 1/2) / 2^52 of 'LazyPPL.Internal.Sampler.randomLabel'. It is exact
-- for every label 'randomLabel' draws: u * 2^52 = k + 1/2 is exactly
-- representable in binary64, and floor (k + 1/2) is k. Labels of hand-built
-- trees keep the index of the grid point at or below them.
labelIndex :: Double -> Word64
labelIndex u = fromInteger (floor (u * 4503599627370496))

instance StatefulGen ProbGen Prob where
  uniformWord64 ProbGen = Prob $ \(Tree r ts) -> case ts of
    (Tree c _ : _) -> (labelIndex r `shiftL` 12) .|. (labelIndex c `shiftR` 40)
    [] -> treeExhausted
  {-# INLINE uniformWord64 #-}
  uniformWord32 ProbGen = Prob $ \(Tree r _) -> fromIntegral (labelIndex r `shiftR` 20)
  {-# INLINE uniformWord32 #-}
  -- The class default delegates to a frozen generator (System.Random.Internal
  -- 1.3.1, uniformByteArrayM's default signature), which a tree coordinate has
  -- no notion of. The stock fillByteArrayST (System.Random.Stateful, under
  -- "Helper functions for creating instances") fills the array little-endian
  -- from successive words, drawn here from successive children through an
  -- STRef; the empty list is matched explicitly, so no byte stays
  -- uninitialised.
  uniformByteArrayM isPinned n ProbGen = Prob $ \(Tree _ gs) -> runST $ do
    ref <- newSTRef gs
    fillByteArrayST isPinned n $ do
      ts <- readSTRef ref
      case ts of
        (t : ts') -> do
          writeSTRef ref ts'
          pure (treeWord t)
        [] -> treeExhausted
  {-# INLINE uniformByteArrayM #-}

-- One clear error when a hand-built tree runs out of children.
treeExhausted :: a
treeExhausted = error "LazyPPL: random tree exhausted"
{-# NOINLINE treeExhausted #-}

-- One tree word per node, as uniformWord64 makes it.
treeWord :: Tree -> Word64
treeWord (Tree r ts) = case ts of
  (Tree c _ : _) -> (labelIndex r `shiftL` 12) .|. (labelIndex c `shiftR` 40)
  [] -> treeExhausted
{-# INLINE treeWord #-}
