{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module ReverseSupport (reverseSupport) where

import Control.DeepSeq (NFData(..))
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.IORef
import LazyPPL (Prob(..), Tree(..), sample)
import LazyPPL.SingleSite (mh1)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)
import System.Random (mkStdGen, setStdGen)

-- We force the unused root label once, to make the root a site. A root
-- proposal leaves the new label lazy, so the proposed tree has no root
-- site (no reverse support).
data Observation = Observation Double Tree (IORef Bool)

instance NFData Observation where
  rnf (Observation value (Tree root _) first) = unsafePerformIO $ do
    _ <- evaluate (rnf value)
    initial <- atomicModifyIORef' first (\b -> (False, b))
    if initial then evaluate root >> pure () else pure ()

observe :: IORef Bool -> Tree -> Observation
observe first tree@(Tree _ (Tree value _ : _)) = Observation value tree first
observe _ _ = error "random tree has no first child"
{-# NOINLINE observe #-}

valueOf :: Observation -> Double
valueOf (Observation value _ _) = value

reverseSupport :: IO Bool
reverseSupport = do
  results <- forM [1..16] $ \seed -> do
    setStdGen (mkStdGen seed)
    first <- newIORef True
    chain <- mh1 (sample (Prob (observe first)))
    snapshots <- forM (take 64 chain) $ \(draw, _) -> do
      _ <- evaluate (rnf draw)
      identity <- makeStableName =<< evaluate draw
      pure (valueOf draw, identity)
    let pairs = zip snapshots (drop 1 snapshots)
        unsupported = length [() | ((x, a), (y, b)) <- pairs, x == y, a /= b]
        moves = length [() | ((x, _), (y, _)) <- pairs, x /= y]
    putStrLn $ "reverse support seed=" ++ show seed
      ++ " accepted unused-root proposals=" ++ show unsupported
      ++ " demanded-site moves=" ++ show moves
    pure (unsupported, moves)
  -- Rejecting every proposal would pass the unused-root check, so each chain
  -- must also move.
  pure (all ((== 0) . fst) results && all ((> 0) . snd) results)
