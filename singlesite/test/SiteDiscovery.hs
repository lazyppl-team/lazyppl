module Main where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)
import GHC.Exts.Heap (Closure, GenClosure(..), asBox, getBoxedClosureData)
import GHC.Stats (getRTSStats, gcs)
import LazyPPL (Tree(..), randomTree)
import qualified LazyPPL.SingleSite.Internal as S
import System.Mem (performGC)
import System.Random (mkStdGen)
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

at :: [Int] -> Tree -> Double
at [] (Tree x _) = x
at (i:is) (Tree _ ts) = at is (ts !! i)

-- Compare the actual traversal with GHC's uncached decoder on the same
-- evaluated tree. Both traversals run without an intervening collection.
sameSites :: Property
sameSites = forAll arbitrary $ \seed ->
  forAll (listOf1 (resize 4 (listOf (chooseInt (0,5))))) $ \paths -> ioProperty $ do
    let tree = randomTree (mkStdGen seed)
    forM_ paths (evaluate . (`at` tree))
    performGC
    before <- getRTSStats
    cached <- S.trunc tree >>= evaluate . force . S.getSites
    plain <- S.helperT getBoxedClosureData (asBox tree) >>= evaluate . force . S.getSites
    after <- getRTSStats
    pure $ counterexample (show (seed,paths,cached,plain,gcs before,gcs after)) $
      cached === plain .&&. gcs before === gcs after

-- A tree node evaluated to its constructor, with its label and its children
-- both unevaluated. The WebAssembly build meets such nodes in the clustering
-- demo; each field here is a call to a function that is not inlined, so it
-- stays a thunk. Nothing at or below the node was demanded, so the traversal
-- finds no site.
{-# NOINLINE opaqueLabel #-}
opaqueLabel :: Int -> Double
opaqueLabel k = fromIntegral k / 7

{-# NOINLINE opaqueChildren #-}
opaqueChildren :: Int -> [Tree]
opaqueChildren k = [lazyNode (k + 1)]

{-# NOINLINE lazyNode #-}
lazyNode :: Int -> Tree
lazyNode k = Tree (opaqueLabel k) (opaqueChildren k)

isThunk :: Closure -> Bool
isThunk ThunkClosure {} = True
isThunk _ = False

unevaluatedNode :: IO Bool
unevaluatedNode = do
  node@(Tree label children) <- evaluate (lazyNode 3)
  shapes <- mapM getBoxedClosureData [asBox label, asBox children]
  cached <- try (S.trunc node >>= evaluate . force . S.getSites)
  plain <- try (S.helperT getBoxedClosureData (asBox node) >>= evaluate . force . S.getSites)
  let shown = either (\e -> "an error: " ++ show (e :: SomeException)) show
      ok = all isThunk shapes && either (const False) null cached && either (const False) null plain
  putStrLn $ (if ok then "PASS" else "FAIL") ++ " a node with an unevaluated label and children has no site: fields "
    ++ show (map isThunk shapes) ++ " thunks; trunc gives " ++ shown cached ++ "; helperT gives " ++ shown plain
  pure ok

main :: IO ()
main = do
  unevaluated <- unevaluatedNode
  result <- quickCheckWithResult stdArgs {maxSuccess=200,replay=Just(mkQCGen 20261026,0)} sameSites
  unless unevaluated (fail "A node with an unevaluated label and children was not traversed")
  unless (isSuccess result) (fail "Cached and uncached traversal differ without intervening GC")
