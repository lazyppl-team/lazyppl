{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (liftM, unless, forM, forM_)
import Control.Monad.State.Lazy (State, get, put, runState)
import Control.Monad.Trans.Writer (runWriterT)
import Data.Monoid (Product(..))
import GHC.Float (castDoubleToWord64)
import LazyPPL
import LazyPPL.Internal.Sampler (randomLabel)
import Numeric.Log (Log, ln)
import System.Random hiding (uniform)
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Random (mkQCGen)

-- The tree-keeping transition and lazy State traversal are from
-- 8168aff:src/LazyPPL.hs. They do not use the replay history or checkpoints.
reference :: forall a. Double -> Maybe Double -> Meas a -> IO [(a, Product (Log Double))]
reference p refresh (Meas m) = do
  g <- newStdGen
  let (g1,g2) = split g
      t = randomTree g1
      (x,w) = runProb (runWriterT m) t
      (samples,_) = runState (iterateM step (t,x,w)) g2
  pure (map (\(_,v,weight) -> (v,weight)) samples)
  where
    step :: (Tree,a,Product (Log Double)) -> State StdGen (Tree,a,Product (Log Double))
    step (t,x,w) = do
      g <- get
      let (g1,g2) = split g
          t' = case refresh of
            Nothing -> mutate p g1 t
            Just q -> let (r,g1') = random g1
                      in if r < q then randomTree g1' else mutate p g1' t
          (x',w') = runProb (runWriterT m) t'
          ratio = getProduct w' / getProduct w
          (r,g2') = random g2
      put g2'
      if r < min 1 (exp (ln ratio)) then pure (t',x',w') else pure (t,x,w)
    iterateM f x = do
      x' <- f x
      (x':) `liftM` iterateM f x'

mutate :: Double -> StdGen -> Tree -> Tree
mutate p g (Tree a ts) =
  let (a',g') = randomLabel g
      (a'',g'') = randomLabel g'
      children seed (t:rest) = let (g1,g2) = split seed
                              in mutate p g1 t : children g2 rest
      children _ [] = []
  in Tree (if a' < p then a'' else a) (children g'' ts)

at :: [Int] -> Tree -> Double
at [] (Tree x _) = x
at (i:is) (Tree _ ts) = at is (ts !! i)

model :: Int -> Meas ([Int] -> Double)
model kind = do
  tree <- sample (Prob id)
  let root = at [] tree
      branch = if root < 0.5 then [0,2] else [2,0]
  case kind of
    0 -> pure ()
    1 -> score (if root < 0.3 then 1 else 1e-12)
    _ -> score (0.01 + at branch tree)
  pure (\path -> if path == [99] then error "delayed payload" else at path tree)

chainProperty :: Property
chainProperty = forAll arbitrary $ \seed ->
  forAll (elements [0,0.001,0.2,1]) $ \rate ->
  forAll (elements [Nothing,Just 0.05,Just 1]) $ \refresh ->
  forAll (chooseInt (0,2)) $ \kind ->
  forAll (elements [33,97,257,4101]) $ \count -> ioProperty $ do
    let measure = model kind
        sampler = maybe (mh rate) (mhirreducible rate) refresh
        paths = [[],[0],[1,0],[0,2],[2,0],[5,3,1]]
        observe (f,w) = map (castDoubleToWord64 . f) paths ++ [castDoubleToWord64 (ln (getProduct w))]
    setStdGen (mkStdGen seed)
    actual <- sampler measure
    setStdGen (mkStdGen seed)
    expected <- reference rate refresh measure
    -- Advance before demanding the old payloads, including across seed-store growth.
    _ <- evaluate (snd (actual !! (count-1)))
    _ <- evaluate (snd (expected !! (count-1)))
    let indices = [0 .. min 256 (count-1)] ++ [count-1]
    observed <- evaluate (force (map (observe . (actual !!)) indices))
    oracle <- evaluate (force (map (observe . (expected !!)) indices))
    failures <- forM [actual,expected] $ \xs -> do
      outcome <- try (evaluate (fst (head xs) [99])) :: IO (Either SomeException Double)
      pure (either (const True) (const False) outcome)
    pure $ counterexample (show (seed,rate,refresh,kind,count)) $
      observed === oracle .&&. failures === [True,True]

main :: IO ()
main = do
  forM_ [Nothing, Just 0.05, Just 1] $ \refresh -> do
    let sampler = maybe (mh 0.2) (mhirreducible 0.2) refresh
    forM_ [sampler, reference 0.2 refresh] $ \run -> do
      xs <- run (pure (error "undemanded payload" :: Bool))
      count <- evaluate (length (take 128 xs))
      unless (count == 128) (fail "Sampler forced an undemanded payload")
      bad <- run (score (error "demanded score") >> pure True)
      outcome <- try (evaluate (head bad)) :: IO (Either SomeException (Bool,Product (Log Double)))
      unless (either (const True) (const False) outcome) (fail "Sampler hid a score exception")
  result <- quickCheckWithResult stdArgs {maxSuccess=200, replay=Just (mkQCGen 20261026,0)} chainProperty
  unless (isSuccess result) (fail "Seeded chains differ from the tree-keeping sampler")
