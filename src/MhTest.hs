{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MhTest where

import Data.List
import Distr
import Graphics.Matplotlib hiding (density)
import LazyPPL
import Statistics.Distribution
import Statistics.Distribution.Beta

-- | normal distribution reparameterized so that it uses n seeds.
-- | This causes (mh p) to behave like a random-walk Metropolis-Hastings,
nnormal n mu sigma =
  do
    xs <- mapM (\_ -> normal (mu / n) $ sqrt (sigma ^ 2 / n)) [1 .. n]
    return $ sum xs

-- | a simple model that has a nasty bimodal posterior
bimodal prior = do
  x <- sample prior
  score $ normalPdf (-15) 2 x + normalPdf 15 2 x
  return x

-- | to test, run model for time n with different parameters
-- | typically "test (bimodal (nnormal 10 0 4)) 10000"
-- | or        "test (bimodal (nnormal 10 0 4)) 300" to see the random walk
-- | or        "test (sample $ nnormal 10 0 4) 10000" to check the priors are ok
test model n = do
  let ys = map fromIntegral [1 .. n]
  let ideas =
        [ (1, 0, "mh 1", "tab:blue"),
          (0.5, 0, "mh 0.5", "tab:green"),
          (0.1, 0.1, "mh 0.2 mixed with mh 1", "tab:red")]
  xws <- mapM (\(p, q, _, _) -> mhirreducible p q model) ideas
  let xs = map (map fst . take n) xws
  onscreen $
    histscatters
      xs
      ys
      (map (\(_, _, s, _) -> s) ideas)
      (map (\x -> "duplicates: " ++ show (n - length (nub $ sort x))) xs)
      (map (\(_, _, _, c) -> c) ideas)

-- Plotting routine
histscatters xs y titles xtitles colors =
  foldl histscatter (subplots @@ [o2 "nrows" 2, o2 "ncols" (length xs)]) [0 .. (length xs - 1)]
  where
    histscatter' mpl i =
      mpl
        % setSubplot i
        % histogram (xs !! i) (bins i) @@ [o2 "color" (colors !! i)]
        % mp # "ax.xaxis.set_major_formatter(mticker.NullFormatter())"
        % title (titles !! i)
        % setSubplot (i + n)
        % scatter (xs !! i) y @@ [o2 "s" 0.5, o2 "c" (colors !! i)]
        % xlabel (xtitles !! i)
    binwidth = 0.1
    xmax i = maximum (xs !! i) + 0.5
    xmin i = minimum (xs !! i) -0.5
    bins i = [xmin i, xmin i + binwidth .. (xmax i + binwidth)]
    n = length xs
    histscatter mpl i =
      if i == 0 
        then histscatter' mpl i % ylabel "time" 
        else histscatter' mpl i % mp # "ax.yaxis.set_major_formatter(mticker.NullFormatter())"
