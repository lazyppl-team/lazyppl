{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MinimalDemo
import qualified RegressionDemo as R
import qualified GaussianProcessDemo as GP
import qualified WienerDemo as W
import qualified ClusteringDemo as C
import qualified IrmDemo as I
import qualified ProgramInductionDemo as P
import qualified AdditiveClusteringDemo as A
import qualified GraphDemo as G
import qualified LazyPPL.Distributions.Mondrian as M
import qualified Data.Array as Array
import LazyPPL.SingleSite (mh1)
import qualified LazyPPL.SingleSite.Internal as Sites
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (nub, sort, group)
import Data.Monoid (Product(..))
import Data.Aeson (Value, object, (.=), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BL
import LazyPPL (sample, Prob(..), Tree(..))
import LazyPPL (mh, mhirreducible, every, runProb, randomTree,uniform)
import LazyPPL.Distributions (normal)
import GHC.Clock (getMonotonicTimeNSec)
import Numeric.Log (Log, ln)
import System.Environment (getArgs)
import System.Random (mkStdGen, setStdGen, splitGen, StdGen)
import Text.Read (readMaybe)

data Setting = Setting Double Double Double Double Double

settings :: [(String, Maybe Setting)]
settings = [("poisson",Nothing), ("regression",Just (Setting 0.1 3 0.1 0.5 0.5)),
  ("piecewise",Just (Setting 1 1000 1 5 1000)),
  ("gp",Just (Setting 5 100 1 5 20)), ("wiener",Just (Setting 5 100 1 20 20)),
  ("clustering",Just (Setting 1000 50000 1000 1000 20000)),
  ("irm",Just (Setting 1000 50000 1000 1000 10000)),
  ("program",Just (Setting 1 20 1 1 3)),
  ("ibp",Just (Setting 1000 200000 1000 10000 100000)),
  ("graph",Just (Setting 1000 50000 1000 5000 20000)),
  ("mondrian",Just (Setting 0.5 10 0.5 5 5))]

settingJSON :: Maybe Setting -> Value
settingJSON Nothing = object []
settingJSON (Just (Setting lo hi step value original)) = object
  ["min" .= lo,"max" .= hi,"step" .= step,"value" .= value,"original" .= original]

emit :: Value -> IO ()
emit = BL.putStrLn . encode

valid :: String -> [String] -> Bool
valid kind parameters = case (lookup kind settings, parameters) of
  (Just Nothing, []) -> True
  (Just (Just (Setting lo hi step _ _)), [value]) -> case readMaybe value of
    Just n ->
      let ratio = (n - lo) / step
          nearest = fromInteger (round ratio)
      in not (isNaN n || isInfinite n)
         && n >= lo
         && n <= hi
         && step > 0
         && abs (ratio - nearest) <= 1e-9
    Nothing -> False
  _ -> False

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--settings"] -> emit $ object [Key.fromString kind .= settingJSON value | (kind,value) <- settings]
    kind:value:parameters | Just seed <- readMaybe value, valid kind parameters -> do
      let number = case parameters of [parameter] -> read parameter; _ -> 0 :: Double
          count = round number
      setStdGen (mkStdGen seed)
      timed $ case kind of
        "poisson" -> poisson
        "regression" -> do
          let (inferenceGen,predictiveGen) = splitGen (mkStdGen seed)
          setStdGen inferenceGen
          regression number predictiveGen
        "piecewise" -> piecewise count
        "gp" -> do
          draws <- mh 0.2 GP.example
          curves GP.dataset (take count $ every 1000 $ drop 10000 draws)
        "wiener" -> do
          draws <- mh 0.1 W.example
          curves W.dataset (take count $ every 1000 $ drop 10000 draws)
        "clustering" -> clustering count
        "irm" -> relational count
        "program" -> induction count
        "ibp" -> features count
        "graph" -> graph count
        "mondrian" -> mondrian seed number
        _ -> fail "Unknown model"
    _ -> fail "Expected a model, integer seed and parameter within --settings bounds"

poisson :: IO ()
poisson = do
  draws <- mh 1 $ sample $ Prob $ \tree -> (runProb (MinimalDemo.poissonSim 1) tree, tree)
  case draws of
    (((count,tree),_):_) -> do
      let arrivals = runProb (do xs <- MinimalDemo.poissonPP 0 1; pure xs) tree
          (points,rest) = span (<1) arrivals
      case rest of
        next:_ -> do
          _ <- evaluate (force (count,points,next))
          partial <- Sites.trunc tree
          let addresses = map reverse (Sites.getSites partial)
              label (Tree u _) [] = u
              label (Tree _ children) (i:is) = label (children !! i) is
          emit $ object ["kind" .= ("trace" :: String), "count" .= count,
            "points" .= points, "next_point" .= next, "addresses" .= addresses,
            "uniforms" .= map (label tree) addresses]
        [] -> fail "The Poisson process ended before leaving the viewport"
    [] -> fail "Poisson sampling returned no draws"
  let groups = group $ sort $ map (fst . fst) $ take 10000 draws
  forM_ groups $ \xs -> case xs of
    x:_ -> emit $ object ["kind" .= ("sample" :: String),
      "count" .= x, "frequency" .= (fromIntegral (length xs) / 10000 :: Double)]
    [] -> pure ()

-- The natural logarithm of a retained sample's Metropolis-Hastings weight: the likelihood of the observations,
-- because these demos score only observations and no prior term.
logLikelihood :: Product (Log Double) -> Double
logLikelihood = ln . getProduct

observation :: (Double,Double) -> IO ()
observation (x,y) = emit $ object ["kind" .= ("observation" :: String), "x" .= x, "y" .= y]

timed :: IO () -> IO ()
timed action = do
  start <- getMonotonicTimeNSec
  action
  end <- getMonotonicTimeNSec
  emit $ object ["kind" .= ("timing" :: String), "elapsed_ms" .= (fromIntegral (end - start) / 1e6 :: Double)]

regression :: Double -> StdGen -> IO ()
regression sigma predictiveGen = do
  samples <- mh 0.5 (R.regress sigma R.linear R.dataset)
  let draws = take 1000 $ every 50 $ drop 100 samples
      fs = map fst draws
      grid = [fromIntegral i / 5 | i <- [0..30 :: Int]]
      predictions = runProb (mapM (\f -> mapM (\x -> normal (f x) sigma) grid) fs) (randomTree predictiveGen)
  forM_ R.dataset observation
  forM_ (zip3 [0..999 :: Int] draws predictions) $ \(i,(f,w),ys) ->
    emit $ object ["kind" .= ("sample" :: String), "draw" .= i, "f0" .= f 0, "f6" .= f 6, "predictive" .= ys,
      "log_likelihood" .= logLikelihood w]

piecewise :: Int -> IO ()
piecewise count = do
  let prior = do
        changes <- R.poissonPP 0 0.1
        f <- R.splice (pure changes) R.linear
        pure (changes,f)
      model = do
        (changes,f) <- sample prior
        _ <- R.regress 0.1 (pure f) R.dataset
        pure (changes,f)
  draws <- mhirreducible 0.2 0.1 model
  let retained = take count $ every 1000 $ drop 10000 draws
      grid = [fromIntegral i / 5 | i <- [0..30 :: Int]]
      line f lower upper =
        let a = lower + (upper - lower) / 3
            b = lower + 2 * (upper - lower) / 3
            ya = f a
            slope = (f b - ya) / (b - a)
        in object ["x_lower" .= lower, "x_upper" .= upper,
          "y_lower" .= (ya + slope * (lower - a)), "y_upper" .= (ya + slope * (upper - a))]
  forM_ R.dataset observation
  forM_ (zip [0::Int ..] retained) $ \(i,((changes,f),w)) -> do
    let visible = takeWhile (<6) changes
        bounds = 0 : visible ++ [6]
    emit $ object ["kind" .= ("sample" :: String), "draw" .= i,
      "change_points" .= visible, "pieces" .= zipWith (line f) bounds (drop 1 bounds),
      "values" .= map f grid, "log_likelihood" .= logLikelihood w]

curves :: [(Double,Double)] -> [(Double -> Double, Product (Log Double))] -> IO ()
curves observations draws = do
  let grid = [fromIntegral i/5 | i <- [0..30::Int]]
  forM_ observations observation
  forM_ (zip [0::Int ..] draws) $ \(i,(f,w)) ->
    emit $ object ["kind" .= ("sample" :: String), "draw" .= i, "values" .= map f grid,
      "log_likelihood" .= logLikelihood w]

clustering :: Int -> IO ()
clustering n = do
  draws <- take n <$> mh1 C.example
  let bestWeight = maximum (map snd draws)
  case [points | (points,weight) <- draws,weight==bestWeight] of
    points:_ -> forM_ (zip [0::Int ..] points) $ \(i,((x,y),c,(mx,my,s))) ->
      emit $ object ["kind" .= ("sample" :: String),"point" .= i,"x" .= x,"y" .= y,"cluster" .= c,"centre_x" .= mx,"centre_y" .= my,"sigma" .= s]
    [] -> fail "Clustering inference returned no samples"

relational :: Int -> IO ()
relational n = do
  draws <- take n <$> mh 0.2 I.model
  let people = [minBound..maxBound :: I.Person]
  forM_ (zip [0::Int ..] people) $ \(i,a) -> do
    let probabilities = [fromIntegral (length [() | (table,_) <- draws,table a==table b])/fromIntegral n :: Double | b <- people]
    emit $ object ["kind" .= ("sample" :: String),"person" .= i,"label" .= show a,"probabilities" .= probabilities]

induction :: Int -> IO ()
induction n = do
  draws <- mhirreducible 0.1 0.005 (P.regress 0.25 P.randfun P.dataset)
  let functions = take n $ every 1000 $ drop 1000 draws
  forM_ P.dataset observation
  forM_ (zip [0::Int ..] functions) $ \(i,((f,expression),w)) -> do
    emit $ object ["kind" .= ("expression" :: String),"program" .= i,"expression" .= expression,
      "observation_values" .= map (f . fst) P.dataset, "log_likelihood" .= logLikelihood w]
    forM_ (P.sampleFun f) $ \(x,y) ->
      emit $ object ["kind" .= ("sample" :: String),"program" .= i,"x" .= x,"y" .= y]

features :: Int -> IO ()
features n = do
  draws <- mhirreducible 0.2 0.01 (A.additiveClustering 2 6 0.3 A.similarityData)
  let assignment = A.maxap (take n draws)
      dishes = nub (concat $ Array.elems assignment)
  forM_ (zip [0::Int ..] A.countries) $ \(i,country) -> do
    emit $ object ["kind" .= ("sample" :: String),"country" .= i,"label" .= show country,
      "features" .= [if dish `elem` (assignment Array.! country) then 1 else 0 :: Int | dish <- dishes]]

graph :: Int -> IO ()
graph n = do
  draws <- mh 0.1 G.exampleA
  case drop (n-1) draws of
    (((matches,theta),(adjacency,_)),_):_ -> do
      emit $ object ["kind" .= ("graph_stat" :: String),"matches" .= matches,"theta" .= theta]
      forM_ (zip [0::Int ..] adjacency) $ \(i,row) ->
        emit $ object ["kind" .= ("sample" :: String),"vertex" .= i,"edges" .= [if edge then 1 else 0 :: Int | edge <- row]]
    [] -> fail "Graph inference returned no samples"

mondrian :: Int -> Double -> IO ()
mondrian seed budget = do
  let partition = runProb (M.randomMondrian uniform budget [(0,1),(0,1)]) (randomTree $ mkStdGen seed)
      blocks (M.Block p [(a,b),(c,d)]) = [[a,b,c,d,p]]
      blocks (M.Partition _ _ _ left right) = blocks left ++ blocks right
      blocks _ = error "Expected a two-dimensional Mondrian partition"
  forM_ (zip [0::Int ..] (blocks partition)) $ \(i,row) -> case row of
    [a,b,c,d,p] -> emit $ object ["kind" .= ("sample" :: String),"block" .= i,"x_lower" .= a,"x_upper" .= b,"y_lower" .= c,"y_upper" .= d,"value" .= p]
    _ -> fail "Expected a rectangular block"
