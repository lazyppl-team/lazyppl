module CodedMessage where

import Distr
import LazyPPL

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.IO
import System.IO.Unsafe

import Data.Aeson

import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy as LB (readFile)


{- | Decoding a coded message (substitution cipher)
    Inpired from https://math.uchicago.edu/~shmuel/Network-course-readings/MCMCRev.pdf -}

{- | Create a Map of first-order letter transitions from a file containing a corpus of English words.
'M !! (c1, c2)' = probability of character c2 following character c1 in an English text. 
-}
transitionMap :: String -> IO (Map.Map (Char, Char) Double)
transitionMap file = do
  contents <- readFile file
  let unnormalisedMap = foldl updateMap Map.empty $ words contents
  -- let keysGroupedByFirstLetter = Data.List.groupBy (\a b -> fst a == fst b) $ keys unnormalisedMap
  let normalisedMap =
        Set.foldl (\m c ->
          let sumOccurences = sum $ Map.elems $ Map.filterWithKey (\k _ -> fst k == c) unnormalisedMap
          in Map.mapWithKey 
          (\(c1, c2) count -> if c1 == c then count / sumOccurences else count) m)
          unnormalisedMap 
          $ Map.keysSet $ Map.mapKeys fst unnormalisedMap
  return normalisedMap
  where
    updateMap :: Map.Map (Char, Char) Double -> String -> Map.Map (Char, Char) Double
    updateMap m s =
      foldl (\m' (c1, c2) ->
        let count = Map.findWithDefault 0 (c1, c2) m'
        in Map.insert (c1, c2) (count + 1) m)
        m $ zip s (tail s)


-- English corpus: https://github.com/first20hours/google-10000-english/blob/master/google-10000-english.txt
tMap :: IO (Map.Map (Char, Char) Double)
tMap = transitionMap "../english-words-10000.txt"

saveTransitionMap :: IO ()
saveTransitionMap = do
  tMap' <- tMap
  encodeFile "../english-transition-map.json" tMap'

loadTransitionMap :: IO (Map.Map (Char, Char) Double)
loadTransitionMap = do
  contents <- LB.readFile "../english-transition-map.json"
  return $ fromJust $ decode contents


