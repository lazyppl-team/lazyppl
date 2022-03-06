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

import Data.Foldable ( foldrM, foldlM )
import Data.IntMap (findWithDefault)
import Data.Char (toLower, isLetter)

import System.Random


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
          let sumOccurrences = sum $ Map.elems $ Map.filterWithKey (\k _ -> fst k == c) unnormalisedMap
          in Map.mapWithKey 
          (\(c1, c2) count -> if c1 == c then count / sumOccurrences else count) m)
          unnormalisedMap 
          $ Map.keysSet $ Map.mapKeys fst unnormalisedMap
  return normalisedMap
  where
    updateMap :: Map.Map (Char, Char) Double -> String -> Map.Map (Char, Char) Double
    updateMap m s =
      foldl (\m' (c1, c2) ->
        let count = Map.findWithDefault 0 (c1, c2) m'
        in Map.insert (c1, c2) (count + 1) m')
        m $ zip (' ' : s) (s ++ [' '])

saveTransitionMap :: String -> IO ()
saveTransitionMap file = do
  tMap' <- transitionMap file
  encodeFile (take (length file - 3) file ++ "json") tMap'

loadTransitionMap :: String -> IO (Map.Map (Char, Char) Double)
loadTransitionMap file = do
  contents <- LB.readFile file
  return $ fromJust $ decode contents

-- | Encode a message with a substitution (substitution cipher)
encodeMessage :: String -> Map.Map Char Char -> String
encodeMessage s m = map (\c -> Map.findWithDefault c c m) s

{- | Decode a coded message 'codedMsg' based on a transition map 'tMap' 
  from an input alphabet 'inAlphabet' 
  (to the output alphabet in which codedMsg' is written),  
  where a letter in a 'inAlphabet' corresponds to a unique output letter.
-}
decodeMessage :: Map.Map (Char, Char) Double -> [Char] -> String -> Meas String
decodeMessage tMap inAlphabet codedMsg = do
  let setCodedMsg = Set.fromList $ map toLower codedMsg
  decodedLetters <- foldrM (\c m -> 
    if Data.Char.isLetter c
      then do 
        i <- sample $ uniformdiscrete n
        return $ Map.insert c (inAlphabet !! i) m
      else do return m)
    Map.empty setCodedMsg
  let decodedMsg = map (\c -> Map.findWithDefault c c decodedLetters) codedMsg
  mapM_ (\cs -> score $ Map.findWithDefault 0 cs tMap) 
    $ zip decodedMsg (tail decodedMsg)
  return decodedMsg
  where
    n = length inAlphabet

inferenceMessage :: String -> String -> Map.Map Char Char -> IO ()
inferenceMessage tMapJson msg subst = do
  let codedMsg = encodeMessage msg subst
  tMap <- loadTransitionMap tMapJson
  let inAlphabet = nub $ concatMap (\(c1, c2) -> [c1, c2]) $ Map.keys tMap
  mws' <- mh 0.2 $ decodeMessage tMap inAlphabet codedMsg
  mws <- takeWithProgress 5000 $ every 100 $ drop 100 mws'
  let maxw = maximum $ map snd mws
  let (Just m) = Data.List.lookup maxw $ map (\(m, w) -> (w, m)) mws
  putStrLn $ "Initial message: " ++ msg
  putStrLn $ "Coded message (to decipher): " ++ codedMsg
  putStrLn $ "Decoded message: " ++ m


-- | Examples

-- English corpus: https://github.com/first20hours/google-10000-english/blob/master/google-10000-english.txt
tMapEng :: IO (Map.Map (Char, Char) Double)
tMapEng = transitionMap "../english-words.txt"

saveTransitionMapEng :: IO ()
saveTransitionMapEng = saveTransitionMap "../english-words.txt"

exampleHume1 :: String
exampleHume1 = "But the life of a man is of no greater importance to the universe than that of an oyster."

exampleHume2 :: String
exampleHume2 = "The sweetest and most inoffensive path of life leads through the avenues of science and learning; and whoever can either remove any obstructions in this way, or open up any new prospect, ought so far to be esteemed a benefactor to mankind."

exampleFeynman1 :: String
exampleFeynman1 = "I can live with doubt, and uncertainty, and not knowing. I think it's much more interesting to live not knowing than to have answers which might be wrong. I have approximate answers, and possible beliefs, and different degrees of certainty about different things, but I'm not absolutely sure of anything. There are many things I don't know anything about, such as whether it means anything to ask 'Why are we here?' I might think about it a little bit, and if I can't figure it out then I go on to something else. But I don't have to know an answer. I don't feel frightened by not knowing things, by being lost in the mysterious universe without having any purpose — which is the way it really is, as far as I can tell. Possibly. It doesn't frighten me."

exampleGrothendieck1 :: String
exampleGrothendieck1 = "Craindre l'erreur et craindre la vérité est une seule et même chose. Celui qui craint de se tromper est impuissant à découvrir. C'est quand nous craignons de nous tromper que l'erreur qui est en nous se fait immuable comme un roc. Car dans notre peur, nous nous accrochons à ce que nous avons décrété 'vrai' un jour, ou à ce qui depuis toujours nous a été présenté comme tel. Quand nous sommes mûs, non par la peur de voir s'évanouir une illusoire sécurité, mais par une soif de connaître, alors l'erreur, comme la souffrance ou la tristesse, nous traverse sans se ﬁger jamais, et la trace de son passage est une connaissance renouvelée."

randomSubstitution :: String -> [Char] -> IO (Map.Map Char Char)
randomSubstitution msg outAlphabet = do
  foldlM (\m c -> do
    if Data.Char.isLetter c 
      then do 
        i <- getStdRandom $ randomR (0, n-1)
        return $ Map.insert c (outAlphabet !! i) m
      else do return m)
    Map.empty $ Set.fromList msg
  where
    n = length outAlphabet


main :: IO ()
main = do
  -- saveTransitionMapEng
  let outAlphabet = ['a'..'z']
  subst <- randomSubstitution exampleHume1 outAlphabet
  inferenceMessage "../english-words.json" exampleHume1 subst