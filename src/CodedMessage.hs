{-# LANGUAGE BangPatterns #-}
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

import Control.Monad.Extra (iterateM)

import qualified Numeric.Log as L (Log( Exp), sum)
import Data.Monoid ( Product(Product, getProduct) )
import Control.Monad (forM)
import Control.DeepSeq ( deepseq )

import Debug.Trace

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

{- | Create a Map of letter frequencies based on a corpus of English words. -}
frequenciesMap :: String -> IO (Map.Map Char Double)
frequenciesMap file = do
  contents <- readFile file
  let unnormalisedMap = foldl updateMap Map.empty $ words contents
  let sumOccurrences = sum $ Map.elems unnormalisedMap
  let normalisedMap = Map.map (/ sumOccurrences) unnormalisedMap
  return normalisedMap
  where
    updateMap :: Map.Map Char Double -> String -> Map.Map Char Double
    updateMap m s =
      foldl (\m' c ->
        let count = Map.findWithDefault 0 c m'
        in Map.insert c (count + 1) m')
        m s

{- | Create a set of existing words. -}
corpusSet :: String -> IO (Set.Set String)
corpusSet file = do
  contents <- readFile file
  return $ Set.fromList $ words contents

saveTransitionMap :: String -> IO ()
saveTransitionMap file = do
  tMap' <- transitionMap file
  encodeFile (take (length file - 4) file ++ "-transition.json") tMap'

saveFrequenciesMap :: String -> IO ()
saveFrequenciesMap file = do
  fMap' <- frequenciesMap file
  encodeFile (take (length file - 4) file ++ "-frequencies.json") fMap'

loadJSONFile :: FromJSON b => FilePath -> IO b
loadJSONFile file = do
  contents <- LB.readFile file
  return $ fromJust $ decode contents

-- | Encode a message with a substitution (substitution cipher)
encodeMessage :: String -> Map.Map Char Char -> String
encodeMessage s m = map ((\c -> Map.findWithDefault c c m) . toLower) s

lettersOccurrences :: String -> [Char]
lettersOccurrences codedMsg = map fst
  $ sortOn (\(_, n) -> -n) $ Map.toList
  $ Map.fromListWith (+) [(c, 1) | c <- codedMsg]

renormalise :: Map.Map k Double -> Map.Map k Double
renormalise a =
  let sumOccurrences = sum $ Map.elems a
  in Map.map (/ sumOccurrences) a

getKey :: Double -> Map.Map Char Double -> Char
getKey val m = fromJust $ Map.foldrWithKey lookupKey Nothing m
  where
    lookupKey k v Nothing = if val == v then Just k else Nothing
    lookupKey _ _  (Just k) = Just k

accuracy :: String -> String -> Double
accuracy guessedMsg msg =
  let n = length $ words guessedMsg
      nCorrect = length $ filter (uncurry (==)) $ zip (words guessedMsg) (words msg)
  in fromIntegral nCorrect / fromIntegral n

{- | Generate a random substitution mapping each letter of 'msg' 
  to a letter of the output (coding) Alphabet 'outAlphabet'. -}
randomSubstitution :: String -> [Char] -> IO (Map.Map Char Char)
randomSubstitution msg outAlphabet = do
  (m, _, _) <- foldlM (\(m, a, n) c -> do
    if Data.Char.isLetter c
      then do
        i <- getStdRandom $ randomR (0, n - 1)
        let c' = a !! i
        return (Map.insert c c' m, delete c' a, n-1)
      else do return (m, a, n))
    (Map.empty, outAlphabet, length outAlphabet)
    $ Set.fromList $ map toLower msg
  return m

{- | Generate a random substitution of a `codedMsg` based on 
  a frequency map of each letter of the input alphabet 
  (the most common coded letter is more likely to be mapped to 
  the most common input letter, etc...). -}
sensibleSubstitution :: String -> Map.Map Char Double -> IO (Map.Map Char Char)
sensibleSubstitution codedMsg fMap = do
  let codedLettersOccurrences = lettersOccurrences $ map toLower codedMsg
  (decodedLetters, _) <- foldlM (\(m, a) c ->
    if Data.Char.isLetter c
      then do
        let frequencies = Map.elems a
        r <- randomIO
        let i = fromJust $ findIndex (>r) $ tail $ scanl (+) 0 frequencies
        let c' = getKey (frequencies !! i) a
        return (Map.insert c c' m, renormalise $ Map.delete c' a)
      else do return (m, a))
    (Map.empty, fMap) codedLettersOccurrences
  return decodedLetters

-- TODO: general map rather than substitution, and transpositions at the end
{- | Statistical model 1: Decode a coded message 'codedMsg' from scratch 
  (guess the cipher substitution)
  -- based on a transition map 'tMap', a frequency map 'fMap', 
  and corpus of words 'corpus' (set of existing words) --
  from an input alphabet 
  (to the output alphabet in which codedMsg' is written),  
  where each input letter is coded as a unique output letter. 
  The score given by the transition map is multiplied by 'transitionFactor', 
  and the score given by the number of existing words is multiplied by 'existingWordsFactor'.
-}
decodeMessageScratch :: Double -> Double -> Double ->
  Map.Map (Char, Char) Double -> Map.Map Char Double
  -> Set.Set String -> [Char] -> String -> Meas String
decodeMessageScratch transitionFactor existingWordsFactor
  lambda tMap fMap corpus codedLettersOccurrences codedMsg = do
  (decodedLetters, _) <- foldlM (\(m, a) c ->
    if Data.Char.isLetter c
      then do
        let frequencies = Map.elems a
        i <- sample $ categorical frequencies
        let c' = getKey (frequencies !! i) a
        return (Map.insert c c' m, renormalise $ Map.delete c' a)
      else do return (m, a))
    (Map.empty, fMap) codedLettersOccurrences

  let keysSubst = Map.keys decodedLetters
  let n = length keysSubst

  numberTranspositions <- sample $ poisson lambda

  decodedLetters <- iterateNtimesM (fromIntegral numberTranspositions)
    (\subst -> do
      i <- sample $ uniformdiscrete n
      j <- sample $ uniformdiscrete (n-1)
      let c1 = keysSubst !! i
          c2 = delete c1 keysSubst !! j
      return $ Map.insert c1 (fromJust $ Map.lookup c2 subst)
        $ Map.insert c2 (fromJust $ Map.lookup c1 subst) subst)
    decodedLetters

  let decodedMsg = map (\c -> Map.findWithDefault c c decodedLetters) codedMsg
      wordsDecodedMsg = words decodedMsg
      tScore = (L.Exp . (transitionFactor *)) $ foldl
        (\score (c1, c2) -> let (c1', c2') = replaceSpecialChar c1 c2 in
          if c1' == ' ' && c2' == ' ' then score
          else score + log (1 + Map.findWithDefault 0 (c1', c2') tMap))
        0 (zip (' ' : decodedMsg) (decodedMsg ++ [' ']))
        / fromIntegral (length decodedMsg + 1)
      fScore = (L.Exp . (existingWordsFactor *) . log)
        (foldl (\count w -> if Set.member w corpus then count+1 else count) 0 wordsDecodedMsg
        / fromIntegral (length wordsDecodedMsg)
        + 1)
      totalScore = L.sum [tScore, fScore]

  -- scoreLog $ trace ("Score: " ++ show totalScore) totalScore
  scoreLog totalScore
  return decodedMsg
  where
    replaceSpecialChar :: Char -> Char -> (Char, Char)
    replaceSpecialChar c1 c2 =
      let c1' = if Data.Char.isLetter c1 then c1 else ' '
          c2' = if Data.Char.isLetter c2 then c2 else ' ' in
      (c1', c2')
    iterateNtimesM :: Monad m => Int -> (a -> m a) -> a -> m a
    iterateNtimesM i _ a | i <= 0 = return a
    iterateNtimesM i f a = do
      !a' <- f a
      iterateNtimesM (i-1) f a'

{- | Statistical model 2: Decode a coded message 'codedMsg' by making transpositions 
  of the values of given a cipher substitution 'subst'
  -- based on a transition map 'tMap', a frequency map 'fMap', 
  and corpus of words 'corpus' (list of existing words) --
  from an input alphabet 
  (to the output alphabet in which codedMsg' is written),  
  where each input letter is coded as a unique output letter.
-}
substTranspose :: Map.Map (Char, Char) Double -> Map.Map Char Double ->
  Map.Map Char Char -> String -> Meas (Map.Map Char Char, String)
substTranspose tMap fMap subst codedMsg = do
  let keysSubst = Map.keys subst
  let n = length keysSubst
  i <- sample $ uniformdiscrete n
  j <- sample $ uniformdiscrete (n-1)
  let c1 = keysSubst !! i
      c2 = delete c1 keysSubst !! j
  let newSubst = Map.insert c1 (fromJust $ Map.lookup c2 subst)
        $ Map.insert c2 (fromJust $ Map.lookup c1 subst) subst
  let decodedMsg = map (\c -> Map.findWithDefault c c newSubst) codedMsg
  mapM_ (\cs -> let (c1', c2') = replaceSpecialChar cs in
    if c1' == ' ' && c2' == ' ' then return ()
    else score $ Map.findWithDefault 0 (c1', c2') tMap)
    $ zip decodedMsg (tail decodedMsg)
  return (newSubst, decodedMsg)
  where
    replaceSpecialChar (c1, c2) =
      let c1' = if Data.Char.isLetter c1 then c1 else ' '
          c2' = if Data.Char.isLetter c2 then c2 else ' ' in
      (c1', c2')

getBestMessageTranspose :: Map.Map (Char, Char) Double -> Map.Map Char Double ->
  Map.Map Char Char -> String -> IO String
getBestMessageTranspose tMap fMap subst codedMsg = do
  !scs' <- iterateNtimesM 5000 substMsgMax ((subst, codedMsg),  mempty)
  let scs = maxWeightElement scs'
  return $ snd scs
  where
    substMsgMax ((subst', codedMsg'), _) = do
      scs <- mh 0.2 $ substTranspose tMap fMap subst' codedMsg'
      let scs' = takeEagerEveryDrop 100 10 10 scs
      return $ scs' `deepseq` maxWeightPair scs'
    iterateNtimesM :: Int -> (a -> IO a) -> a -> IO [a]
    iterateNtimesM n f x = helper n n f x
      where
        helper _ i _ _ | i <= 0 = return []
        helper n i f a = do
          putStrLn $ "Progress: " ++ show (fromIntegral (100*(n-i)) / fromIntegral n) ++ "%"
          !a' <- f a
          as <- helper n (i-1) f a
          return $ a' : as

inferenceMessage :: String -> String -> Set.Set String
  -> String -> Map.Map Char Char -> IO ()
inferenceMessage tMapJson fMapJson corpus msg subst = do
  let codedMsg = encodeMessage msg subst
  tMap <- loadJSONFile tMapJson
  fMap <- loadJSONFile fMapJson
  let codedLettersOccurrences = lettersOccurrences codedMsg

  putStrLn $ "Input alphabet: " ++ show fMap

  mws' <- mh 0.2
    $ decodeMessageScratch 100 30 10 tMap fMap corpus codedLettersOccurrences codedMsg
  -- mws' <- mh1 $ decodeMessageScratch 1 4 4 tMap fMap corpus codedLettersOccurrences codedMsg
  mws <- takeProgressEveryDrop 20000 10 10 mws'
  let (maxMsg, maxWeight) = maxWeightPair mws


  -- subst' <- sensibleSubstitution codedMsg fMap
  -- maxMsg <- getBestMessageTranspose tMap fMap subst' codedMsg

  putStrLn $ "Initial message: " ++ msg ++ "\n"
  putStrLn $ "Coded message (to decipher): " ++ codedMsg ++ "\n"
  -- putStrLn $ "Initial Substitution : " ++ show (encodeMessage codedMsg subst') ++ "\n"
  putStrLn $ "Decoded message: " ++ maxMsg ++ "\n"
  putStrLn $ "Weight: " ++ show maxWeight ++ "\n"
  putStrLn $ "Accuracy: " ++ show (100 * accuracy maxMsg msg) ++ "% \n"

inferenceMessageHyperparameters :: String -> String -> Set.Set String
  -> String -> IO ()
inferenceMessageHyperparameters tMapJson fMapJson corpus msg = do
  -- Coded Feynam quote 
  let codedMsg = "a lto vamb najq sfepj, tos eolbzjtaojh, tos ofj dofnaoi. a jqaod aj'y welq wfzb aojbzbyjaoi jf vamb ofj dofnaoi jqto jf qtmb toynbzy nqalq waiqj pb nzfoi. a qtmb tggzfcawtjb toynbzy, tos gfyyapvb pbvabxy, tos saxxbzboj sbizbby fx lbzjtaojh tpfej saxxbzboj jqaoiy, pej a'w ofj tpyfvejbvh yezb fx tohjqaoi. jqbzb tzb wtoh jqaoiy a sfo'j dofn tohjqaoi tpfej, yelq ty nqbjqbz aj wbtoy tohjqaoi jf tyd 'nqh tzb nb qbzb?' a waiqj jqaod tpfej aj t vajjvb paj, tos ax a lto'j xaiezb aj fej jqbo a if fo jf yfwbjqaoi bvyb. pej a sfo'j qtmb jf dofn to toynbz. a sfo'j xbbv xzaiqjbobs ph ofj dofnaoi jqaoiy, ph pbaoi vfyj ao jqb whyjbzafey eoambzyb najqfej qtmaoi toh gezgfyb — nqalq ay jqb nth aj zbtvvh ay, ty xtz ty a lto jbvv. gfyyapvh. aj sfbyo'j xzaiqjbo wb."
  
  tMap <- loadJSONFile tMapJson
  fMap <- loadJSONFile fMapJson
  let codedLettersOccurrences = lettersOccurrences codedMsg

  -- Test different values of hyperparameters 
  -- 'transitionFactor', 'existingWordsFactor' and 'lambda' 
  -- and keep the best ones

  let numberEpochs = 10
      numberMhSteps = 10000

  let listHyperparam = [(t, e, l) | t <- [10, 20.. 1500],
        e <- [10, 20.. 1500], 
        l <- [1, 2.. 20]]
      listEpochs = [1, 2.. numberEpochs]

  hyperParamResults <- mapM (\(t, e, l) -> do
    !listResults <- mapM (\_ -> do
      mws' <- mh 0.2
        $ decodeMessageScratch t e l tMap fMap corpus codedLettersOccurrences codedMsg
      let mws = takeEager numberMhSteps mws'
          (!maxMsg, !maxWeight) = maxWeightPair mws
      return ((maxMsg, maxWeight), accuracy maxMsg msg))
      listEpochs
    return (((t, e, l), maxWeightElement listResults), averageAcc listResults))
    listHyperparam
  hyperParamResults <- takeWithProgress (length listHyperparam) hyperParamResults

  let (((t, e, l), (maxMsg, maxWeight)), acc) = maxWeightPair hyperParamResults

  putStrLn $ "Initial message: " ++ msg ++ "\n"
  putStrLn $ "Coded message (to decipher): " ++ codedMsg ++ "\n"
  putStrLn $ "Decoded message: " ++ maxMsg ++ "\n"
  putStrLn $ "Weight: " ++ show maxWeight ++ "\n\n\n"
  putStrLn $ "Best hyperparameters (transitionFactor, existingWordsFactor, λ): " ++ show (t, e, l) ++ "\n"
  putStrLn $ "Max Accuracy: " ++ show (100 * acc) ++ "% \n"
  putStrLn $ "Avg Accuracy: " ++ show (100 * accuracy maxMsg msg) ++ "% \n"
  where
    averageAcc l = 
      let (sum, len) = foldl' (\(!sum,!len) (_, i) -> (i+sum, len+1)) (0,0) l 
      in realToFrac sum / realToFrac len





-- | Examples

-- English corpus: https://github.com/first20hours/google-10000-english/blob/master/google-10000-english.txt
tMapEng :: Map.Map (Char, Char) Double
tMapEng = unsafePerformIO $ transitionMap "../english-words.txt"

corpusEng :: Set.Set String
corpusEng = unsafePerformIO $ corpusSet "../english-words.txt"

saveTransitionMapEng :: IO ()
saveTransitionMapEng = saveTransitionMap "../english-words.txt"

saveFrequenciesMapEng :: IO ()
saveFrequenciesMapEng = saveFrequenciesMap "../english-words.txt"

exampleHume1 :: String
exampleHume1 = "But the life of a man is of no greater importance to the universe than that of an oyster"

exampleHume2 :: String
exampleHume2 = "The sweetest and most inoffensive path of life leads through the avenues of science and learning; and whoever can either remove any obstructions in this way, or open up any new prospect, ought so far to be esteemed a benefactor to mankind."

exampleFeynman1 :: String
exampleFeynman1 = "I can live with doubt, and uncertainty, and not knowing. I think it's much more interesting to live not knowing than to have answers which might be wrong. I have approximate answers, and possible beliefs, and different degrees of certainty about different things, but I'm not absolutely sure of anything. There are many things I don't know anything about, such as whether it means anything to ask 'Why are we here?' I might think about it a little bit, and if I can't figure it out then I go on to something else. But I don't have to know an answer. I don't feel frightened by not knowing things, by being lost in the mysterious universe without having any purpose — which is the way it really is, as far as I can tell. Possibly. It doesn't frighten me."

exampleGrothendieck1 :: String
exampleGrothendieck1 = "Craindre l'erreur et craindre la vérité est une seule et même chose. Celui qui craint de se tromper est impuissant à découvrir. C'est quand nous craignons de nous tromper que l'erreur qui est en nous se fait immuable comme un roc. Car dans notre peur, nous nous accrochons à ce que nous avons décrété 'vrai' un jour, ou à ce qui depuis toujours nous a été présenté comme tel. Quand nous sommes mûs, non par la peur de voir s'évanouir une illusoire sécurité, mais par une soif de connaître, alors l'erreur, comme la souffrance ou la tristesse, nous traverse sans se ﬁger jamais, et la trace de son passage est une connaissance renouvelée."

-- For debugging purposes
returnScore :: Map.Map (Char, Char) Double
  -> Set.Set String -> Double -> Double -> String -> L.Log Double
returnScore tMap corpus transitionFactor existingWordsFactor msg =
  let n = length msg
      tScore = L.Exp $ (transitionFactor *) $
        foldl (\score (c1, c2) -> let (c1', c2') = replaceSpecialChar c1 c2 in
          if c1' == ' ' && c2' == ' ' then trace ("log-score: " ++ show score) score
          else
            let newScore = score +
                  log (1 + Map.findWithDefault 0 (c1', c2') tMap)
            in trace ("log-score: " ++ show newScore) newScore)
          0 (zip (' ' : msg) (msg ++ [' ']))
        / fromIntegral (n+1)
      fScore =
        (L.Exp . (existingWordsFactor *) . log) 
        (foldl (\count w -> if Set.member w corpus then count+1 else count) 0 (words msg)
        / fromIntegral (length (words msg)) + 1)
      in L.sum [trace ("tScore: " ++ show tScore) tScore, trace ("fScore: " ++ show fScore) fScore]
  where
  replaceSpecialChar :: Char -> Char -> (Char, Char)
  replaceSpecialChar c1 c2 =
    let c1' = if Data.Char.isLetter c1 then c1 else ' '
        c2' = if Data.Char.isLetter c2 then c2 else ' ' in
    (c1', c2')

f1 = map Data.Char.toLower exampleFeynman1
f2 = "i can live with doubt, and uncertainty, and not knowing. i think it's much more interesting to live not knowing than to have answers which might be wrong. i have affroximate answers, and fossible belieps, and dipperent degrees op certainty about dipperent things, but i'm not absolutely sure op anything. there are many things i don't know anything about, such as whether it means anything to ask 'why are we here?' i might think about it a little bit, and ip i can't pigure it out then i go on to something else. but i don't have to know an answer. i don't peel prightened by not knowing things, by being lost in the mysterious universe without having any furfose — which is the way it really is, as par as i can tell. fossibly. it doesn't prighten me."

s1 = returnScore tMapEng corpusEng 100 10 f1
s2 = returnScore tMapEng corpusEng 100 10 f2

main :: IO ()
main = do
  saveTransitionMapEng
  saveFrequenciesMapEng
  corpus <- corpusSet "../english-words.txt"
  let msg = map Data.Char.toLower exampleFeynman1
  
  --  let outAlphabet = ['a'..'z']
  -- subst <- randomSubstitution msg outAlphabet
  --inferenceMessage "../english-words-transition.json" "../english-words-frequencies.json" corpus msg subst

  inferenceMessageHyperparameters "../english-words-transition.json" "../english-words-frequencies.json" corpus msg