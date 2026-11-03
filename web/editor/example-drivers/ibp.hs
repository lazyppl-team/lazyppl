features :: Int -> IO ()
features n = do
  draws <- mhirreducible 0.2 0.01 (additiveClustering 2 6 0.3 similarityData)
  let assignment = maxap (take n draws)
      dishes = nub (concat $ Data.Array.elems assignment)
  forM_ (zip [0::Int ..] countries) $ \(i,country) -> do
    putStrLn $ intercalate "\t" ["label",show i,show country]
    putStrLn $ intercalate "\t" (show i : [if dish `elem` (assignment Data.Array.! country) then "1" else "0" | dish <- dishes])

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  features 10000
