relational :: Int -> IO ()
relational n = do
  draws <- take n <$> mh 0.2 model
  let people = [minBound..maxBound :: Person]
  forM_ (zip [0::Int ..] people) $ \(i,a) -> do
    putStrLn $ intercalate "\t" ["label",show i,show a]
    let probabilities = [fromIntegral (length [() | (table,_) <- draws,table a==table b])/fromIntegral n :: Double | b <- people]
    putStrLn $ intercalate "\t" (show i : map show probabilities)

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  relational 1000
