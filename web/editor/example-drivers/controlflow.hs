main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  a <- mh 0.3 (model prior1)
  b <- mh 0.33 (model prior2)
  print (fromIntegral (length (filter fst (take 10000 a))) / 10000 :: Double)
  print (fromIntegral (length (filter fst (take 10000 b))) / 10000 :: Double)
