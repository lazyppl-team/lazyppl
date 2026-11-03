main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  do { draws <- mh 0.2 example; curves dataset (map fst $ take 5 $ every 1000 $ drop 10000 draws) }
