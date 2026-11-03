main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  do { draws <- mh 0.1 example; curves dataset (map fst $ take 20 $ every 1000 $ drop 10000 draws) }
