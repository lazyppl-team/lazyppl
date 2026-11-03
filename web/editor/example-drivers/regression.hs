regression :: Double -> StdGen -> IO ()
regression sigma predictiveGen = do
  samples <- mh 0.5 (regress sigma linear dataset)
  let fs = map fst $ take 1000 $ every 50 $ drop 100 samples
      grid = [fromIntegral i / 5 | i <- [0..30 :: Int]]
      predictions = runProb (mapM (\f -> mapM (\x -> normal (f x) sigma) grid) fs) (randomTree predictiveGen)
  forM_ dataset $ \(x,y) -> putStrLn $ "observation\t" ++ show x ++ "\t" ++ show y
  putStrLn "draw\tf(0)\tf(6)\tpredictive values at x=0,0.2,...,6"
  forM_ (zip3 [0..999 :: Int] fs predictions) $ \(i,f,ys) ->
    putStrLn $ intercalate "\t" (show i : map show (f 0 : f 6 : ys))

main :: IO ()
main = do
  seed <- seedFromArgs
  let (inferenceGen, predictiveGen) = splitGen (mkStdGen seed)
  setStdGen inferenceGen
  regression 0.5 predictiveGen
