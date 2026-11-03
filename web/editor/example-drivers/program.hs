induction :: Int -> IO ()
induction n = do
  draws <- mhirreducible 0.1 0.005 (regress 0.25 randfun dataset)
  let functions = map fst $ take n $ every 1000 $ drop 1000 draws
  forM_ dataset $ \(x,y) -> putStrLn $ intercalate "\t" ["observation",show x,show y]
  forM_ (zip [0::Int ..] functions) $ \(i,(f,expression)) -> do
    putStrLn $ intercalate "\t" ["expression",show i,expression]
    forM_ (sampleFun f) $ \(x,y) ->
      putStrLn $ intercalate "\t" [show i,show x,show y]

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  induction 1
