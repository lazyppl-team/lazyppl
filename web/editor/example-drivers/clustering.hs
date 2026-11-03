clustering :: Int -> IO ()
clustering n = do
  draws <- take n <$> mh1 example
  let bestWeight = maximum (map snd draws)
  case [points | (points,weight) <- draws,weight==bestWeight] of
    points:_ -> forM_ (zip [0::Int ..] points) $ \(i,((x,y),c,(mx,my,s))) ->
      putStrLn $ intercalate "\t" (show i : map show [x,y,c,mx,my,s])
    [] -> fail "Clustering inference returned no samples"

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  clustering 1000
