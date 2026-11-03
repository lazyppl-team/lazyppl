mondrian :: Int -> Double -> IO ()
mondrian seed budget = do
  let partition = runProb (randomMondrian uniform budget [(0,1),(0,1)]) (randomTree $ mkStdGen seed)
      blocks (Block p [(a,b),(c,d)]) = [[a,b,c,d,p]]
      blocks (Partition _ _ _ left right) = blocks left ++ blocks right
      blocks _ = error "Expected a two-dimensional Mondrian partition"
  forM_ (zip [0::Int ..] (blocks partition)) $ \(i,row) ->
    putStrLn $ intercalate "\t" (show i : map show row)

main :: IO ()
main = do
  seed <- seedFromArgs
  mondrian seed 5
