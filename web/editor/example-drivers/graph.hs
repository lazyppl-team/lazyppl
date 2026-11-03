graph :: Int -> IO ()
graph n = do
  draws <- mh 0.1 exampleA
  case drop (n-1) draws of
    (((matches,theta),(adjacency,_)),_):_ -> do
      putStrLn $ intercalate "\t" ["graph_stat",show matches,show theta]
      forM_ (zip [0::Int ..] adjacency) $ \(i,row) ->
        putStrLn $ intercalate "\t" (show i : [if edge then "1" else "0" | edge <- row])
    [] -> fail "Graph inference returned no samples"

main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  graph 5000
