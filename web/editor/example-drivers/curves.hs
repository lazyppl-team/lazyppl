curves :: [(Double,Double)] -> [Double -> Double] -> IO ()
curves observations fs = do
  let grid = [fromIntegral i/5 | i <- [0..30::Int]]
  forM_ observations $ \(x,y) -> putStrLn $ intercalate "\t" ["observation",show x,show y]
  forM_ (zip [0::Int ..] fs) $ \(i,f) ->
    putStrLn $ intercalate "\t" (show i : map (show . f) grid)
