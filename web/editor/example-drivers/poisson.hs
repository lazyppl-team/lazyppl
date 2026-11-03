-- The playground also shows the sampled point-process trace.
main :: IO ()
main = do
  seed <- seedFromArgs
  setStdGen (mkStdGen seed)
  runPoissonSim
