module InfiniteWalk where

import LazyPPL
import Distr
import Control.Monad.Extra
import Control.Monad.Loops

step :: (Double, Double) -> Prob (Double, Double)
step (pos,dis) = do x <- uniform
                    let delta = x * 2.0 - 1.0
                    return (pos + delta, dis + abs delta)

walk :: Prob (Double, Double)
walk = do x <- uniform
          let start = x * 3.0
          distance <- snd <$> iterateUntilM ((<0) . fst) step (start, 0.0)
          return (start, distance)

walkObs :: Meas Double
walkObs = do (start, distance) <- sample walk
             score $ normalPdf distance 0.1 1.1
             return start

main :: IO () -- [Double]
main = do xs <- (take 60 . map fst . every 10) <$> mh 0.1 walkObs
          putStrLn $ show $ xs
