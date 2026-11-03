module Main where

import Control.Monad (unless)
import qualified GraphDemo as G

main :: IO ()
main = do
  let coordinate = 1 / sqrt 3 :: Double
      coordinates = replicate 3 coordinate
      vertex = G.SV coordinates
      opposite = G.SV (map negate coordinates)
      graph = G.SG 3 0.5
      dot = sum (zipWith (*) coordinates coordinates)
  unless (dot > 1) (fail "The normalized-vector fixture no longer exercises acos roundoff")
  unless (G.edge graph vertex vertex)
    (fail "Coincident normalized vertices must connect at a positive angle; acos roundoff lost the edge")
  unless (not (G.edge graph vertex opposite))
    (fail "Antipodal vertices must not connect at angle 0.5")
  putStrLn $ "PASS geometric edges for coincident and antipodal normalized vertices; self dot = " ++ show dot
