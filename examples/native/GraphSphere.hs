-- | The sphere figure of the graph example, images/graph-sphere.html. It runs
-- the page's Metropolis-Hastings call (GraphDemo.testA) with the global
-- generator seeded, so the points it writes to graph-sphere.csv and the theta
-- it prints are reproducible. Plot them with
--
-- > uv run examples/models/geometric_graph_plot.py THETA --csv graph-sphere.csv --out images/graph-sphere
-- > python3 examples/test/check_graph_figure.py images/graph-sphere.html graph-sphere.csv THETA
module GraphSphere where

import qualified GraphDemo
import System.Random (mkStdGen, setStdGen)

main :: IO ()
main = setStdGen (mkStdGen 1) >> GraphDemo.testA
