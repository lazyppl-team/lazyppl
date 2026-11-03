module RenderExamples where
import qualified IBPOutput
import qualified WienerDemo
import qualified RegressionDemo
import qualified ClusteringDemo
import qualified ProgramInductionDemo
import qualified MondrianOutput
import qualified GaussianProcessDemo
import qualified Index

main :: IO ()
main = do
  IBPOutput.main
  WienerDemo.main
  RegressionDemo.main
  ClusteringDemo.main
  ProgramInductionDemo.main
  MondrianOutput.main
  GaussianProcessDemo.main
  Index.main
