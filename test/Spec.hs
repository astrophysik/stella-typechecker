import Test.Hspec
import qualified Core.BoolSpec
import qualified Core.IntSpec
import qualified Core.LambdaSpec

main :: IO ()
main = hspec $ do
  Core.BoolSpec.spec
  Core.IntSpec.spec
  Core.LambdaSpec.spec