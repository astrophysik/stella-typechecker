import Test.Hspec
import qualified Core.BoolSpec

main :: IO ()
main = hspec $ do
  Core.BoolSpec.spec