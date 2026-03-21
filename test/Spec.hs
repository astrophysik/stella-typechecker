import Test.Hspec
import qualified Core.BoolSpec
import qualified Core.IntSpec
import qualified Core.LambdaSpec
import qualified Decl.DeclSpec
import qualified SimpleTypes.UnitSpec
import qualified SimpleTypes.TupleSpec

main :: IO ()
main = hspec $ do
  Core.BoolSpec.spec
  Core.IntSpec.spec
  Core.LambdaSpec.spec
  Decl.DeclSpec.spec
  SimpleTypes.UnitSpec.spec
  SimpleTypes.TupleSpec.spec
  