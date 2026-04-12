import qualified Core.BoolSpec
import qualified Core.IntSpec
import qualified Core.LambdaSpec
import qualified Decl.DeclSpec
import qualified DerivedForms.FixPointSpec
import qualified DerivedForms.LetSpec
import qualified DerivedForms.SequencingSpec
import qualified DerivedForms.TypeAsc
import qualified DerivedForms.TypeCast
import qualified Exceptions.ExceptionsSpec
import qualified Exceptions.PanicSpec
import qualified References.ReferencesSpec
import qualified SimpleTypes.ListSpec
import qualified SimpleTypes.RecordSpec
import qualified SimpleTypes.SumTypesSpec
import qualified SimpleTypes.TupleSpec
import qualified SimpleTypes.UnitSpec
import qualified SimpleTypes.VariantSpec
import qualified SubTyping.SubTyping
import Test.Hspec

main :: IO ()
main = hspec $ do
  Core.BoolSpec.spec
  Core.IntSpec.spec
  Core.LambdaSpec.spec
  Decl.DeclSpec.spec
  DerivedForms.TypeAsc.spec
  DerivedForms.FixPointSpec.spec
  DerivedForms.LetSpec.spec
  DerivedForms.SequencingSpec.spec
  DerivedForms.TypeCast.spec
  SimpleTypes.UnitSpec.spec
  SimpleTypes.TupleSpec.spec
  SimpleTypes.RecordSpec.spec
  SimpleTypes.SumTypesSpec.spec
  SimpleTypes.ListSpec.spec
  SimpleTypes.VariantSpec.spec
  References.ReferencesSpec.spec
  Exceptions.PanicSpec.spec
  Exceptions.ExceptionsSpec.spec
  SubTyping.SubTyping.spec
