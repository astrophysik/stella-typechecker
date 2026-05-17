module UniversalTypes.UniversalTypes(spec)
where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (incorrectNumberOfTypeArguments, duplicateTypeParameter, undefinedTypeVariable, notAGenericFunction, unexpectedTypeForExpression)

spec :: Spec
spec = describe "universal types test" $ do
  it "generic function decl" $ do
    shouldTypecheckFile "test/Programs/UniversalTypes/generic-function-decl.stella"
  it "nested generic function decl" $ do
    shouldTypecheckFile "test/Programs/UniversalTypes/nested-generic-function-decl.stella"
  it "generic function with two type args decl" $ do
    shouldTypecheckFile "test/Programs/UniversalTypes/generic-function-two-arg.stella"
  it "generic self application" $ do
    shouldTypecheckFile "test/Programs/UniversalTypes/generic-self-app.stella"
  it "ill typed incorrect type args" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-incorrect-type-arg.stella" incorrectNumberOfTypeArguments
  it "ill typed undefined type var" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-undefined-type-var.stella" undefinedTypeVariable
  it "ill typed nested undefined type var" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-nested-undefined-var.stella" undefinedTypeVariable
  it "ill typed not generic function" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-not-generic-function.stella" notAGenericFunction
  it "ill typed unexpected type #1" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-unexpected-type.stella" unexpectedTypeForExpression
  it "ill typed unexpected type #2" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-wrong-type.stella" unexpectedTypeForExpression
  it "ill typed missed forall" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-missed-forall.stella" unexpectedTypeForExpression
  it "ill typed duplicate type param" $ do
    shouldFailFileWith "test/Programs/UniversalTypes/ill-typed-duplicate-type-param.stella" duplicateTypeParameter




