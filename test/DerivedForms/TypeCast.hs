module DerivedForms.TypeCast (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression)


spec :: Spec
spec = describe "type cast tests" $ do
  it "infer type cast" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/TypeCast/infer-type-cast.stella"
  it "check type cast" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/TypeCast/check-type-cast.stella"
  it "ill typed type cast" $ do
    shouldFailFileWith "test/Programs/DerivedForms/TypeCast/ill-typed-type-cast.stella" unexpectedTypeForExpression
