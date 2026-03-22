module DerivedForms.TypeAsc (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression)


spec :: Spec
spec = describe "type asc tests" $ do
  it "simple type asc" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/TypeAsc/type-asc.stella"
  it "ill typed type asc" $ do
    shouldFailFileWith "test/Programs/DerivedForms/TypeAsc/ill-typed-type-asc.stella" unexpectedTypeForExpression
  it "nested type asc" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/TypeAsc/nested-type-asc.stella"