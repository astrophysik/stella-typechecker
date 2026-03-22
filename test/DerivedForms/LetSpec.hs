module DerivedForms.LetSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression)


spec :: Spec
spec = describe "let bindings tests" $ do
  it "simple let binding" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/LetBindings/simple-let.stella"
  it "nested let binding" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/LetBindings/nested-let.stella"
  it "ill typed let value" $ do
    shouldFailFileWith "test/Programs/DerivedForms/LetBindings/ill-typed-binding-value.stella" unexpectedTypeForExpression