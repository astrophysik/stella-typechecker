module DerivedForms.SequencingSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression)


spec :: Spec
spec = describe "sequencing tests" $ do
  it "simple sequence" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/Sequencing/simple-sequence.stella"
  it "sequence with variable" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/Sequencing/sequence-with-var.stella"
  it "nested sequence" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/Sequencing/nested-sequence.stella"
  it "sequence return bool" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/Sequencing/sequence-return-bool.stella"
  it "ill typed first not unit" $ do
    shouldFailFileWith "test/Programs/DerivedForms/Sequencing/ill-typed-first-not-unit.stella" unexpectedTypeForExpression
  it "ill typed wrong return type" $ do
    shouldFailFileWith "test/Programs/DerivedForms/Sequencing/ill-typed-wrong-return-type.stella" unexpectedTypeForExpression
