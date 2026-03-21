module DerivedForms.FixPointSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression, notAFunction)


spec :: Spec
spec = describe "fixpoint tests" $ do
  it "simple fixpoint" $ do
    shouldTypecheckFile "test/Programs/DerivedForms/FixPoint/fixpoint.stella"
  it "ill typed fixpoint" $ do
    shouldFailFileWith "test/Programs/DerivedForms/FixPoint/ill-typed-fixpoint.stella" unexpectedTypeForExpression
  it "ill typed fixpoint not function" $ do
    shouldFailFileWith "test/Programs/DerivedForms/FixPoint/ill-typed-not-function.stella" notAFunction