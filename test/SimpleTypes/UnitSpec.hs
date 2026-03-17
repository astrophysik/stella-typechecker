module SimpleTypes.UnitSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression)

spec :: Spec
spec = describe "unit type tests" $ do
  it "unit type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Unit/unit.stella"
  it "ill typed unit type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Unit/ill-typed-unit.stella" unexpectedTypeForExpression
