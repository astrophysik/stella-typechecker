module Core.IntSpec (spec) where

import Test.Hspec
import TestUtil (shouldFailFileWith, shouldTypecheckFile)
import TypeCheck.TypeCheck (unexpectedTypeForExpression)

spec :: Spec
spec = describe "integer tests" $ do
  it "constant int" $ do
    shouldTypecheckFile "test/Programs/Core/Integer/constant.stella"
  it "pred" $ do
    shouldTypecheckFile "test/Programs/Core/Integer/pred.stella"
  it "ill typed pred" $ do
    shouldFailFileWith "test/Programs/Core/Integer/ill-typed-pred.stella" unexpectedTypeForExpression
  it "succ" $ do
    shouldTypecheckFile "test/Programs/Core/Integer/succ.stella"
  it "ill typed succ" $ do
    shouldFailFileWith "test/Programs/Core/Integer/ill-typed-succ.stella" unexpectedTypeForExpression
  it "iszero" $ do
    shouldTypecheckFile "test/Programs/Core/Integer/iszero.stella"
  it "ill typed iszero" $ do
    shouldFailFileWith "test/Programs/Core/Integer/ill-typed-iszero.stella" unexpectedTypeForExpression
  it "nat rec" $ do
    shouldTypecheckFile "test/Programs/Core/Integer/nat-rec.stella"
  it "ill-typed nat rec" $ do
    shouldFailFileWith "test/Programs/Core/Integer/ill-typed-nat-rec.stella" unexpectedTypeForExpression
