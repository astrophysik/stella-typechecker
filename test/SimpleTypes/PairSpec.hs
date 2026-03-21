module SimpleTypes.PairSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression, notATuple, unexpectedTuple)

spec :: Spec
spec = describe "pair type tests" $ do
  it "pair type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Pair/pair.stella"
  it "ill typed pair proj" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Pair/ill-typed-proj.stella" notATuple
  it "ill typed unexpected pair" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Pair/ill-typed-unexpected-tuple.stella" unexpectedTuple
  it "ill typed unexpected pair application" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Pair/ill-typed-unexpected-tuple-call.stella" unexpectedTuple

