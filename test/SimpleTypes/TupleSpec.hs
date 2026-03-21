module SimpleTypes.TupleSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (notATuple, unexpectedTuple, tupleIndexOutOfBounds)

spec :: Spec
spec = describe "tuple type tests" $ do
  it "pair type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Tuple/pair.stella"
  it "tuple type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Tuple/tuple.stella"
  it "empty tuple type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Tuple/empty-tuple.stella"
  it "ill typed pair proj" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Tuple/ill-typed-proj.stella" notATuple
  it "ill typed unexpected pair" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Tuple/ill-typed-unexpected-tuple.stella" unexpectedTuple
  it "ill typed unexpected pair application" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Tuple/ill-typed-unexpected-tuple-call.stella" unexpectedTuple
  it "ill typed index out of bound" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Tuple/ill-typed-index-out-of-bound.stella" tupleIndexOutOfBounds

