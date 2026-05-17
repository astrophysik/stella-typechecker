module TypeReconstruction.TypeReconstruction (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression, undefinedVariable, ambiguousType, accursCheckInfiniteType)

spec :: Spec
spec = describe "type reconstruction tests" $ do
  it "return type auto" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/return-type-auto.stella"
  it "ill typed return type auto" $ do
    shouldFailFileWith "test/Programs/TypeReconstruction/ill-typed-return-type-auto.stella" unexpectedTypeForExpression
  it "ill typed undefined var" $ do
    shouldFailFileWith "test/Programs/TypeReconstruction/ill-typed-undefined-var.stella" undefinedVariable
  it "call function with auto arg and auto result" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/auto-increment-twice.stella"
  it "cons reconstruct" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/cons-reconstruct.stella"
  it "infer cons reconstruct" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/infer-cons-reconstruction.stella"
  it "ill typed ambigous reconstruct" $ do
    shouldFailFileWith "test/Programs/TypeReconstruction/ill-typed-ambigous-reconstruct.stella" ambiguousType
  it "let function reconstruct" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/let-function.stella"
  it "list ascription" $ do
    shouldTypecheckFile "test/Programs/TypeReconstruction/list-ascription.stella"
  it "list operations" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/list-operations.stella" 
  it "parenthesis expr" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/parenthesis-expr.stella" 
  it "inl reconstruct" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/inl-reconstruct.stella" 
  it "inl inr reconstruct" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/inl-inr-reconstruct.stella" 
  it "match sum type" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/sum-type-match.stella" 
  it "several functions reconstruct" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/several-functions.stella" 
  it "let square reconstruct" $ do 
    shouldTypecheckFile "test/Programs/TypeReconstruction/several-functions.stella" 
  it "ill typed infinite type" $ do
    shouldFailFileWith "test/Programs/TypeReconstruction/ill-typed-infinite-type.stella" accursCheckInfiniteType