module References.ReferencesSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck
  ( ambiguousReferenceType,
    notAReference,
    unexpectedMemoryAddress,
    unexpectedReferenceType,
    unexpectedTypeForExpression
  )

spec :: Spec
spec = describe "references tests" $ do
  it "simple ref" $ do
    shouldTypecheckFile "test/Programs/References/simple-ref.stella"
  it "complex ref" $ do
    shouldTypecheckFile "test/Programs/References/complex-ref.stella"
  it "ref and deref" $ do
    shouldTypecheckFile "test/Programs/References/ref-and-deref.stella"
  it "ref assign" $ do
    shouldTypecheckFile "test/Programs/References/ref-assign.stella"
  it "ref ref assign" $ do
    shouldTypecheckFile "test/Programs/References/ref-ref-assign.stella"
  it "ref bool" $ do
    shouldTypecheckFile "test/Programs/References/ref-bool.stella"
  it "raw address check" $ do
    shouldTypecheckFile "test/Programs/References/raw-address.stella"
  it "ill typed deref not ref" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-deref-not-ref.stella" unexpectedTypeForExpression
  it "ill typed assign type mismatch" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-assign-type-mismatch.stella" unexpectedTypeForExpression
  it "ill typed new expect type" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-new-expect-type.stella" unexpectedReferenceType
  it "ill typed ambiguous reference type" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-ambiguous-ref.stella" ambiguousReferenceType
  it "ill typed non reference assign" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-non-ref-assign.stella" notAReference
  it "ill typed unexpected memory address" $ do
    shouldFailFileWith "test/Programs/References/ill-typed-unexpected-address.stella" unexpectedMemoryAddress
