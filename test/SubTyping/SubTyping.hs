module SubTyping.SubTyping (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedSubType)

spec :: Spec
spec = describe "subtyping tests" $ do
  it "ill typed variant subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-variant.stella" unexpectedSubType
  it "ill typed tuple subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-variant.stella" unexpectedSubType
  it "ill typed sum subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-sum.stella" unexpectedSubType
  it "ill typed ref subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-ref.stella" unexpectedSubType
  it "ill typed record subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-record.stella" unexpectedSubType
  it "ill typed list subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-list.stella" unexpectedSubType
  it "ill typed func subtype" $ do
    shouldFailFileWith "test/Programs/SubTyping/Base/ill-typed-func.stella" unexpectedSubType
  it "type record subtype" $ do
    shouldTypecheckFile "test/Programs/SubTyping/Base/type-record.stella"
  it "type variant subtype" $ do
    shouldTypecheckFile "test/Programs/SubTyping/Base/type-variant.stella"
  it "type variant check subtype" $ do
    shouldTypecheckFile "test/Programs/SubTyping/Base/type-variant-check.stella"