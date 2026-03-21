module SimpleTypes.SumTypesSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck
  ( ambiguousVariantType,
    illegalEmptyMatching,
    nonExhaustiveMatchPatterns,
    unepxectedPatternForType,
    unexpectedInjection,
  )
spec :: Spec
spec = describe "sum types tests" $ do
  it "sum type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/SumTypes/sum-types.stella"
  it "match sum type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/SumTypes/match-sum-types.stella"
  it "empty match sum type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/SumTypes/ill-typed-empty-match.stella" illegalEmptyMatching
  it "non exhaustive pattern match sum type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/SumTypes/ill-typed-non-exhaustive-match.stella" nonExhaustiveMatchPatterns
  it "unexpected pattern match sum type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/SumTypes/ill-typed-unexpected-pattern.stella" unepxectedPatternForType
  it "ambiguous sum type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/SumTypes/ill-typed-ambiguous.stella" ambiguousVariantType
  it "unexpected injection sum type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/SumTypes/ill-typed-unexpected-injection.stella" unexpectedInjection
