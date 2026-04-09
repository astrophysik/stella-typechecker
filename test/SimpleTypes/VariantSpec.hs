module SimpleTypes.VariantSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck
  ( ambiguousVariantType,
    nonExhaustiveMatchPatterns,
    unexpectedVariant,
    unexpectedVariantLabel,
    duplicateVariantLabels,
  )
import TypeCheck.Errors (duplicateVariantLabels)

spec :: Spec
spec = describe "variant type tests" $ do
  it "variant type check" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Variant/variant-type-check.stella"
  it "ill typed unexpected label" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-unexpected-label.stella" unexpectedVariantLabel
  it "variant match unit" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Variant/variant-match-unit.stella"
  it "variant pattern match var" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Variant/variant-patten-match.stella"
  it "unexpected variant" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-unexpected-variant.stella" unexpectedVariant
  it "nonexhaustive match" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-nonexhaustive-match.stella" nonExhaustiveMatchPatterns
  it "ambiguous variant type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-ambiguous.stella" ambiguousVariantType
  it "ill typed unexpected label in expression" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-unexpected-label-expr.stella" unexpectedVariantLabel
  it "ill typed duplicate variant labels" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Variant/ill-typed-duplicate-labels.stella" duplicateVariantLabels
