module SimpleTypes.ListSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck
  ( ambiguousList,
    notAList,
    unexpectedList,
    unexpectedTypeForExpression
  )

spec :: Spec
spec = describe "list type tests" $ do
  it "empty list type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/List/empty-list.stella"
  it "ill typed empty list type" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-ambiguous-list.stella" ambiguousList
  it "isempty list type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/List/is-empty-list.stella"
  it "ill typed is empty list" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-is-empty-list.stella" notAList
  it "head list" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/List/head-list.stella"
  it "ill typed head list" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-head-list.stella" notAList
  it "tail list" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/List/tail-list.stella"
  it "ill typed tail list" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-tail-list.stella" notAList
  it "cons list" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/List/cons-list.stella"
  it "ill typed tail cons list" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-tail-cons-list.stella" unexpectedTypeForExpression
  it "ill typed head cons list" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-head-cons-list.stella" unexpectedList
  it "ill typed unexpected list type in head" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-unexpected-type-head.stella" unexpectedTypeForExpression
  it "ill typed unexpected list type in tail" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/List/ill-typed-unexpected-type-tail.stella" unexpectedTypeForExpression


