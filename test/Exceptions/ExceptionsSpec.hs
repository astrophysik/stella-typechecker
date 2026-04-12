module Exceptions.ExceptionsSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.Errors
  ( exceptionTypeNotDeclared,
    unexpectedTypeForExpression,
    ambiguousThrowType,
    duplicateExceptionType
  )

spec :: Spec
spec = describe "exceptions tests" $ do
  describe "throw tests" $ do
    it "throw type check" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/throw-type-check.stella"
    it "throw basic" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/throw-basic.stella"
    it "throw in function" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/throw-in-function.stella"
    it "ill typed throw type infer" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-throw-type-infer.stella" ambiguousThrowType
    it "ill typed throw wrong type" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-throw-wrong-type.stella" unexpectedTypeForExpression
    it "ill typed throw no exception type declared" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-throw-no-exception-type.stella" exceptionTypeNotDeclared
    it "ill typed duplicate exception type" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-duplicate-exception.stella" duplicateExceptionType

  describe "try-catch tests" $ do
    it "try-catch basic" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/try-catch-basic.stella"
    it "try-catch with throw" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/try-catch-with-throw.stella"
    it "try-catch with different types (sum type)" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/try-catch-different-types.stella"
    it "ill typed try-catch type mismatch" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-try-catch-mismatch.stella" unexpectedTypeForExpression
    it "ill typed try-catch no exception type declared" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-try-catch-no-exception-type.stella" exceptionTypeNotDeclared

  describe "try-with tests" $ do
    it "try-with basic" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/try-with-basic.stella"
    it "try-with with throw" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/try-with-throw.stella"
    it "ill typed try-with type mismatch" $ do
      shouldFailFileWith "test/Programs/Exceptions/Exceptions/ill-typed-try-with-mismatch.stella" unexpectedTypeForExpression
    it "try-with no exception type declared" $ do
      shouldTypecheckFile "test/Programs/Exceptions/Exceptions/ill-typed-try-with-no-exception-type.stella"
