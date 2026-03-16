module Decl.DeclSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedTypeForExpression, missingMain)

spec :: Spec
spec = describe "decls tests" $ do
  it "function param" $ do
    shouldTypecheckFile "test/Programs/Decl/function-param.stella"
  it "function call" $ do
    shouldTypecheckFile "test/Programs/Decl/function-call.stella"
  it "missing main" $ do
    shouldFailFileWith "test/Programs/Decl/missing-main.stella" missingMain
