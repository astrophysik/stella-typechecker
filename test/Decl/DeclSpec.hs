module Decl.DeclSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (missingMain, dublicateFunctionDeclaration, unexpectedTypeForExpression)

spec :: Spec
spec = describe "decls tests" $ do
  it "function param" $ do
    shouldTypecheckFile "test/Programs/Decl/function-param.stella"
  it "function call" $ do
    shouldTypecheckFile "test/Programs/Decl/function-call.stella"
  it "missing main" $ do
    shouldFailFileWith "test/Programs/Decl/missing-main.stella" missingMain
  it "dublicate function declaration" $ do
    shouldFailFileWith "test/Programs/Decl/dublicate-function-declaration.stella" dublicateFunctionDeclaration
  it "ill typed function param" $ do
    shouldFailFileWith "test/Programs/Decl/ill-typed-function-param.stella" unexpectedTypeForExpression
