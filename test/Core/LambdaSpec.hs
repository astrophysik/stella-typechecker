module Core.LambdaSpec (spec) where

import Test.Hspec
import TestUtil (shouldFailFileWith, shouldTypecheckFile)
import TypeCheck.TypeCheck (notAFunction, undefinedVariable, unexpectedTypeForParam, unexpectedLambda)

spec :: Spec
spec = describe "lambda tests" $ do
  it "lambda app" $ do
    shouldTypecheckFile "test/Programs/Core/Lambda/lambda-app.stella"
  it "ill typed app" $ do
    shouldFailFileWith "test/Programs/Core/Lambda/ill-typed-app.stella" notAFunction
  it "ill typed undefined var" $ do
    shouldFailFileWith "test/Programs/Core/Lambda/ill-typed-undefined-var.stella" undefinedVariable
  it "ill typed param" $ do
    shouldFailFileWith "test/Programs/Core/Lambda/ill-typed-param.stella" unexpectedTypeForParam
  it "ill typed unexpected lambda" $ do
    shouldFailFileWith "test/Programs/Core/Lambda/ill-typed-unexpected-lambda.stella" unexpectedLambda
