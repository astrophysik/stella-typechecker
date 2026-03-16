module Core.BoolSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck

spec :: Spec
spec = describe "bool" $ do
  it "constant true" $ do
    shouldTypecheckFile "test/Programs/Core/Bool/constant-true.stella"
  it "constant false" $ do
    shouldTypecheckFile "test/Programs/Core/Bool/constant-false.stella"
  it "if" $ do
    shouldTypecheckFile "test/Programs/Core/Bool/if.stella"  
  it "nested if" $ do
    shouldTypecheckFile "test/Programs/Core/Bool/nested-if.stella"  
  it "ill typed if" $ do
    shouldFailFileWith "test/Programs/Core/Bool/ill-typed-if.stella" unexpectedTypeForExpression  

