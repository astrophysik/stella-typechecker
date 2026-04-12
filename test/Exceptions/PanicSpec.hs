module Exceptions.PanicSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (ambiguousPanicType)

spec :: Spec
spec = describe "panic tests" $ do
  it "panic as Nat" $ do
    shouldTypecheckFile "test/Programs/Exceptions/Panic/panic-as-nat.stella"
  it "panic in if" $ do
    shouldTypecheckFile "test/Programs/Exceptions/Panic/panic-in-if.stella"
  it "panic as function" $ do
    shouldTypecheckFile "test/Programs/Exceptions/Panic/panic-as-function.stella"
  it "ill typed panic without ascription" $ do
    shouldFailFileWith "test/Programs/Exceptions/Panic/ill-typed-panic-infer.stella" ambiguousPanicType
