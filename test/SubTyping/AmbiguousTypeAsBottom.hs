module SubTyping.AmbiguousTypeAsBottom (spec) where

import Test.Hspec
import TestUtil

spec :: Spec
spec = describe "ambiguous-type-as-bottom tests" $ do
  it "type ambiguous inl" $ do
    shouldTypecheckFile "test/Programs/SubTyping/AmbiguousTypeAsBottom/type-ambiguous-inl.stella"
  it "type ambiguous inr" $ do
    shouldTypecheckFile "test/Programs/SubTyping/AmbiguousTypeAsBottom/type-ambiguous-inr.stella"
  it "type ambiguous empty list" $ do
    shouldTypecheckFile "test/Programs/SubTyping/AmbiguousTypeAsBottom/type-ambiguous-list.stella"
  it "type ambiguous panic" $ do
    shouldTypecheckFile "test/Programs/SubTyping/AmbiguousTypeAsBottom/type-ambiguous-panic.stella"
  it "type ambiguous throw" $ do
    shouldTypecheckFile "test/Programs/SubTyping/AmbiguousTypeAsBottom/type-ambiguous-throw.stella"

