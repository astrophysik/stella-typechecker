module SubTyping.TopBottom (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedSubType)


spec :: Spec
spec = describe "subtyping top-bottom tests" $ do
  it "subtype for top" $ do
    shouldTypecheckFile "test/Programs/SubTyping/TopBottom/type-top.stella"
  it "ill typed subtype for bottom" $ do
    shouldFailFileWith "test/Programs/SubTyping/TopBottom/ill-typed-type-bottom.stella" unexpectedSubType
  it "subtype for top exception" $ do
    shouldTypecheckFile "test/Programs/SubTyping/TopBottom/type-error.stella"
