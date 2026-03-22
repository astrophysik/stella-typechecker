module TestUtil
  ( shouldTypecheckFile
  , shouldFailFileWith
  ) where

import Test.Hspec (Expectation, shouldBe, expectationFailure)
import Parsing.LexSyntax (tokens)
import Parsing.ParSyntax (pProgram)
import TypeCheck.TypeCheck (typeCheck)

shouldTypecheckFile :: FilePath -> Expectation
shouldTypecheckFile path = do
  src <- readFile path
  case pProgram (tokens src) of
    Left perr -> expectationFailure ("parse failed in " ++ path ++ ":\n" ++ perr)
    Right ast -> typeCheck ast `shouldBe` Right ()

shouldFailFileWith :: FilePath -> String -> Expectation
shouldFailFileWith path expectedErr = do
  src <- readFile path
  case pProgram (tokens src) of
    Left perr -> expectationFailure ("parse failed in " ++ path ++ ":\n" ++ perr)
    Right ast -> typeCheck ast `shouldBe` Left expectedErr