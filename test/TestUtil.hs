module TestUtil
  ( shouldTypecheckFile
  , shouldFailFileWith
  ) where

import Test.Hspec (Expectation, shouldBe, shouldContain, expectationFailure)
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
        Right ast -> case typeCheck ast of
            Left err -> err `shouldContain` expectedErr
            Right () -> expectationFailure "expected type error but got success"