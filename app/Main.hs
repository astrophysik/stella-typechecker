module Main (main) where

import Parsing.ErrM (Err(..))
import Parsing.ParSyntax (pProgram)
import Parsing.LexSyntax (tokens)

main :: IO ()
main = do
  input <- getContents
  case pProgram $ tokens input of
    Left msg -> putStrLn msg
    Right ast  -> print ast