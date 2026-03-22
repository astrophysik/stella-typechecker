module Main (main) where

import Options.Applicative
import Parsing.LexSyntax (tokens)
import Parsing.ParSyntax (pProgram)
import TypeCheck.TypeCheck (typeCheck)
import System.IO (hPutStrLn, stderr)

data Options = Options
  {inputFile :: Maybe FilePath, parseOnly :: Bool}
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional
      ( strOption
          ( long "input"
              <> short 'i'
              <> metavar "FILE"
              <> help "Read input file (defalut std::in)"
          )
      )
    <*> switch
      ( long "parse"
          <> short 'p'
          <> help "Only parse and print AST (no typecheck)"
      )

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper) fullDesc
  program <- maybe getContents readFile (inputFile opts)
  case pProgram $ tokens program of
    Left msg -> putStrLn msg
    Right ast ->
      if parseOnly opts
        then print ast
        else case typeCheck ast of
          Left msg -> hPutStrLn stderr msg
          Right () -> putStrLn "Input program is well-typed!\n"
