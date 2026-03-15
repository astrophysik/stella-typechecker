module Main (main) where

import Options.Applicative
import Parsing.LexSyntax (tokens)
import Parsing.ParSyntax (pProgram)
import TypeCheck.TypeCheck (checkProgram)

newtype Options = Options
  {inputFile :: Maybe FilePath}
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

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper) fullDesc
  program <- maybe getContents readFile (inputFile opts)
  case pProgram $ tokens program of
    Left msg -> putStrLn msg
    Right ast -> case checkProgram ast of
      Left msg -> putStrLn msg
      Right () -> putStrLn "OK"

-- main :: IO ()
-- main = do
--   input <- getContents
--   case pProgram $ tokens input of
--     Left msg -> putStrLn msg
--     Right ast -> print ast
