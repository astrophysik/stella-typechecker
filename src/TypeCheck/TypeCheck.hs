module TypeCheck.TypeCheck (checkProgram) where

import qualified Parsing.AbsSyntax as AbsSyntax

checkProgram :: AbsSyntax.Program -> Either String ()
checkProgram _ = Right ()
