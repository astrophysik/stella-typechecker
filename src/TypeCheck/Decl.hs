module TypeCheck.Decl
  ( checkDeclarations,
    collectDeclarations,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.Expr (checkTypeExpression)
import TypeCheck.Common (Context, extendContext)
import TypeCheck.Errors (dublicateFunctionDeclaration, missingMain)

checkDeclarations :: Context -> [AbsSyntax.Decl] -> Either String ()
checkDeclarations _ [] = Right ()
checkDeclarations programContext ((AbsSyntax.DeclFun _ _ params (AbsSyntax.SomeReturnType returnType) _ _ expr) : xs) = do
  let functionContext = foldl (\context (AbsSyntax.AParamDecl (AbsSyntax.StellaIdent varName) varType) -> extendContext varName varType context) programContext params
  checkTypeExpression functionContext expr returnType
  checkDeclarations programContext xs
checkDeclarations _ _ = Left "Unsupported declaration"

collectDeclarations :: [AbsSyntax.Decl] -> Either String Context
collectDeclarations [] = Right HM.empty
collectDeclarations ((AbsSyntax.DeclFun _ (AbsSyntax.StellaIdent name) [AbsSyntax.AParamDecl _ paramType] (AbsSyntax.SomeReturnType returnType) _ _ _) : xs) = do
  tailContext <- collectDeclarations xs
  case HM.lookup name tailContext of
    Just _ -> Left dublicateFunctionDeclaration
    _ -> pure $ HM.insert name (AbsSyntax.TypeFun [paramType] returnType) tailContext
collectDeclarations _ = Left "Unsupported declaration"
