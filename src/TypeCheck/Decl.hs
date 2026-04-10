module TypeCheck.Decl
  ( checkDeclarations,
    collectDeclarations,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.BidirectionalTyping (checkTypeExpression)
import TypeCheck.Common (Context, extendContext, validateType)
import TypeCheck.Errors (dublicateFunctionDeclaration)

checkDeclarations :: Context -> [AbsSyntax.Decl] -> Either String ()
checkDeclarations _ [] = Right ()
checkDeclarations programContext ((AbsSyntax.DeclFun _ _ [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent paramName) paramType] (AbsSyntax.SomeReturnType returnType) _ _ expr) : xs) = do
  validateType returnType
  validateType paramType
  let functionContext = extendContext paramName paramType programContext
  checkTypeExpression functionContext expr returnType
  checkDeclarations programContext xs
checkDeclarations _ _ = Left "Internal error : unsupported declaration"

collectDeclarations :: [AbsSyntax.Decl] -> Either String Context
collectDeclarations [] = Right HM.empty
collectDeclarations ((AbsSyntax.DeclFun _ (AbsSyntax.StellaIdent name) [AbsSyntax.AParamDecl _ paramType] (AbsSyntax.SomeReturnType returnType) _ _ _) : xs) = do
  validateType paramType
  validateType returnType
  tailContext <- collectDeclarations xs
  case HM.lookup name tailContext of
    Just _ -> Left dublicateFunctionDeclaration
    _ -> pure $ HM.insert name (AbsSyntax.TypeFun [paramType] returnType) tailContext
collectDeclarations _ = Left "Internal error : unsupported declaration"
