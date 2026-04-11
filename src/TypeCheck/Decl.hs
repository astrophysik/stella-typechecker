module TypeCheck.Decl
  ( checkDeclarations,
    collectDeclarations,
  )
where

import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.BidirectionalTyping (checkTypeExpression)
import TypeCheck.Common
  ( Context,
    emptyContext,
    insertException,
    insertVar,
    lookupException,
    lookupVar,
    validateType,
  )
import TypeCheck.Errors (dublicateFunctionDeclaration, duplicateExceptionType)

checkDeclarations :: Context -> [AbsSyntax.Decl] -> Either String ()
checkDeclarations _ [] = Right ()
checkDeclarations programContext ((AbsSyntax.DeclFun _ _ [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent paramName) paramType] (AbsSyntax.SomeReturnType returnType) _ _ expr) : xs) = do
  validateType returnType
  validateType paramType
  let functionContext = insertVar paramName paramType programContext
  checkTypeExpression functionContext expr returnType
  checkDeclarations programContext xs
checkDeclarations programContext ((AbsSyntax.DeclExceptionType _) : xs) = checkDeclarations programContext xs 
checkDeclarations _ _ = Left "Internal error : unsupported declaration"

collectDeclarations :: [AbsSyntax.Decl] -> Either String Context
collectDeclarations [] = Right emptyContext
collectDeclarations ((AbsSyntax.DeclFun _ (AbsSyntax.StellaIdent name) [AbsSyntax.AParamDecl _ paramType] (AbsSyntax.SomeReturnType returnType) _ _ _) : xs) = do
  validateType paramType
  validateType returnType
  tailContext <- collectDeclarations xs
  case lookupVar name tailContext of
    Just _ -> Left dublicateFunctionDeclaration
    _ -> pure $ insertVar name (AbsSyntax.TypeFun [paramType] returnType) tailContext
collectDeclarations ((AbsSyntax.DeclExceptionType exceptionType) : xs) = do
  tailContext <- collectDeclarations xs
  case lookupException tailContext of 
    Just _ -> Left $ duplicateExceptionType ++ "\nduplicate exception type declaration(s) at top-level (only one is allowed)"
    _ -> case exceptionType of 
      AbsSyntax.TypeVar typeName-> Left $ "Illegal type\n" ++ show typeName
      _ -> pure $ insertException exceptionType tailContext
collectDeclarations _ = Left "Internal error : unsupported declaration"
