module TypeCheck.ConstraintBased.Decl
  ( typeCheck,
  )
where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.ConstraintBased.Unification
import TypeCheck.Common (validateType)
import TypeCheck.ConstraintBased.Context
  ( Context(..),
    emptyContext,
    insertVar,
    lookupVar,
  )
import TypeCheck.ConstraintBased.Typing (Infer, inferTypeExpression, freshTypeVar, withVar)
import TypeCheck.Errors (dublicateFunctionDeclaration, missingMain)

runInfer :: Context -> Infer a -> Either String a
runInfer ctx action = evalStateT action ctx

replaceAuto :: AbsSyntax.Type -> Infer AbsSyntax.Type
replaceAuto AbsSyntax.TypeAuto = freshTypeVar
replaceAuto (AbsSyntax.TypeFun paramsType returnType) = AbsSyntax.TypeFun <$> mapM replaceAuto paramsType <*> replaceAuto returnType
replaceAuto (AbsSyntax.TypeList t) = AbsSyntax.TypeList <$> replaceAuto t
replaceAuto (AbsSyntax.TypeSum l r) = AbsSyntax.TypeSum <$> replaceAuto l <*> replaceAuto r
-- replaceAuto (AbsSyntax.TypeRecord fields) = AbsSyntax.TypeRecord <$> mapM replaceRecordField fields
--     where
--       replaceRecordField (AbsSyntax.ARecordFieldType ident t) =
--         AbsSyntax.ARecordFieldType ident <$> replaceAuto t
replaceAuto t = pure t

checkTypeExpression :: AbsSyntax.Expr -> AbsSyntax.Type -> Infer [(AbsSyntax.Type, AbsSyntax.Type)]
checkTypeExpression expr expectedType = do
  (actualType, constraints) <- inferTypeExpression expr
  pure $ constraints ++ [(actualType, expectedType)]

checkDeclarations :: Context -> [AbsSyntax.Decl] -> Infer [(AbsSyntax.Type, AbsSyntax.Type)]
checkDeclarations _ [] = pure []
checkDeclarations programContext ((AbsSyntax.DeclFun _ _ [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent paramName) paramType] (AbsSyntax.SomeReturnType returnType) _ _ expr) : xs) = do
  constraints <- withVar paramName paramType (checkTypeExpression expr returnType)
  tailConstraints <- checkDeclarations programContext xs
  pure $ constraints ++ tailConstraints
checkDeclarations programContext ((AbsSyntax.DeclExceptionType _) : xs) = checkDeclarations programContext xs
checkDeclarations _ _ = lift $ Left "Internal error : unsupported declaration"

collectDeclarations :: [AbsSyntax.Decl] -> Infer (Context, [AbsSyntax.Decl])
collectDeclarations [] = pure (emptyContext, [])
collectDeclarations ((AbsSyntax.DeclFun annotation funcName@(AbsSyntax.StellaIdent name) [AbsSyntax.AParamDecl paramName paramType] (AbsSyntax.SomeReturnType returnType) throwType decls expr) : xs) = do
  newParamType <- replaceAuto paramType
  newReturnType <- replaceAuto returnType
  (tailContext, tailDecls) <- collectDeclarations xs
  case lookupVar name tailContext of
    Just _ -> lift $ Left dublicateFunctionDeclaration
    _ -> do 
      currentId <- gets typeVarId
      let newVarContext = HM.insert name (AbsSyntax.TypeFun [newParamType] newReturnType) (variableContext tailContext)
      let newContext = Context {variableContext = newVarContext, typeVarId = currentId}
      pure (newContext, AbsSyntax.DeclFun annotation funcName [AbsSyntax.AParamDecl paramName newParamType] (AbsSyntax.SomeReturnType newReturnType) throwType decls expr : tailDecls )
collectDeclarations _ = lift $ Left "Internal error : unsupported declaration"

typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck (AbsSyntax.AProgram _ extensions declarations) = do
  (programContext, programDeclarations) <- runInfer emptyContext (collectDeclarations declarations)
  case lookupVar "main" programContext of
    Just _ -> do
      constraints <- evalStateT (checkDeclarations programContext programDeclarations) programContext
      -- Left $ show constraints
      unify constraints
    Nothing -> Left missingMain
