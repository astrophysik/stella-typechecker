module TypeCheck.TypeCheck (typeCheck, unexpectedTypeForExpression, undefinedVariable, unexpectedLambda, notAFunction, unexpectedTypeForParam) where

import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

type Context = HM.HashMap String AbsSyntax.Type

emptyContext :: Context
emptyContext = HM.empty

unexpectedTypeForExpression :: String
unexpectedTypeForExpression = "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"

undefinedVariable :: String
undefinedVariable = "ERROR_UNDEFINED_VARIABLE"

notAFunction :: String
notAFunction = "ERROR_NOT_A_FUNCTION"

unexpectedTypeForParam :: String
unexpectedTypeForParam = "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"

unexpectedLambda :: String
unexpectedLambda = "ERROR_UNEXPECTED_LAMBDA"

-- Type infer
inferTypeExpression :: Context -> AbsSyntax.Expr -> Either String AbsSyntax.Type
-- Boolean expressions
-- T-False
inferTypeExpression _ AbsSyntax.ConstFalse = Right AbsSyntax.TypeBool
-- T-True
inferTypeExpression _ AbsSyntax.ConstTrue = Right AbsSyntax.TypeBool
-- T-If
inferTypeExpression context (AbsSyntax.If condition onTrue onFalse) = do
  checkTypeExpression context condition AbsSyntax.TypeBool
  tTrue <- inferTypeExpression context onTrue
  checkTypeExpression context onFalse tTrue
  pure tTrue
-- Integer expressions
-- T-Zero
inferTypeExpression _ (AbsSyntax.ConstInt _) = Right AbsSyntax.TypeNat
-- T-Succ
inferTypeExpression context (AbsSyntax.Succ integer) = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeNat
-- T-Pred
inferTypeExpression context (AbsSyntax.Pred integer) = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeNat
-- T-IsZero
inferTypeExpression context (AbsSyntax.IsZero integer) = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeBool
-- Lambda expressions
-- T-Var
inferTypeExpression context (AbsSyntax.Var (AbsSyntax.StellaIdent var)) = case HM.lookup var context of
  Just tValue -> Right tValue
  Nothing -> Left undefinedVariable
-- T-Abs
inferTypeExpression context (AbsSyntax.Abstraction [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent var) varType] body) = do
  bodyType <- inferTypeExpression (HM.insert var varType context) body
  pure $ AbsSyntax.TypeFun [varType] bodyType
inferTypeExpression _ (AbsSyntax.Abstraction [] _) = Left "function with zero arguments"
inferTypeExpression _ (AbsSyntax.Abstraction (_ : _ : _) _) = Left "function with several arguments"
-- T-App
inferTypeExpression context (AbsSyntax.Application function [argument]) = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [varType] bodyType) -> do
      case checkTypeExpression context argument varType of
        Left _ -> Left unexpectedTypeForParam
        Right _ -> pure bodyType
    _ -> Left notAFunction
inferTypeExpression _ (AbsSyntax.Application _ []) = Left "apply function with zero arguments"
inferTypeExpression _ (AbsSyntax.Application _ (_ : _ : _)) = Left "apply function with several arguments"
inferTypeExpression _ _ = Left "unsupported"

-- Type check
checkTypeExpression :: Context -> AbsSyntax.Expr -> AbsSyntax.Type -> Either String ()
-- Boolean expressions
-- T-False
checkTypeExpression _ AbsSyntax.ConstFalse expectedType = case expectedType of
  AbsSyntax.TypeBool -> Right ()
  _ -> Left unexpectedTypeForExpression
-- T-True
checkTypeExpression _ AbsSyntax.ConstTrue expectedType = case expectedType of
  AbsSyntax.TypeBool -> Right ()
  _ -> Left unexpectedTypeForExpression
-- T-If
checkTypeExpression context (AbsSyntax.If condition onTrue onFalse) expectedType = do
  checkTypeExpression context condition AbsSyntax.TypeBool
  checkTypeExpression context onTrue expectedType
  checkTypeExpression context onFalse expectedType
  pure ()
-- Integer expressions
-- T-Zero
checkTypeExpression _ (AbsSyntax.ConstInt _) expectedType = case expectedType of
  AbsSyntax.TypeNat -> Right ()
  _ -> Left unexpectedTypeForExpression
-- T-Succ
checkTypeExpression context (AbsSyntax.Succ integer) expectedType = case expectedType of
  AbsSyntax.TypeNat -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
-- T-Pred
checkTypeExpression context (AbsSyntax.Pred integer) expectedType = case expectedType of
  AbsSyntax.TypeNat -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
-- T-IsZero
checkTypeExpression context (AbsSyntax.IsZero integer) expectedType = case expectedType of
  AbsSyntax.TypeBool -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
-- T-Var
checkTypeExpression context var@(AbsSyntax.Var _) expectedType = do
  varType <- inferTypeExpression context var
  if varType == expectedType then Right () else Left unexpectedTypeForExpression
-- T-Abs
checkTypeExpression context (AbsSyntax.Abstraction [AbsSyntax.AParamDecl varIdent@(AbsSyntax.StellaIdent varName) varType] body) (AbsSyntax.TypeFun [paramType] resultType) =
  if varType == paramType
    then
      ( do
          checkTypeExpression
            (HM.insert varName paramType context)
            body
            resultType
          pure ()
      )
    else
      Left unexpectedTypeForParam
checkTypeExpression _ (AbsSyntax.Abstraction _ _) _ = Left unexpectedLambda
-- T-App
checkTypeExpression context (AbsSyntax.Application function [argument]) expectedType = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] resultType) -> do
      checkTypeExpression context argument paramType
      if resultType == expectedType then pure () else Left unexpectedTypeForExpression
    _ -> Left notAFunction
checkTypeExpression _ _ _ = Left "unsupported"

checkDeclaration :: AbsSyntax.Decl -> Either String ()
checkDeclaration (AbsSyntax.DeclFun annotations ident params returnType throwType decls expr) = case inferTypeExpression emptyContext expr of
  Left msg -> Left msg
  Right _ -> Right ()
checkDeclaration _ = Left "unsupported"

typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck (AbsSyntax.AProgram languageDecl extentions declarations) = traverse_ checkDeclaration declarations
