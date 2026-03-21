module TypeCheck.TypeCheck
  ( typeCheck,
    unexpectedTypeForExpression,
    undefinedVariable,
    unexpectedLambda,
    notAFunction,
    unexpectedTypeForParam,
    missingMain,
    notATuple,
    unexpectedTuple,
    tupleIndexOutOfBounds,
    unexpectedRecord, 
    notARecord,
    unexpectedFieldAccess,
    missingRecordFields,
    unexpectedRecordFields
  )
where

import Control.Arrow (ArrowChoice (right))
import Control.Monad (forM_)
import qualified Control.Monad (zipWithM_)
import Data.Char (GeneralCategory (Control))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parsing.AbsSyntax as AbsSyntax
import qualified Parsing.ParSyntax as AbsSyntax

type Context = HM.HashMap String AbsSyntax.Type

nthElement :: Integer -> [a] -> Maybe a
nthElement 1 (x : _) = Just x
nthElement n (_ : xs) | n > 1 = nthElement (n - 1) xs
nthElement _ _ = Nothing

unexpectedTypeForExpression :: String
unexpectedTypeForExpression = "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"

undefinedVariable :: String
undefinedVariable = "ERROR_UNDEFINED_VARIABLE"

notAFunction :: String
notAFunction = "ERROR_NOT_A_FUNCTION"

notATuple :: String
notATuple = "ERROR_NOT_A_TUPLE"

notARecord :: String
notARecord = "ERROR_NOT_A_RECORD"

unexpectedTypeForParam :: String
unexpectedTypeForParam = "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER"

unexpectedLambda :: String
unexpectedLambda = "ERROR_UNEXPECTED_LAMBDA"

missingMain :: String
missingMain = "ERROR_MISSING_MAIN"

unexpectedTuple :: String
unexpectedTuple = "ERROR_UNEXPECTED_TUPLE"

unexpectedRecord :: String
unexpectedRecord = "ERROR_UNEXPECTED_RECORD"

unexpectedFieldAccess :: String
unexpectedFieldAccess = "ERROR_UNEXPECTED_FIELD_ACCESS"

tupleIndexOutOfBounds :: String
tupleIndexOutOfBounds = "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS"

missingRecordFields :: String
missingRecordFields = "ERROR_MISSING_RECORD_FIELDS"

unexpectedRecordFields :: String
unexpectedRecordFields = "ERROR_UNEXPECTED_RECORD_FIELDS"

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
-- Unit expr
-- T-unit
inferTypeExpression _ AbsSyntax.ConstUnit = Right AbsSyntax.TypeUnit
-- Tuple expr
-- T-Tuple
inferTypeExpression context (AbsSyntax.Tuple elements)
  | null elements = Left notATuple
  | otherwise = do
      elementsType <- mapM (inferTypeExpression context) elements
      pure (AbsSyntax.TypeTuple elementsType)
-- T-Proj
inferTypeExpression context (AbsSyntax.DotTuple tuple n) = do
  tupleType <- inferTypeExpression context tuple
  case tupleType of
    (AbsSyntax.TypeTuple elementsType) ->
      case nthElement n elementsType of
        Nothing -> Left tupleIndexOutOfBounds
        Just typeNth -> pure typeNth
    _ -> Left notATuple
-- Record expr
-- T-Record
inferTypeExpression context (AbsSyntax.Record bindings) = do
  bindingsType <-
    mapM
      ( \(AbsSyntax.ABinding ident expr) -> do
          exprType <- inferTypeExpression context expr
          pure (AbsSyntax.ARecordFieldType ident exprType)
      )
      bindings
  pure (AbsSyntax.TypeRecord bindingsType)
-- T-Record-Proj
inferTypeExpression context (AbsSyntax.DotRecord record ident) = do
  recordType <- inferTypeExpression context record
  case recordType of
    (AbsSyntax.TypeRecord bindings) -> case Data.List.find (\(AbsSyntax.ARecordFieldType fieldName _) -> fieldName == ident) bindings of
      Nothing -> Left unexpectedFieldAccess
      Just (AbsSyntax.ARecordFieldType _ fieldType) -> Right fieldType
    _ -> Left notARecord
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
-- Unit expr
-- T-unit
checkTypeExpression _ AbsSyntax.ConstUnit expectedType = if expectedType == AbsSyntax.TypeUnit then Right () else Left unexpectedTypeForExpression
-- Tuple expr
-- T-Tuple
checkTypeExpression context (AbsSyntax.Tuple elements) expectedType = do
  case expectedType of
    (AbsSyntax.TypeTuple elementsType) ->
      if length elements /= length elementsType
        then Left unexpectedTuple
        else do
          Control.Monad.zipWithM_ (checkTypeExpression context) elements elementsType
    _ -> Left unexpectedTuple
-- T-Proj
checkTypeExpression context (AbsSyntax.DotTuple tuple n) expectedType = do
  tupleType <- inferTypeExpression context tuple
  case tupleType of
    (AbsSyntax.TypeTuple elementsType) ->
      case nthElement n elementsType of
        Nothing -> Left tupleIndexOutOfBounds
        Just typeNth -> if expectedType == typeNth then Right () else Left unexpectedTypeForExpression
    _ -> Left notATuple
-- Record expr
-- T-Record
checkTypeExpression context (AbsSyntax.Record bindings) expectedType =
  case expectedType of
    AbsSyntax.TypeRecord bindingsType -> do
      let exprMap :: M.Map AbsSyntax.StellaIdent AbsSyntax.Expr
          exprMap =
            M.fromList
              [ (ident, expr)
                | AbsSyntax.ABinding ident expr <- bindings
              ]

      let typeMap :: M.Map AbsSyntax.StellaIdent AbsSyntax.Type
          typeMap =
            M.fromList
              [ (ident, ty)
                | AbsSyntax.ARecordFieldType ident ty <- bindingsType
              ]

      let exprKeys = M.keysSet exprMap
          typeKeys = M.keysSet typeMap

      let extraInExpr = S.difference exprKeys typeKeys
      if not (S.null extraInExpr)
        then Left unexpectedRecordFields 
        else do
          let extraInType = S.difference typeKeys exprKeys
          if not (S.null extraInType)
            then Left missingRecordFields
            else do
              forM_ (M.toList exprMap) $ \(ident, expr) ->
                case M.lookup ident typeMap of
                  Nothing -> Left "impossible"
                  Just ty -> checkTypeExpression context expr ty
    _ -> Left unexpectedRecord
-- T-Record-Proj
checkTypeExpression context (AbsSyntax.DotRecord record ident) expectedType = do
  recordType <- inferTypeExpression context record
  case recordType of
    (AbsSyntax.TypeRecord bindings) -> case Data.List.find (\(AbsSyntax.ARecordFieldType fieldName _) -> fieldName == ident) bindings of
      Nothing -> Left unexpectedFieldAccess
      Just (AbsSyntax.ARecordFieldType _ fieldType) -> if fieldType == expectedType then Right () else Left unexpectedTypeForExpression
    _ -> Left notARecord
checkTypeExpression _ _ _ = Left "unsupported"

checkDeclarations :: Context -> [AbsSyntax.Decl] -> Either String ()
checkDeclarations _ [] = Right ()
checkDeclarations programContext ((AbsSyntax.DeclFun _ _ params (AbsSyntax.SomeReturnType returnType) _ _ expr) : xs) = do
  let functionContext = foldl (\context (AbsSyntax.AParamDecl (AbsSyntax.StellaIdent varName) varType) -> HM.insert varName varType context) programContext params
  checkTypeExpression functionContext expr returnType
  checkDeclarations programContext xs
checkDeclarations _ _ = Left "Unsupported declaration"

collectDeclarations :: [AbsSyntax.Decl] -> Either String Context
collectDeclarations [] = Right HM.empty
collectDeclarations ((AbsSyntax.DeclFun _ (AbsSyntax.StellaIdent name) [AbsSyntax.AParamDecl _ paramType] (AbsSyntax.SomeReturnType returnType) _ _ _) : xs) = do
  tailContext <- collectDeclarations xs
  pure $ HM.insert name (AbsSyntax.TypeFun [paramType] returnType) tailContext
collectDeclarations _ = Left "Unsupported declaration"

typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck (AbsSyntax.AProgram _ _ declarations) = do
  programContext <- collectDeclarations declarations
  case HM.lookup "main" programContext of
    Just _ -> checkDeclarations programContext declarations
    Nothing -> Left missingMain
