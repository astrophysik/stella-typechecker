module TypeCheck.Expr
  ( inferTypeExpression,
    checkTypeExpression,
  )
where

import Control.Monad (forM_, when)
import qualified Control.Monad (zipWithM_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.Common (Context, extendContext, hasDuplicateBy, nthElement)
import TypeCheck.Errors

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
-- T-Rec
inferTypeExpression context (AbsSyntax.NatRec n z s) = do
  checkTypeExpression context n AbsSyntax.TypeNat
  zType <- inferTypeExpression context z
  checkTypeExpression context s (AbsSyntax.TypeFun [AbsSyntax.TypeNat] (AbsSyntax.TypeFun [zType] zType))
  pure zType
-- Lambda expressions
-- T-Var
inferTypeExpression context (AbsSyntax.Var (AbsSyntax.StellaIdent var)) = case HM.lookup var context of
  Just tValue -> Right tValue
  Nothing -> Left undefinedVariable
-- T-Abs
inferTypeExpression context (AbsSyntax.Abstraction [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent var) varType] body) = do
  bodyType <- inferTypeExpression (HM.insert var varType context) body
  pure $ AbsSyntax.TypeFun [varType] bodyType
inferTypeExpression _ (AbsSyntax.Abstraction [] _) = Left "unsupported function with zero arguments"
inferTypeExpression _ (AbsSyntax.Abstraction (_ : _ : _) _) = Left "unsupported function with several arguments"
-- T-App
inferTypeExpression context (AbsSyntax.Application function [argument]) = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [varType] bodyType) -> do
      case checkTypeExpression context argument varType of
        Left _ -> Left unexpectedTypeForParam
        Right _ -> pure bodyType
    _ -> Left notAFunction
inferTypeExpression _ (AbsSyntax.Application _ []) = Left "unsupported apply function with zero arguments"
inferTypeExpression _ (AbsSyntax.Application _ (_ : _ : _)) = Left "unsupported apply function with several arguments"
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
  when (hasDuplicateBy (\(AbsSyntax.ABinding lhs _) (AbsSyntax.ABinding rhs _) -> lhs == rhs) bindings) $
    Left dublicateRecordFields
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
-- Let bindings
-- T-Let
inferTypeExpression context (AbsSyntax.Let bindings expr) = do
  case bindings of
    [AbsSyntax.APatternBinding (AbsSyntax.PatternVar (AbsSyntax.StellaIdent name)) value] -> do
      valueType <- inferTypeExpression context value
      inferTypeExpression (HM.insert name valueType context) expr
    _ -> Left "let with several arguments is unsupported"
-- Type Ascriptions
inferTypeExpression context (AbsSyntax.TypeAsc expr exprType) = do
  checkTypeExpression context expr exprType
  pure exprType
-- Type Sum
-- T-inl
inferTypeExpression _ (AbsSyntax.Inl _) = Left ambiguousSumType
-- T-inr
inferTypeExpression _ (AbsSyntax.Inr _) = Left ambiguousSumType
-- T-Case
inferTypeExpression context (AbsSyntax.Match expr matchCases) = do
  exprType <- inferTypeExpression context expr
  case exprType of
    (AbsSyntax.TypeSum leftType rightType) -> case matchCases of
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [] -> Left illegalEmptyMatching
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInl _) _] -> Left nonExhaustiveMatchPatterns
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInr _) _] -> Left nonExhaustiveMatchPatterns
      _ -> Left unepxectedPatternForType
    _ -> Left unexpectedTypeForExpression
  where
    inferSum (leftVarName, leftVarType, leftExpr) (rightVarName, rightVarType, rightExpr) = do
      leftExprType <- inferTypeExpression (HM.insert leftVarName leftVarType context) leftExpr
      checkTypeExpression (HM.insert rightVarName rightVarType context) rightExpr leftExprType
      pure leftExprType
-- List expressions
inferTypeExpression context (AbsSyntax.List (h : t)) = do
  headType <- inferTypeExpression context h
  mapM_
    ( \element -> do
        checkTypeExpression context element headType
    )
    t
  pure (AbsSyntax.TypeList headType)
-- T-Nil
inferTypeExpression _ (AbsSyntax.List []) = Left ambiguousList
-- T-Cons
inferTypeExpression context (AbsSyntax.ConsList listHead listTail) = do
  headType <- inferTypeExpression context listHead
  checkTypeExpression context listTail (AbsSyntax.TypeList headType)
  pure headType
-- T-IsNil
inferTypeExpression context (AbsSyntax.IsEmpty list) = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList _) -> pure AbsSyntax.TypeBool
    _ -> Left notAList
-- T-Head
inferTypeExpression context (AbsSyntax.Head list) = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList headType) -> pure headType
    _ -> Left notAList
-- T-Tail
inferTypeExpression context (AbsSyntax.Tail list) = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList _) -> pure listType
    _ -> Left notAList
-- fix expr
-- T-Fix
inferTypeExpression context (AbsSyntax.Fix function) = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] returnType) ->
      if paramType == returnType
        then Right returnType
        else Left unexpectedTypeForExpression
    _ -> Left notAFunction
inferTypeExpression _ _ = Left "unsupported"

-- Type check
checkTypeExpression :: Context -> AbsSyntax.Expr -> AbsSyntax.Type -> Either String ()
-- Boolean expressions
-- T-False
checkTypeExpression _ AbsSyntax.ConstFalse expectedType = case expectedType of
  AbsSyntax.TypeBool -> Right ()
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeBool expectedType AbsSyntax.ConstFalse
-- T-True
checkTypeExpression _ AbsSyntax.ConstTrue expectedType = case expectedType of
  AbsSyntax.TypeBool -> Right ()
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeBool expectedType AbsSyntax.ConstTrue
-- T-If
checkTypeExpression context (AbsSyntax.If condition onTrue onFalse) expectedType = do
  checkTypeExpression context condition AbsSyntax.TypeBool
  checkTypeExpression context onTrue expectedType
  checkTypeExpression context onFalse expectedType
  pure ()
-- Integer expressions
-- T-Zero
checkTypeExpression _ expr@(AbsSyntax.ConstInt _) expectedType = case expectedType of
  AbsSyntax.TypeNat -> Right ()
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeNat expectedType expr
-- T-Succ
checkTypeExpression context expr@(AbsSyntax.Succ integer) expectedType = case expectedType of
  AbsSyntax.TypeNat -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeNat expectedType expr
-- T-Pred
checkTypeExpression context expr@(AbsSyntax.Pred integer) expectedType = case expectedType of
  AbsSyntax.TypeNat -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeNat expectedType expr
-- T-IsZero
checkTypeExpression context expr@(AbsSyntax.IsZero integer) expectedType = case expectedType of
  AbsSyntax.TypeBool -> checkTypeExpression context integer AbsSyntax.TypeNat
  _ -> Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeBool expectedType expr
-- T-Rec
checkTypeExpression context (AbsSyntax.NatRec n z s) expectedType = do
  checkTypeExpression context n AbsSyntax.TypeNat
  checkTypeExpression context z expectedType
  checkTypeExpression context s (AbsSyntax.TypeFun [AbsSyntax.TypeNat] (AbsSyntax.TypeFun [expectedType] expectedType))
  pure ()
-- Lambda expressions
-- T-Var
checkTypeExpression context var@(AbsSyntax.Var _) expectedType = do
  varType <- inferTypeExpression context var
  if varType == expectedType then Right () else Left $ formatUnexpectedTypeForExpressionMsg varType expectedType var
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
      Left $ unexpectedTypeForParam ++ "\nexpected type\n\t" ++ show paramType ++ "\nbut got\n\t" ++ show varType ++ "\nfor param " ++ show varIdent
checkTypeExpression _ expr@(AbsSyntax.Abstraction _ _) expectedType = Left $ unexpectedLambda 
  ++ "\nExpected an expression of a non-function type\n\t" ++ show expectedType ++ "\nbut got an anonymous function\n\t" ++ show expr
-- T-App
checkTypeExpression context expr@(AbsSyntax.Application function [argument]) expectedType = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] resultType) -> do
      checkTypeExpression context argument paramType
      if resultType == expectedType then pure () else Left $ formatUnexpectedTypeForExpressionMsg resultType expectedType expr
    _ -> Left $ notAFunction ++ "\nexpected a function type but got\n\t" ++ show functionType ++ "\nfor the expression\n\t" ++ show function ++ "\nin the function call\n\t" ++ show expr
-- Unit expr
-- T-unit
checkTypeExpression _ AbsSyntax.ConstUnit expectedType = if expectedType == AbsSyntax.TypeUnit then Right () else Left $ formatUnexpectedTypeForExpressionMsg AbsSyntax.TypeUnit expectedType AbsSyntax.ConstUnit
-- Tuple expr
-- T-Tuple
checkTypeExpression context expr@(AbsSyntax.Tuple elements) expectedType = do
  case expectedType of
    (AbsSyntax.TypeTuple elementsType) ->
      if length elements /= length elementsType
        then Left $ unexpectedTupleLength ++ "\nexpected " ++ show (length elementsType) 
          ++ " components\n\t" ++ show expectedType ++ "\nbut got " ++ show (length elements) ++ "\n\t" ++ show expr 
        else do
          Control.Monad.zipWithM_ (checkTypeExpression context) elements elementsType
    _ -> Left $ unexpectedTuple ++ "\nexpected an expression of a non-tuple type\n\t" ++ show expectedType ++ "\nbut got a tuple\n\t" ++ show expr
-- T-Proj
checkTypeExpression context expr@(AbsSyntax.DotTuple tuple n) expectedType = do
  tupleType <- inferTypeExpression context tuple
  case tupleType of
    (AbsSyntax.TypeTuple elementsType) ->
      case nthElement n elementsType of
        Nothing -> Left $ tupleIndexOutOfBounds ++ "\nunexpected access to component number " ++ show n ++ "\nin a tuple\n\t" ++ show tuple
         ++ "\nof length " ++ show (length elementsType)
        Just typeNth -> if expectedType == typeNth then Right () else Left $ formatUnexpectedTypeForExpressionMsg typeNth expectedType expr
    _ -> Left $ notATuple ++ "\nexpected an expression of tuple type\nbut got expression\n\t" ++ show tuple ++ "\n of type\n\t"
     ++ show tupleType ++ "\nin expression\n\t" ++ show expr
-- Record expr
-- T-Record
checkTypeExpression context recordExpr@(AbsSyntax.Record bindings) expectedType =
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
        then Left $ unexpectedRecordFields ++ "\nunexpected fields\n\t" ++ show extraInExpr ++ "\nfor an expected record of type\n\t"
         ++ show expectedType ++ "\nin the record\n\t" ++ show recordExpr
        else do
          let extraInType = S.difference typeKeys exprKeys
          if not (S.null extraInType)
            then Left $ missingRecordFields ++ "\nmissing fields\n\t" ++ show extraInType ++ "\nfor an expected record type\n\t" 
              ++ show expectedType ++ "\nin the record\n\t" ++ show recordExpr
            else do
              forM_ (M.toList exprMap) $ \(ident, expr) ->
                case M.lookup ident typeMap of
                  Nothing -> Left "Typechecker internal error : missing ident in typeMap"
                  Just ty -> checkTypeExpression context expr ty
    _ -> Left $ unexpectedRecord ++ "\nexpected an expression of a non-record type\n\t" ++ show expectedType ++ "\nbut got a record\n\t" ++ show recordExpr
-- T-Record-Proj
checkTypeExpression context expr@(AbsSyntax.DotRecord record ident) expectedType = do
  recordType <- inferTypeExpression context record
  case recordType of
    (AbsSyntax.TypeRecord bindings) -> case Data.List.find (\(AbsSyntax.ARecordFieldType fieldName _) -> fieldName == ident) bindings of
      Nothing -> Left $ unexpectedFieldAccess ++ "\nunexpected access to field some in a record of type\n\t" ++ show recordType ++ "\nin the expression\n\t" ++ show expr
      Just (AbsSyntax.ARecordFieldType _ fieldType) -> if fieldType == expectedType then Right () else Left unexpectedTypeForExpression
    _ -> Left $ notARecord ++ "\nexpected a record type but got\n\t" ++ show recordType ++ "\nfor the expression\n\t" ++ show record ++ "\nin the expression\n\t" ++ show expr
-- Let bindings
-- T-Let
checkTypeExpression content (AbsSyntax.Let bindings expr) expectedType = do
  case bindings of
    [AbsSyntax.APatternBinding (AbsSyntax.PatternVar (AbsSyntax.StellaIdent name)) value] -> do
      valueType <- inferTypeExpression content value
      checkTypeExpression (HM.insert name valueType content) expr expectedType
    _ -> Left "Typechecker internal error : let with several arguments is unsupported"
-- Type Ascriptions
checkTypeExpression context (AbsSyntax.TypeAsc expr exprType) expectedType = do
  checkTypeExpression context expr exprType
  if expectedType == exprType then Right () else Left unexpectedTypeForExpression
-- Type Sum
-- T-inl
checkTypeExpression context (AbsSyntax.Inl inlExpr) expectedType =
  case expectedType of
    (AbsSyntax.TypeSum inlType _) -> checkTypeExpression context inlExpr inlType
    _ -> Left unexpectedInjection
-- T-inr
checkTypeExpression context (AbsSyntax.Inr inrExpr) expectedType =
  case expectedType of
    (AbsSyntax.TypeSum _ inrType) -> checkTypeExpression context inrExpr inrType
    _ -> Left unexpectedInjection
-- T-Case
checkTypeExpression context (AbsSyntax.Match expr matchCases) expectedType = do
  exprType <- inferTypeExpression context expr
  case exprType of
    (AbsSyntax.TypeSum leftType rightType) -> case matchCases of
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [] -> Left illegalEmptyMatching
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInl _) _] -> Left nonExhaustiveMatchPatterns
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInr _) _] -> Left nonExhaustiveMatchPatterns
      _ -> Left unepxectedPatternForType
    _ -> Left unepxectedPatternForType
  where
    inferSum (leftVarName, leftVarType, leftExpr) (rightVarName, rightVarType, rightExpr) = do
      checkTypeExpression (HM.insert leftVarName leftVarType context) leftExpr expectedType
      checkTypeExpression (HM.insert rightVarName rightVarType context) rightExpr expectedType
      pure ()
-- List expressions
checkTypeExpression context (AbsSyntax.List (h : t)) expectedType = case expectedType of
  (AbsSyntax.TypeList typeElement) -> do
    checkTypeExpression context h typeElement
    mapM_
      ( \element -> do
          checkTypeExpression context element typeElement
      )
      t
  _ -> Left unexpectedList
-- T-Nil
checkTypeExpression context (AbsSyntax.List []) expectedType = case expectedType of
  (AbsSyntax.TypeList _) -> Right ()
  _ -> Left unexpectedList
-- T-Cons
checkTypeExpression context (AbsSyntax.ConsList listHead listTail) expectedType = case expectedType of
  (AbsSyntax.TypeList elementType) -> do
    checkTypeExpression context listHead elementType
    checkTypeExpression context listTail expectedType
    pure ()
  _ -> Left unexpectedList
-- T-IsNil
checkTypeExpression context (AbsSyntax.IsEmpty list) expectedType = case expectedType of
  AbsSyntax.TypeBool -> do
    listType <- inferTypeExpression context list
    case listType of
      (AbsSyntax.TypeList _) -> Right ()
      _ -> Left notAList
  _ -> Left unexpectedTypeForExpression
-- T-Head
checkTypeExpression context (AbsSyntax.Head list) expectedType = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList elementType) -> if expectedType == elementType then Right () else Left unexpectedTypeForExpression
    _ -> Left notAList
-- T-Tail
checkTypeExpression context (AbsSyntax.Tail list) expectedType = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList _) -> if expectedType == listType then Right () else Left unexpectedTypeForExpression
    _ -> Left notAList
-- fix expr
-- T-Fix
checkTypeExpression context (AbsSyntax.Fix function) expectedType = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] returnType) ->
      if paramType == returnType
        then Right ()
        else Left unexpectedTypeForExpression
    _ -> Left notAFunction
checkTypeExpression _ _ _ = Left "unsupported"
