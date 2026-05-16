{-# LANGUAGE MultiWayIf #-}

module TypeCheck.ConstraintBased.Typing
  ( inferTypeExpression,
    Infer,
    ConstraintSet,
    freshTypeVar,
    withVar,
  )
where

import Control.Monad.State
import Control.Monad (forM_, when, unless)
import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.ConstraintBased.Context
  ( Context,
    incTypeVarId,
    lookupVar,
    typeVarId,
    variableContext,
  )
import TypeCheck.Errors
import qualified Data.List
import TypeCheck.Common(hasDuplicateBy)

type ConstraintEquation = (AbsSyntax.Type, AbsSyntax.Type)

type ConstraintSet = [ConstraintEquation]

type Infer a = StateT Context (Either String) a

freshTypeVar :: Infer AbsSyntax.Type
freshTypeVar = do
  ctx <- get
  put $ incTypeVarId ctx
  pure $ AbsSyntax.TypeVar $ AbsSyntax.StellaIdent $ "T" ++ show (typeVarId ctx)

withVar :: String -> AbsSyntax.Type -> Infer a -> Infer a
withVar varName varType action = do
  oldVars <- gets variableContext
  modify $ \ctx -> ctx {variableContext = HM.insert varName varType oldVars}
  result <- action
  modify $ \ctx -> ctx {variableContext = oldVars}
  pure result

inferTypeExpression :: AbsSyntax.Expr -> Infer (AbsSyntax.Type, ConstraintSet)
-- Boolean expressions
inferTypeExpression AbsSyntax.ConstFalse = pure (AbsSyntax.TypeBool, [])
inferTypeExpression AbsSyntax.ConstTrue = pure (AbsSyntax.TypeBool, [])
inferTypeExpression (AbsSyntax.If condition onTrue onFalse) = do
  (conditionType, c1) <- inferTypeExpression condition
  (onTrueType, c2) <- inferTypeExpression onTrue
  (onFalseType, c3) <- inferTypeExpression onFalse
  pure (onTrueType, c1 ++ c2 ++ c3 ++ [(conditionType, AbsSyntax.TypeBool), (onTrueType, onFalseType)])
-- integer expressions
inferTypeExpression (AbsSyntax.ConstInt _) = pure (AbsSyntax.TypeNat, [])
inferTypeExpression (AbsSyntax.Succ arg) = do
  (argType, c) <- inferTypeExpression arg
  pure (AbsSyntax.TypeNat, c ++ [(argType, AbsSyntax.TypeNat)])
inferTypeExpression (AbsSyntax.Pred arg) = do
  (argType, c) <- inferTypeExpression arg
  pure (AbsSyntax.TypeNat, c ++ [(argType, AbsSyntax.TypeNat)])
inferTypeExpression (AbsSyntax.IsZero arg) = do
  (argType, c) <- inferTypeExpression arg
  pure (AbsSyntax.TypeBool, c ++ [(argType, AbsSyntax.TypeNat)])
inferTypeExpression (AbsSyntax.NatRec n z s) = do
  (nType, c1) <- inferTypeExpression n
  (zType, c2) <- inferTypeExpression z
  (sType, c3) <- inferTypeExpression s
  pure (zType, c1 ++ c2 ++ c3 ++ [(nType, AbsSyntax.TypeNat), (sType, AbsSyntax.TypeFun [AbsSyntax.TypeNat] (AbsSyntax.TypeFun [zType] zType))])
-- Lambda expressions
inferTypeExpression (AbsSyntax.Var (AbsSyntax.StellaIdent var)) = do
  maybeType <- gets (lookupVar var)
  case maybeType of
    Just tValue -> pure (tValue, [])
    Nothing -> lift $ Left $ undefinedVariable ++ "\nundefined variable " ++ show var
inferTypeExpression (AbsSyntax.Abstraction [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent var) varType] body) = do
  case varType of
    AbsSyntax.TypeAuto -> do
      argType <- freshTypeVar
      (bodyType, c) <- withVar var argType $ inferTypeExpression body
      pure (AbsSyntax.TypeFun [argType] bodyType, c)
    _ -> do
      (bodyType, c) <- withVar var varType $ inferTypeExpression body
      pure (AbsSyntax.TypeFun [varType] bodyType, c)
inferTypeExpression (AbsSyntax.Abstraction [] _) = lift $ Left "unsupported function with zero arguments"
inferTypeExpression (AbsSyntax.Abstraction (_ : _ : _) _) = lift $ Left "unsupported function with several arguments"
inferTypeExpression expr@(AbsSyntax.Application function [argument]) = do
  (functionType, c1) <- inferTypeExpression function
  (argType, c2) <- inferTypeExpression argument
  typeVar <- freshTypeVar
  if
    | AbsSyntax.TypeVar _ <- functionType ->
        pure (typeVar, c1 ++ c2 ++ [(functionType, AbsSyntax.TypeFun [argType] typeVar)])
    | AbsSyntax.TypeFun _ _ <- functionType ->
        pure (typeVar, c1 ++ c2 ++ [(functionType, AbsSyntax.TypeFun [argType] typeVar)])
    | otherwise ->
        lift $
          Left $
            notAFunction
              ++ "\nexpected a function type but got\n\t"
              ++ show functionType
              ++ "\nfor the expression\n\t"
              ++ show function
              ++ "\nin the function call\n\t"
              ++ show expr
-- Unit Type
inferTypeExpression AbsSyntax.ConstUnit = pure (AbsSyntax.TypeUnit, [])
-- Record
inferTypeExpression (AbsSyntax.Record bindings) = do
  when (hasDuplicateBy (\(AbsSyntax.ABinding lhs _) (AbsSyntax.ABinding rhs _) -> lhs == rhs) bindings) $
    lift $ Left $ dublicateRecordFields ++ "\nduplicate record fields\n\t" ++ show bindings
  (bindingsType, c) <- inferBindings bindings
  pure (AbsSyntax.TypeRecord bindingsType, c)
  where
    inferBindings [] = pure ([], [])
    inferBindings (AbsSyntax.ABinding ident expr : rest) = do
      (exprType, c1) <- inferTypeExpression expr
      (restTypes, c2) <- inferBindings rest
      pure (AbsSyntax.ARecordFieldType ident exprType : restTypes, c1 ++ c2) 
inferTypeExpression expr@(AbsSyntax.DotRecord record ident) = do
  (recordType, c) <- inferTypeExpression record
  case recordType of
    AbsSyntax.TypeRecord bindings -> case Data.List.find (\(AbsSyntax.ARecordFieldType fieldName _) -> fieldName == ident) bindings of
      Nothing -> lift $ Left $ unexpectedFieldAccess ++ "\nin expression\n\t" ++ show expr
      Just (AbsSyntax.ARecordFieldType _ fieldType) -> pure (fieldType, c)
    AbsSyntax.TypeVar _ -> lift $ Left $ "Cannot infer type for record\n\t" ++ show expr 
    _ -> lift $ Left $ notARecord ++ "\nexpected a record type but got\n\t" ++ show recordType ++ "\n\tin the expression\n\t" ++ show expr
-- Let Bindings
inferTypeExpression (AbsSyntax.Let bindings expr) = do
  case bindings of
    [AbsSyntax.APatternBinding (AbsSyntax.PatternVar (AbsSyntax.StellaIdent name)) value] -> do
      (valueType, c1) <- inferTypeExpression value
      (exprType, c2) <- withVar name valueType (inferTypeExpression expr)
      pure (exprType, c1 ++ c2)
    _ -> lift $ Left "Internal error : let with several arguments is unsupported"
-- Type Ascriptions
inferTypeExpression (AbsSyntax.TypeAsc expr exprType) = do
  (actualType, c) <- inferTypeExpression expr
  pure (exprType, c ++ [(actualType, exprType)])
-- Type Sum
inferTypeExpression (AbsSyntax.Inl inlExpr) = do
  (inlType, c) <- inferTypeExpression inlExpr
  inrType <- freshTypeVar
  pure (AbsSyntax.TypeSum inlType inrType, c)
inferTypeExpression (AbsSyntax.Inr inrExpr) = do
  (inrType, c) <- inferTypeExpression inrExpr
  inlType <- freshTypeVar
  pure (AbsSyntax.TypeSum inlType inrType, c)
inferTypeExpression matchExpr@(AbsSyntax.Match expr matchCases) = do
  case matchCases of
    [ AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent inlVarName))) inlExpr,
      AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent inrVarName))) inrExpr
      ] -> processSum (inlVarName, inlExpr) (inrVarName, inrExpr)
    [ AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent inrVarName))) inrExpr,
      AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent inlVarName))) inlExpr
      ] -> processSum (inlVarName, inlExpr) (inrVarName, inrExpr)
    [] -> lift $ Left $ illegalEmptyMatching ++ "\nin expression\n\t" ++ show matchExpr
    [AbsSyntax.AMatchCase (AbsSyntax.PatternInl _) _] -> lift $ Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinr"
    [AbsSyntax.AMatchCase (AbsSyntax.PatternInr _) _] -> lift $ Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinl"
  where
    processSum (inlVarName, inlExpr) (inrVarName, inrExpr) = do
      (exprType, exprC) <- inferTypeExpression expr
      inlType <- freshTypeVar
      (inlBranchType, inlC) <- withVar inlVarName inlType (inferTypeExpression inlExpr)
      inrType <- freshTypeVar
      (inrBranchType, inrC) <- withVar inrVarName inrType (inferTypeExpression inrExpr)
      pure (inlBranchType, exprC ++ inlC ++ inrC ++ [(inlBranchType, inrBranchType), (exprType, AbsSyntax.TypeSum inlType inrType)])
-- List expressions
inferTypeExpression (AbsSyntax.List (h : t)) = do
  (headType, c1) <- inferTypeExpression h
  (tailType, c2) <- inferTypeExpression (AbsSyntax.List t)
  pure (AbsSyntax.TypeList headType, c1 ++ c2 ++ [(AbsSyntax.TypeList headType, tailType)])
inferTypeExpression (AbsSyntax.List []) = do
  listType <- freshTypeVar
  pure (AbsSyntax.TypeList listType, [])
inferTypeExpression (AbsSyntax.ConsList listHead listTail) = do
  (headType, c1) <- inferTypeExpression listHead
  (tailType, c2) <- inferTypeExpression listTail
  pure (AbsSyntax.TypeList headType, c1 ++ c2 ++ [(AbsSyntax.TypeList headType, tailType)])
inferTypeExpression (AbsSyntax.IsEmpty list) = do
  (listType, c) <- inferTypeExpression list
  nestedListType <- freshTypeVar
  pure (AbsSyntax.TypeBool, c ++ [(AbsSyntax.TypeList nestedListType, listType)])
inferTypeExpression (AbsSyntax.Head list) = do
  (listType, c) <- inferTypeExpression list
  nestedListType <- freshTypeVar
  pure (nestedListType, c ++ [(AbsSyntax.TypeList nestedListType, listType)])
inferTypeExpression (AbsSyntax.Tail list) = do
  (listType, c) <- inferTypeExpression list
  nestedListType <- freshTypeVar
  pure (AbsSyntax.TypeList nestedListType, c ++ [(AbsSyntax.TypeList nestedListType, listType)])
-- fix expr
inferTypeExpression expr@(AbsSyntax.Fix function) = do
  (functionType, c) <- inferTypeExpression function
  returnType <- freshTypeVar
  if
    | AbsSyntax.TypeVar _ <- functionType ->
        pure (returnType, c ++ [(functionType, AbsSyntax.TypeFun [returnType] returnType)])
    | AbsSyntax.TypeFun _ _ <- functionType ->
        pure (returnType, c ++ [(functionType, AbsSyntax.TypeFun [returnType] returnType)])
    | otherwise ->
        lift $
          Left $
            notAFunction
              ++ "\nexpected a function type but got\n\t"
              ++ show functionType
              ++ "\nfor the expression\n\t"
              ++ show function
              ++ "\nin the function call\n\t"
              ++ show expr
inferTypeExpression expr = do 
  lift $ Left $ "unsupported expression type for type-recostruction\n\t" ++ show expr
