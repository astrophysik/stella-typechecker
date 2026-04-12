module TypeCheck.BidirectionalTyping
  ( inferTypeExpression,
    checkTypeExpression,
  )
where

import Control.Monad (forM_, when)
import qualified Control.Monad (zipWithM_)
import qualified Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.Common (hasDuplicateBy, nthElement, validateType)
import TypeCheck.Context (Context, insertVar, lookupVar, lookupException, isSubTypingEnabled)
import TypeCheck.Errors
import TypeCheck.SubTyping ((<:))

inferVariantCase :: Context -> M.Map AbsSyntax.StellaIdent AbsSyntax.OptionalTyping -> AbsSyntax.MatchCase -> Either String AbsSyntax.Type
inferVariantCase context fieldMap matchCase@(AbsSyntax.AMatchCase matchPattern expr) =
  case matchPattern of
    (AbsSyntax.PatternVariant label (AbsSyntax.SomePatternData pat)) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label ++ "\nin match case\n\t" ++ show matchCase
        Just (AbsSyntax.SomeTyping expectedType) -> do
          case pat of
            (AbsSyntax.PatternVar (AbsSyntax.StellaIdent varName)) ->
              inferTypeExpression (insertVar varName expectedType context) expr
            AbsSyntax.PatternUnit ->
              inferTypeExpression context expr
            _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show pat ++ "\nwhen pattern matching is expected for type\n\t" ++ show expectedType
        Just AbsSyntax.NoTyping -> Left $ unexpectedVariantLabel ++ "\nlabel " ++ show label ++ " expects no data but pattern has data"
    (AbsSyntax.PatternVariant label AbsSyntax.NoPatternData) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label
        Just AbsSyntax.NoTyping -> inferTypeExpression context expr
        Just (AbsSyntax.SomeTyping _) -> Left $ unexpectedVariantLabel ++ "\nlabel " ++ show label ++ " expects data but pattern has none"
    _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show matchPattern

checkVariantCase :: Context -> M.Map AbsSyntax.StellaIdent AbsSyntax.OptionalTyping -> AbsSyntax.MatchCase -> AbsSyntax.Type -> Either String ()
checkVariantCase context fieldMap (AbsSyntax.AMatchCase matchPattern expr) expectedType =
  case matchPattern of
    (AbsSyntax.PatternVariant label (AbsSyntax.SomePatternData pat)) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label
        Just (AbsSyntax.SomeTyping varType) ->
          case pat of
            (AbsSyntax.PatternVar (AbsSyntax.StellaIdent varName)) ->
              checkTypeExpression (insertVar varName varType context) expr expectedType
            AbsSyntax.PatternUnit ->
              checkTypeExpression context expr expectedType
            _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show pat ++ "\nwhen pattern matching is expected for type\n\t" ++ show varType
        Just AbsSyntax.NoTyping -> Left unexpectedVariantLabel
    (AbsSyntax.PatternVariant label AbsSyntax.NoPatternData) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label
        Just AbsSyntax.NoTyping -> checkTypeExpression context expr expectedType
        Just (AbsSyntax.SomeTyping _) -> Left unexpectedVariantLabel
    _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show matchPattern

checkVariantMatchCase :: Context -> M.Map AbsSyntax.StellaIdent AbsSyntax.OptionalTyping -> AbsSyntax.Type -> AbsSyntax.MatchCase -> Either String ()
checkVariantMatchCase context fieldMap expectedType (AbsSyntax.AMatchCase matchPattern expr) =
  case matchPattern of
    (AbsSyntax.PatternVariant label (AbsSyntax.SomePatternData pat)) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label
        Just (AbsSyntax.SomeTyping varType) ->
          case pat of
            (AbsSyntax.PatternVar (AbsSyntax.StellaIdent varName)) ->
              checkTypeExpression (insertVar varName varType context) expr expectedType
            AbsSyntax.PatternUnit ->
              checkTypeExpression context expr expectedType
            _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show pat ++ "\nwhen pattern matching is expected for type\n\t" ++ show varType
        Just AbsSyntax.NoTyping -> Left unexpectedVariantLabel
    (AbsSyntax.PatternVariant label AbsSyntax.NoPatternData) ->
      case M.lookup label fieldMap of
        Nothing -> Left $ unexpectedVariantLabel ++ "\nunexpected label\n\t" ++ show label
        Just AbsSyntax.NoTyping -> checkTypeExpression context expr expectedType
        Just (AbsSyntax.SomeTyping _) -> Left unexpectedVariantLabel
    _ -> Left $ unepxectedPatternForType ++ "\nunexpected pattern\n\t" ++ show matchPattern

compatible :: Context -> AbsSyntax.Type -> AbsSyntax.Type -> AbsSyntax.Expr -> Either String ()
compatible ctx t1 t2 expr
  | isSubTypingEnabled ctx = if t1 <: t2 then Right () else Left $ formatUnexpectedSubTypeMsg t1 t2 expr
  | otherwise = if t1 == t2 then Right () else Left $ formatUnexpectedTypeForExpressionMsg t1 t2 expr

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
  validateType tTrue
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
  validateType zType
  checkTypeExpression context s (AbsSyntax.TypeFun [AbsSyntax.TypeNat] (AbsSyntax.TypeFun [zType] zType))
  pure zType
-- Lambda expressions
-- T-Var
inferTypeExpression context (AbsSyntax.Var (AbsSyntax.StellaIdent var)) = case lookupVar var context of
  Just tValue -> Right tValue
  Nothing -> Left $ undefinedVariable ++ "\nundefined variable " ++ show var
-- T-Abs
inferTypeExpression context (AbsSyntax.Abstraction [AbsSyntax.AParamDecl (AbsSyntax.StellaIdent var) varType] body) = do
  bodyType <- inferTypeExpression (insertVar var varType context) body
  validateType bodyType
  pure $ AbsSyntax.TypeFun [varType] bodyType
inferTypeExpression _ (AbsSyntax.Abstraction [] _) = Left "unsupported function with zero arguments"
inferTypeExpression _ (AbsSyntax.Abstraction (_ : _ : _) _) = Left "unsupported function with several arguments"
-- T-App
inferTypeExpression context expr@(AbsSyntax.Application function [argument]) = do
  functionType <- inferTypeExpression context function
  validateType functionType
  case functionType of
    (AbsSyntax.TypeFun [varType] bodyType) -> do
      case checkTypeExpression context argument varType of
        Left msg -> Left msg
        Right _ -> pure bodyType
    _ -> Left $ notAFunction ++ "\nexpected a function type but got\n\t" ++ show functionType ++ "\nfor the expression\n\t" ++ show function ++ "\nin the function call\n\t" ++ show expr
inferTypeExpression _ (AbsSyntax.Application _ []) = Left "Internal Error : unsupported apply function with zero arguments"
inferTypeExpression _ (AbsSyntax.Application _ (_ : _ : _)) = Left "Internal Error : unsupported apply function with several arguments"
-- Unit expr
-- T-unit
inferTypeExpression _ AbsSyntax.ConstUnit = Right AbsSyntax.TypeUnit
-- Tuple expr
-- T-Tuple
inferTypeExpression context (AbsSyntax.Tuple elements)
  | null elements = Left notATuple
  | otherwise = do
      elementsType <- mapM (inferTypeExpression context) elements
      mapM_ validateType elementsType
      pure (AbsSyntax.TypeTuple elementsType)
-- T-Proj
inferTypeExpression context (AbsSyntax.DotTuple tuple n) = do
  tupleType <- inferTypeExpression context tuple
  validateType tupleType
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
    Left $
      dublicateRecordFields ++ "\nduplicate record fields\n\t" ++ show bindings
  bindingsType <-
    mapM
      ( \(AbsSyntax.ABinding ident expr) -> do
          exprType <- inferTypeExpression context expr
          pure (AbsSyntax.ARecordFieldType ident exprType)
      )
      bindings
  let recordType = AbsSyntax.TypeRecord bindingsType
  validateType recordType
  pure recordType
-- T-Record-Proj
inferTypeExpression context (AbsSyntax.DotRecord record ident) = do
  recordType <- inferTypeExpression context record
  validateType recordType
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
      validateType valueType
      exprType <- inferTypeExpression (insertVar name valueType context) expr
      validateType exprType
      pure exprType
    _ -> Left "Internal error : let with several arguments is unsupported"
-- Type Ascriptions
inferTypeExpression context (AbsSyntax.TypeAsc expr exprType) = do
  checkTypeExpression context expr exprType
  pure exprType
-- Type Sum
-- T-inl
inferTypeExpression _ expr@(AbsSyntax.Inl _) = Left $ ambiguousSumType ++ "\ntype inference for sum types is not supported\n\t" ++ show expr
-- T-inr
inferTypeExpression _ expr@(AbsSyntax.Inr _) = Left $ ambiguousSumType ++ "\ntype inference for sum types is not supported\n\t" ++ show expr
-- T-Variant
inferTypeExpression _ (AbsSyntax.Variant _ _) = Left $ ambiguousVariantType ++ "\ntype inference for variants is not supported"
-- T-Case
inferTypeExpression context matchExpr@(AbsSyntax.Match expr matchCases) = do
  exprType <- inferTypeExpression context expr
  validateType exprType
  case exprType of
    (AbsSyntax.TypeSum leftType rightType) -> case matchCases of
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> inferSum (leftName, leftType, leftExpr) (rightName, rightType, rightExpr)
      [] -> Left $ illegalEmptyMatching ++ "in expression\n\t" ++ show matchExpr
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInl _) _] -> Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinr"
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInr _) _] -> Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinl"
      matchPattern -> Left $ unepxectedPatternForType ++ "unexpected match pattern\n\t" ++ show matchPattern ++ "\nfor type\n\t" ++ show exprType
    (AbsSyntax.TypeVariant variantFields) -> do
      let fieldMap = M.fromList [(label, optType) | AbsSyntax.AVariantFieldType label optType <- variantFields]
      case matchCases of
        [] -> Left $ illegalEmptyMatching ++ "in expression\n\t" ++ show matchExpr
        (firstCase : restCase) -> do
          firstExprType <- inferVariantCase context fieldMap firstCase
          mapM_ (\c -> checkVariantCase context fieldMap c firstExprType) restCase
          let coveredLabels = S.fromList [label | AbsSyntax.AMatchCase (AbsSyntax.PatternVariant label _) _ <- matchCases]
          let allLabels = M.keysSet fieldMap
          let missingLabels = S.difference allLabels coveredLabels
          if not (S.null missingLabels)
            then Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\t" ++ show missingLabels
            else pure firstExprType
    _ -> Left $ unexpectedTypeForExpression ++ "\nexpected variant of sum-type in match but got\n\t" ++ show exprType ++ "\nin expression\n\t" ++ show matchExpr
  where
    inferSum (leftVarName, leftVarType, leftExpr) (rightVarName, rightVarType, rightExpr) = do
      leftExprType <- inferTypeExpression (insertVar leftVarName leftVarType context) leftExpr
      validateType leftExprType
      checkTypeExpression (insertVar rightVarName rightVarType context) rightExpr leftExprType
      pure leftExprType
-- List expressions
inferTypeExpression context (AbsSyntax.List (h : t)) = do
  headType <- inferTypeExpression context h
  validateType headType
  mapM_
    ( \element -> do
        checkTypeExpression context element headType
    )
    t
  pure (AbsSyntax.TypeList headType)
-- T-Nil
inferTypeExpression _ (AbsSyntax.List []) = Left $ ambiguousList ++ "\ntype inference of empty lists is not supported"
-- T-Cons
inferTypeExpression context (AbsSyntax.ConsList listHead listTail) = do
  headType <- inferTypeExpression context listHead
  validateType headType
  checkTypeExpression context listTail (AbsSyntax.TypeList headType)
  pure $ AbsSyntax.TypeList headType
-- T-IsNil
inferTypeExpression context (AbsSyntax.IsEmpty list) = do
  listType <- inferTypeExpression context list
  validateType listType
  case listType of
    (AbsSyntax.TypeList _) -> pure AbsSyntax.TypeBool
    _ -> Left notAList
-- T-Head
inferTypeExpression context (AbsSyntax.Head list) = do
  listType <- inferTypeExpression context list
  validateType listType
  case listType of
    (AbsSyntax.TypeList headType) -> pure headType
    _ -> Left notAList
-- T-Tail
inferTypeExpression context (AbsSyntax.Tail list) = do
  listType <- inferTypeExpression context list
  validateType listType
  case listType of
    (AbsSyntax.TypeList _) -> pure listType
    _ -> Left notAList
-- fix expr
-- T-Fix
inferTypeExpression context (AbsSyntax.Fix function) = do
  functionType <- inferTypeExpression context function
  validateType functionType
  case functionType of
    (AbsSyntax.TypeFun [paramType] returnType) ->
      if paramType == returnType
        then Right returnType
        else Left unexpectedTypeForExpression
    _ -> Left notAFunction
-- sequence
-- T-seq
inferTypeExpression context (AbsSyntax.Sequence left right) = do
  checkTypeExpression context left AbsSyntax.TypeUnit
  resultType <- inferTypeExpression context right
  validateType resultType
  pure resultType
-- references
-- T-ref
inferTypeExpression context (AbsSyntax.Ref expr) = do
  exprType <- inferTypeExpression context expr
  validateType exprType
  pure $ AbsSyntax.TypeRef exprType
-- T-deref
inferTypeExpression context (AbsSyntax.Deref expr) = do
  exprType <- inferTypeExpression context expr
  validateType exprType
  case exprType of
    (AbsSyntax.TypeRef innerType) -> pure innerType
    _ ->
      Left $
        notAReference
          ++ "\ncannot dereference an expression\n\t"
          ++ show expr
          ++ "\nof a non-reference type\n\t"
          ++ show exprType
-- T-assign
inferTypeExpression context (AbsSyntax.Assign lhs rhs) = do
  lhsRefType <- inferTypeExpression context lhs
  validateType lhsRefType
  case lhsRefType of
    (AbsSyntax.TypeRef lhsType) -> do
      checkTypeExpression context rhs lhsType
      pure AbsSyntax.TypeUnit
    _ ->
      Left $
        notAReference
          ++ "\ncannot assign into expression\n\t"
          ++ show lhs
          ++ "\nof a non-reference type\n\t"
          ++ show lhsRefType
-- T-loc
inferTypeExpression _ (AbsSyntax.ConstMemory _) = Left $ ambiguousReferenceType ++ "\ncannot infer a type of a bare memory address"
-- T-Error
inferTypeExpression _ AbsSyntax.Panic = Left $ ambiguousPanicType ++ "\ncannot infer a type of a panic"
-- T-Raise
inferTypeExpression context (AbsSyntax.Throw expr) = do
  case lookupException context of
    Nothing -> Left $ exceptionTypeNotDeclared ++ "\ncannot throw exceptions, because exception type is not declared"
    Just _ -> do
      _ <- inferTypeExpression context expr
      Left $ ambiguousThrowType ++ "\ncannot infer type for throw"
-- T-TryWith
inferTypeExpression context (AbsSyntax.TryWith mainBranch fallbackBranch) = do
  mainBranchType <- inferTypeExpression context mainBranch
  checkTypeExpression context fallbackBranch mainBranchType
  pure mainBranchType
-- T-TryCatch
inferTypeExpression context (AbsSyntax.TryCatch mainBranch catchPattern fallbackExpr) = do
  mainBranchType <- inferTypeExpression context mainBranch
  case lookupException context of
    Nothing ->
      Left $
        exceptionTypeNotDeclared ++ "\ncannot typecheck catch, because exception type is not declared"
    Just exceptionType -> do
      case catchPattern of
        (AbsSyntax.PatternVar (AbsSyntax.StellaIdent varName)) -> do
          checkTypeExpression (insertVar varName exceptionType context) fallbackExpr mainBranchType
          pure mainBranchType
        _ -> Left "Internal error : unsupported catch pattern"
-- T-Cast
inferTypeExpression context (AbsSyntax.TypeCast expr typeToCast) = do 
  _ <- inferTypeExpression context expr
  pure typeToCast
inferTypeExpression _ expr = Left $ "Internal error : unsupported type inference for expr\n\t" ++ show expr

-- Type check
checkTypeExpression :: Context -> AbsSyntax.Expr -> AbsSyntax.Type -> Either String ()
-- Boolean expressions
-- T-False
checkTypeExpression context AbsSyntax.ConstFalse expectedType =
  compatible context AbsSyntax.TypeBool expectedType AbsSyntax.ConstFalse
-- T-True
checkTypeExpression context AbsSyntax.ConstTrue expectedType =
  compatible context AbsSyntax.TypeBool expectedType AbsSyntax.ConstTrue
-- T-If
checkTypeExpression context (AbsSyntax.If condition onTrue onFalse) expectedType = do
  checkTypeExpression context condition AbsSyntax.TypeBool
  checkTypeExpression context onTrue expectedType
  checkTypeExpression context onFalse expectedType
  pure ()
-- Integer expressions
-- T-Zero
checkTypeExpression context expr@(AbsSyntax.ConstInt _) expectedType =
  compatible context AbsSyntax.TypeNat expectedType expr
-- T-Succ
checkTypeExpression context expr@(AbsSyntax.Succ integer) expectedType = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  compatible context AbsSyntax.TypeNat expectedType expr
-- T-Pred
checkTypeExpression context expr@(AbsSyntax.Pred integer) expectedType = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  compatible context AbsSyntax.TypeNat expectedType expr
-- T-IsZero
checkTypeExpression context expr@(AbsSyntax.IsZero integer) expectedType = do
  checkTypeExpression context integer AbsSyntax.TypeNat
  compatible context AbsSyntax.TypeBool expectedType expr
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
  compatible context varType expectedType var
-- T-Abs
checkTypeExpression context expr@(AbsSyntax.Abstraction [AbsSyntax.AParamDecl varIdent@(AbsSyntax.StellaIdent varName) varType] body) (AbsSyntax.TypeFun [paramType] resultType) = do
  case compatible context paramType varType expr of -- case to print unexpectedTypeForParam instead of unexpectedTypeForExpr
    Left msg ->
      if isSubTypingEnabled context
        then Left msg
        else Left $ unexpectedTypeForParam ++ "\nexpected type\n\t" ++ show paramType ++ "\nbut got\n\t" ++ show varType ++ "\nfor param " ++ show varIdent
    Right _ -> checkTypeExpression (insertVar varName paramType context) body resultType
checkTypeExpression _ expr@(AbsSyntax.Abstraction _ _) expectedType =
  Left $
    unexpectedLambda
      ++ "\nExpected an expression of a non-function type\n\t"
      ++ show expectedType
      ++ "\nbut got an anonymous function\n\t"
      ++ show expr
-- T-App
checkTypeExpression context expr@(AbsSyntax.Application function [argument]) expectedType = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] resultType) -> do
      checkTypeExpression context argument paramType
      compatible context resultType expectedType expr
    _ -> Left $ notAFunction ++ "\nexpected a function type but got\n\t" ++ show functionType ++ "\nfor the expression\n\t" ++ show function ++ "\nin the function call\n\t" ++ show expr
-- Unit expr
-- T-unit
checkTypeExpression context AbsSyntax.ConstUnit expectedType =
  compatible context AbsSyntax.TypeUnit expectedType AbsSyntax.ConstUnit
-- Tuple expr
-- T-Tuple
checkTypeExpression context expr@(AbsSyntax.Tuple elements) expectedType = do
  case expectedType of
    (AbsSyntax.TypeTuple elementsType) ->
      if length elements /= length elementsType
        then
          Left $
            unexpectedTupleLength
              ++ "\nexpected "
              ++ show (length elementsType)
              ++ " components\n\t"
              ++ show expectedType
              ++ "\nbut got "
              ++ show (length elements)
              ++ "\n\t"
              ++ show expr
        else do
          Control.Monad.zipWithM_ (checkTypeExpression context) elements elementsType
    _ -> Left $ unexpectedTuple ++ "\nexpected an expression of a non-tuple type\n\t" ++ show expectedType ++ "\nbut got a tuple\n\t" ++ show expr
-- T-Proj
checkTypeExpression context expr@(AbsSyntax.DotTuple tuple n) expectedType = do
  tupleType <- inferTypeExpression context tuple
  case tupleType of
    (AbsSyntax.TypeTuple elementsType) ->
      case nthElement n elementsType of
        Nothing ->
          Left $
            tupleIndexOutOfBounds
              ++ "\nunexpected access to component number "
              ++ show n
              ++ "\nin a tuple\n\t"
              ++ show tuple
              ++ "\nof length "
              ++ show (length elementsType)
        Just typeNth -> compatible context typeNth expectedType expr
    _ ->
      Left $
        notATuple
          ++ "\nexpected an expression of tuple type\nbut got expression\n\t"
          ++ show tuple
          ++ "\n of type\n\t"
          ++ show tupleType
          ++ "\nin expression\n\t"
          ++ show expr
-- Record expr
-- T-Record
checkTypeExpression context recordExpr@(AbsSyntax.Record bindings) expectedType = do
  when (hasDuplicateBy (\(AbsSyntax.ABinding lhs _) (AbsSyntax.ABinding rhs _) -> lhs == rhs) bindings) $
    Left $
      dublicateRecordFields ++ "\nduplicate record fields\n\t" ++ show bindings
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
      if not (S.null extraInExpr) && not (isSubTypingEnabled context)
        then
          Left $
            unexpectedRecordFields
              ++ "\nunexpected fields\n\t"
              ++ show extraInExpr
              ++ "\nfor an expected record of type\n\t"
              ++ show expectedType
              ++ "\nin the record\n\t"
              ++ show recordExpr
        else do
          let extraInType = S.difference typeKeys exprKeys
          if not (S.null extraInType)
            then
              Left $
                missingRecordFields
                  ++ "\nmissing fields\n\t"
                  ++ show extraInType
                  ++ "\nfor an expected record type\n\t"
                  ++ show expectedType
                  ++ "\nin the record\n\t"
                  ++ show recordExpr
            else do
              forM_ (M.toList exprMap) $ \(ident, expr) ->
                case M.lookup ident typeMap of
                  Nothing ->
                    if isSubTypingEnabled context
                      then case inferTypeExpression context expr of
                        Right _ -> Right ()
                        Left msg -> Left msg
                      else Left "Typechecker internal error : missing ident in typeMap"
                  Just ty -> checkTypeExpression context expr ty
    _ -> Left $ unexpectedRecord ++ "\nexpected an expression of a non-record type\n\t" ++ show expectedType ++ "\nbut got a record\n\t" ++ show recordExpr
-- T-Record-Proj
checkTypeExpression context expr@(AbsSyntax.DotRecord record ident) expectedType = do
  recordType <- inferTypeExpression context record
  case recordType of
    (AbsSyntax.TypeRecord bindings) -> case Data.List.find (\(AbsSyntax.ARecordFieldType fieldName _) -> fieldName == ident) bindings of
      Nothing -> Left $ unexpectedFieldAccess ++ "\nunexpected access to field some in a record of type\n\t" ++ show recordType ++ "\nin the expression\n\t" ++ show expr
      Just (AbsSyntax.ARecordFieldType _ fieldType) -> compatible context fieldType expectedType expr
    _ -> Left $ notARecord ++ "\nexpected a record type but got\n\t" ++ show recordType ++ "\nfor the expression\n\t" ++ show record ++ "\nin the expression\n\t" ++ show expr
-- Let bindings
-- T-Let
checkTypeExpression content (AbsSyntax.Let bindings expr) expectedType = do
  case bindings of
    [AbsSyntax.APatternBinding (AbsSyntax.PatternVar (AbsSyntax.StellaIdent name)) value] -> do
      valueType <- inferTypeExpression content value
      checkTypeExpression (insertVar name valueType content) expr expectedType
    _ -> Left "Typechecker internal error : let with several arguments is unsupported"
-- Type Ascriptions
checkTypeExpression context typeAscExpr@(AbsSyntax.TypeAsc expr exprType) expectedType = do
  checkTypeExpression context expr exprType
  compatible context exprType expectedType typeAscExpr
-- Type Sum
-- T-inl
checkTypeExpression context expr@(AbsSyntax.Inl inlExpr) expectedType =
  case expectedType of
    (AbsSyntax.TypeSum inlType _) -> checkTypeExpression context inlExpr inlType
    _ -> Left $ unexpectedInjection ++ "\nexpected an expression of a non-sum type\n\t" ++ show expectedType ++ "\nbut got an injection into a sum type\n\t" ++ show expr
-- T-inr
checkTypeExpression context expr@(AbsSyntax.Inr inrExpr) expectedType =
  case expectedType of
    (AbsSyntax.TypeSum _ inrType) -> checkTypeExpression context inrExpr inrType
    _ -> Left $ unexpectedInjection ++ "\nexpected an expression of a non-sum type\n\t" ++ show expectedType ++ "\nbut got an injection into a sum type\n\t" ++ show expr
-- T-Variant
checkTypeExpression context variantExpr@(AbsSyntax.Variant ident exprData) expectedType =
  case expectedType of
    (AbsSyntax.TypeVariant variantFields) -> do
      when (hasDuplicateBy (\(AbsSyntax.AVariantFieldType lhs _) (AbsSyntax.AVariantFieldType rhs _) -> lhs == rhs) variantFields) $
        Left $
          dublicateVariantLabels ++ "\nduplicate variant labels in variant type\n\t" ++ show expectedType
      let fieldMap =
            M.fromList
              [ (label, optType)
                | AbsSyntax.AVariantFieldType label optType <- variantFields
              ]
      case M.lookup ident fieldMap of
        Nothing ->
          Left $
            unexpectedVariantLabel
              ++ "\nunexpected label\n\t"
              ++ show ident
              ++ "\nin variant type\n\t"
              ++ show expectedType
              ++ "\nin variant expression\n\t"
              ++ show variantExpr
        Just (AbsSyntax.SomeTyping expectedFieldType) ->
          case exprData of
            AbsSyntax.SomeExprData expr -> checkTypeExpression context expr expectedFieldType
            AbsSyntax.NoExprData -> Left $ unexpectedVariantLabel ++ "\nlabel " ++ show ident ++ " expects data but none provided"
        Just AbsSyntax.NoTyping ->
          case exprData of
            AbsSyntax.NoExprData -> Right ()
            AbsSyntax.SomeExprData _ -> Left $ unexpectedVariantLabel ++ "\nlabel " ++ show ident ++ " expects no data but data provided"
    _ -> Left $ unexpectedVariant ++ "\nexpected an expression of a non-variant type\n\t" ++ show expectedType ++ "\nbut got a variant\n\t" ++ show variantExpr
-- T-Case
checkTypeExpression context matchExpr@(AbsSyntax.Match expr matchCases) expectedType = do
  exprType <- inferTypeExpression context expr
  case exprType of
    (AbsSyntax.TypeSum leftType rightType) -> case matchCases of
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr
        ] -> do
          checkTypeExpression (insertVar leftName leftType context) leftExpr expectedType
          checkTypeExpression (insertVar rightName rightType context) rightExpr expectedType
          pure ()
      [ AbsSyntax.AMatchCase (AbsSyntax.PatternInr (AbsSyntax.PatternVar (AbsSyntax.StellaIdent rightName))) rightExpr,
        AbsSyntax.AMatchCase (AbsSyntax.PatternInl (AbsSyntax.PatternVar (AbsSyntax.StellaIdent leftName))) leftExpr
        ] -> do
          checkTypeExpression (insertVar rightName rightType context) rightExpr expectedType
          checkTypeExpression (insertVar leftName leftType context) leftExpr expectedType
          pure ()
      [] -> Left $ illegalEmptyMatching ++ "in expression\n\t" ++ show matchExpr
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInl _) _] -> Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinr"
      [AbsSyntax.AMatchCase (AbsSyntax.PatternInr _) _] -> Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\tinl"
      matchPattern -> Left $ unepxectedPatternForType ++ "unexpected match pattern\n\t" ++ show matchPattern ++ "\nfor type\n\t" ++ show exprType
    (AbsSyntax.TypeVariant variantFields) -> do
      when (hasDuplicateBy (\(AbsSyntax.AVariantFieldType lhs _) (AbsSyntax.AVariantFieldType rhs _) -> lhs == rhs) variantFields) $
        Left $
          dublicateVariantLabels ++ "\nduplicate variant labels in variant type\n\t" ++ show exprType
      let fieldMap = M.fromList [(label, optType) | AbsSyntax.AVariantFieldType label optType <- variantFields]
      mapM_ (checkVariantMatchCase context fieldMap expectedType) matchCases

      let coveredLabels = S.fromList [label | AbsSyntax.AMatchCase (AbsSyntax.PatternVariant label _) _ <- matchCases]
      let allLabels = M.keysSet fieldMap
      let missingLabels = S.difference allLabels coveredLabels
      if not (S.null missingLabels)
        then Left $ nonExhaustiveMatchPatterns ++ "\nwhen matching on expression\n\t" ++ show expr ++ "\nmisssing labels\n\t" ++ show missingLabels
        else Right ()
    _ -> Left $ unexpectedTypeForExpression ++ "\nexpected variant of sum-type in match but got\n\t" ++ show exprType ++ "\nin expression\n\t" ++ show matchExpr
-- List expressions
checkTypeExpression context expr@(AbsSyntax.List (h : t)) expectedType = case expectedType of
  (AbsSyntax.TypeList typeElement) -> do
    checkTypeExpression context h typeElement
    mapM_
      ( \element -> do
          checkTypeExpression context element typeElement
      )
      t
  _ -> Left $ unexpectedList ++ "\nexpected type\n\t" ++ show expectedType ++ "\nbut got list expression\n\t" ++ show expr
-- T-Nil
checkTypeExpression _ expr@(AbsSyntax.List []) expectedType = case expectedType of
  (AbsSyntax.TypeList _) -> Right ()
  _ -> Left $ unexpectedList ++ "\nexpected type\n\t" ++ show expectedType ++ "\nbut got list expression\n\t" ++ show expr
-- T-Cons
checkTypeExpression context expr@(AbsSyntax.ConsList listHead listTail) expectedType = case expectedType of
  (AbsSyntax.TypeList elementType) -> do
    checkTypeExpression context listHead elementType
    checkTypeExpression context listTail expectedType
    pure ()
  _ -> Left $ unexpectedList ++ "\nexpected type\n\t" ++ show expectedType ++ "\nbut got list expression\n\t" ++ show expr
-- T-IsNil
checkTypeExpression context expr@(AbsSyntax.IsEmpty list) expectedType = do
  compatible context AbsSyntax.TypeBool expectedType expr
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList _) -> Right ()
    _ -> Left $ notAList ++ "\nexpected a list type but got\n\t" ++ show listType ++ "\nfor the expression\n\t" ++ show list
-- T-Head
checkTypeExpression context expr@(AbsSyntax.Head list) expectedType = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList elementType) -> compatible context elementType expectedType expr
    _ -> Left $ notAList ++ "\nexpected a list type but got\n\t" ++ show listType ++ "\nfor the expression\n\t" ++ show list
-- T-Tail
checkTypeExpression context expr@(AbsSyntax.Tail list) expectedType = do
  listType <- inferTypeExpression context list
  case listType of
    (AbsSyntax.TypeList _) -> compatible context listType expectedType expr
    _ -> Left $ notAList ++ "\nexpected a list type but got\n\t" ++ show listType ++ "\nfor the expression\n\t" ++ show list
-- fix expr
-- T-Fix
checkTypeExpression context expr@(AbsSyntax.Fix function) expectedType = do
  functionType <- inferTypeExpression context function
  case functionType of
    (AbsSyntax.TypeFun [paramType] returnType) -> do
      compatible context paramType expectedType expr
      compatible context returnType expectedType expr
    _ -> Left $ notAFunction ++ "\nexpected a function type but got\n\t" ++ show functionType ++ "\nfor the expression\n\t" ++ show function ++ "\nin the fix call\n\t" ++ show expr
-- sequence
-- T-seq
checkTypeExpression context (AbsSyntax.Sequence left right) expectedType = do
  checkTypeExpression context left AbsSyntax.TypeUnit
  checkTypeExpression context right expectedType
-- references
-- T-ref
checkTypeExpression context expr@(AbsSyntax.Ref innerExpr) expectedType = do
  case expectedType of
    (AbsSyntax.TypeRef innerType) -> checkTypeExpression context innerExpr innerType
    _ ->
      Left $
        unexpectedReferenceType
          ++ "\nunexpected reference in expression\n\t"
          ++ show expr
          ++ "\nexpected type\n\t"
          ++ show expectedType
-- T-deref
checkTypeExpression context (AbsSyntax.Deref expr) expectedType = do
  checkTypeExpression context expr (AbsSyntax.TypeRef expectedType)
-- T-assign
checkTypeExpression context expr@(AbsSyntax.Assign lhs rhs) expectedType = do
  lhsRefType <- inferTypeExpression context lhs
  validateType lhsRefType
  case lhsRefType of
    (AbsSyntax.TypeRef lhsType) -> do
      checkTypeExpression context rhs lhsType
      compatible context AbsSyntax.TypeUnit expectedType expr
    _ ->
      Left $
        notAReference
          ++ "\ncannot assign into expression\n\t"
          ++ show lhs
          ++ "\nof a non-reference type\n\t"
          ++ show lhsRefType
-- T-loc
checkTypeExpression _ (AbsSyntax.ConstMemory (AbsSyntax.MemoryAddress address)) expectedType =
  case expectedType of
    (AbsSyntax.TypeRef _) -> pure ()
    _ ->
      Left $
        unexpectedMemoryAddress
          ++ "\nexpected an expression of a non-reference type\n\t"
          ++ show expectedType
          ++ "\nbut got a bare memory address\n\t"
          ++ show address
-- T-Error
checkTypeExpression _ AbsSyntax.Panic _ = pure ()
-- T-Raise
checkTypeExpression context (AbsSyntax.Throw expr) _ = do
  case lookupException context of
    Nothing -> Left $ exceptionTypeNotDeclared ++ "\ncannot throw exceptions, because exception type is not declared"
    Just exceptionType -> do
      checkTypeExpression context expr exceptionType
      pure ()
-- T-TryWith
checkTypeExpression context (AbsSyntax.TryWith mainBranch fallbackBranch) expectedType = do
  checkTypeExpression context mainBranch expectedType
  checkTypeExpression context fallbackBranch expectedType
  pure ()
-- T-TryCatch
checkTypeExpression context (AbsSyntax.TryCatch mainBranch catchPattern fallbackExpr) expectedType = do
  checkTypeExpression context mainBranch expectedType
  case lookupException context of
    Nothing ->
      Left $
        exceptionTypeNotDeclared ++ "\ncannot typecheck catch, because exception type is not declared"
    Just exceptionType -> do
      case catchPattern of
        (AbsSyntax.PatternVar (AbsSyntax.StellaIdent varName)) -> do
          checkTypeExpression (insertVar varName exceptionType context) fallbackExpr expectedType
          pure ()
        _ -> Left "Internal error : unsupported catch pattern"
-- T-Cast
checkTypeExpression context castExpr@(AbsSyntax.TypeCast expr typeToCast) expectedType = do 
  _ <- inferTypeExpression context expr
  compatible context typeToCast expectedType castExpr
-- default rule
checkTypeExpression context expr expectedType = do
  exprType <- inferTypeExpression context expr
  compatible context exprType expectedType expr
