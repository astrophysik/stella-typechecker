module TypeCheck.TypeCheck (checkProgram) where

import Data.Foldable (traverse_)
import qualified Parsing.AbsSyntax as AbsSyntax

unexpectedTypeForExpression :: String
unexpectedTypeForExpression = "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION"

-- Type infer
inferTypeExpression :: AbsSyntax.Expr -> Either String AbsSyntax.Type
-- Boolean expressions
inferTypeExpression AbsSyntax.ConstFalse = Right AbsSyntax.TypeBool -- T-False
inferTypeExpression AbsSyntax.ConstTrue = Right AbsSyntax.TypeBool -- T-True
inferTypeExpression (AbsSyntax.If condition onTrue onFalse) = do
  -- T-If
  checkTypeExpression condition AbsSyntax.TypeBool
  tTrue <- inferTypeExpression onTrue
  checkTypeExpression onFalse tTrue
  pure tTrue
-- Integer expressions
inferTypeExpression (AbsSyntax.ConstInt _) = Right AbsSyntax.TypeNat -- T-Zero
inferTypeExpression (AbsSyntax.Succ integer) = do
  -- T-Succ
  checkTypeExpression integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeNat
inferTypeExpression (AbsSyntax.Pred integer) = do
  -- T-Pred
  checkTypeExpression integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeNat
inferTypeExpression (AbsSyntax.IsZero integer) = do
  -- T-IsZero
  checkTypeExpression integer AbsSyntax.TypeNat
  pure AbsSyntax.TypeBool
inferTypeExpression _ = Left "unsupported"

-- Type check
checkTypeExpression :: AbsSyntax.Expr -> AbsSyntax.Type -> Either String ()
-- Boolean expressions
checkTypeExpression AbsSyntax.ConstFalse expectedType = case expectedType of -- T-False
  AbsSyntax.TypeBool -> Right ()
  _ -> Left unexpectedTypeForExpression
checkTypeExpression AbsSyntax.ConstTrue expectedType = case expectedType of -- T-True
  AbsSyntax.TypeBool -> Right ()
  _ -> Left unexpectedTypeForExpression
checkTypeExpression (AbsSyntax.If condition onTrue onFalse) expectedType = do
  -- T-If
  checkTypeExpression condition AbsSyntax.TypeBool
  checkTypeExpression onTrue expectedType
  checkTypeExpression onFalse expectedType
  pure ()
-- Integer expressions
checkTypeExpression (AbsSyntax.ConstInt _) expectedType = case expectedType of -- T-Zero
  AbsSyntax.TypeNat -> Right ()
  _ -> Left unexpectedTypeForExpression
checkTypeExpression (AbsSyntax.Succ integer) expectedType = case expectedType of -- T-Succ
  AbsSyntax.TypeNat -> checkTypeExpression integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
checkTypeExpression (AbsSyntax.Pred integer) expectedType = case expectedType of -- T-Pred
  AbsSyntax.TypeNat -> checkTypeExpression integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
checkTypeExpression (AbsSyntax.IsZero integer) expectedType = case expectedType of -- T-IsZero
  AbsSyntax.TypeBool -> checkTypeExpression integer AbsSyntax.TypeNat
  _ -> Left unexpectedTypeForExpression
checkTypeExpression _ _ = Left "unsupported"

checkDeclaration :: AbsSyntax.Decl -> Either String ()
checkDeclaration (AbsSyntax.DeclFun annotations ident params returnType throwType decls expr) = case inferTypeExpression expr of
  Left msg -> Left msg
  Right _ -> Right ()
checkDeclaration _ = Left "unsupported"

checkProgram :: AbsSyntax.Program -> Either String ()
checkProgram (AbsSyntax.AProgram languageDecl extentions declarations) = traverse_ checkDeclaration declarations
