module TypeCheck.ConstraintBased.Unification
  ( unify,
  )
where

import qualified Parsing.AbsSyntax as AbsSyntax
import qualified TypeCheck.ConstraintBased.ConstraintSet as ConstraintSet
import qualified TypeCheck.Errors as Errors

unify :: [(AbsSyntax.Type, AbsSyntax.Type)] -> Either String ()
unify [] = Right ()
unify ((lhs, rhs) : xs)
  | lhs == rhs = unify xs
  | AbsSyntax.TypeVar ident <- lhs, not $ ConstraintSet.isFreeVariable ident rhs = unify (ConstraintSet.substituteVar ident rhs xs)
  | AbsSyntax.TypeVar ident <- rhs, not $ ConstraintSet.isFreeVariable ident lhs = unify (ConstraintSet.substituteVar ident lhs xs)
  | AbsSyntax.TypeVar ident <- lhs,
    ConstraintSet.isFreeVariable ident rhs =
      Left $
        Errors.accursCheckInfiniteType
          ++ "\ncannot construct an infinite type when type\n\t"
          ++ show lhs
          ++ "\nis expected to unify with\n\t"
          ++ show rhs
  | AbsSyntax.TypeVar ident <- rhs,
    ConstraintSet.isFreeVariable ident lhs =
      Left $
        Errors.accursCheckInfiniteType
          ++ "\ncannot construct an infinite type when type\n\t"
          ++ show rhs
          ++ "\nis expected to unify with\n\t"
          ++ show lhs
  | AbsSyntax.TypeFun [lhsArgType] lhsReturnType <- lhs, AbsSyntax.TypeFun [rhsArgType] rhsReturnType <- rhs = unify (xs ++ [(lhsArgType, rhsArgType), (lhsReturnType, rhsReturnType)])
  | AbsSyntax.TypeList lhsInnerListType <- lhs, AbsSyntax.TypeList rhsInnerListType <- rhs = unify (xs ++ [(lhsInnerListType, rhsInnerListType)])
  | AbsSyntax.TypeSum lhsInlType lhsInrType <- lhs, AbsSyntax.TypeSum rhsInlType rhsInrType <- rhs = unify (xs ++ [(lhsInlType, rhsInlType), (lhsInrType, rhsInrType)])
  | lhs /= rhs = Left $ Errors.unexpectedTypeForExpression ++ "\nwhen unifying expected type\n\t" ++ show lhs ++ "\nagainst actual type\n\t" ++ show rhs
