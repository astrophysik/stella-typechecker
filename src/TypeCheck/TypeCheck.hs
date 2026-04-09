module TypeCheck.TypeCheck
  ( typeCheck,
    -- * Errors
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
    unexpectedRecordFields,
    ambiguousVariantType,
    illegalEmptyMatching,
    nonExhaustiveMatchPatterns,
    unepxectedPatternForType,
    unexpectedInjection,
    ambiguousList,
    notAList,
    unexpectedList,
    dublicateFunctionDeclaration,
    dublicateRecordFields,
    dublicateRecordTypeFields,
    ambiguousSumType,
    unexpectedTupleLength,
    unexpectedVariant,
    unexpectedVariantLabel,
    duplicateVariantLabels,
    -- * Context
    Context,
    emptyContext,
    lookupVar,
    extendContext,
    -- * Type inference and checking
    inferTypeExpression,
    checkTypeExpression,
    -- * Declarations
    checkDeclarations,
    collectDeclarations,
  )
where

import TypeCheck.Common
  ( Context,
    emptyContext,
    extendContext,
    lookupVar,
  )
import TypeCheck.Decl (checkDeclarations, collectDeclarations)
import TypeCheck.Errors
  ( ambiguousList,
    ambiguousSumType,
    ambiguousVariantType,
    dublicateFunctionDeclaration,
    dublicateRecordFields,
    dublicateRecordTypeFields,
    illegalEmptyMatching,
    missingMain,
    missingRecordFields,
    notAFunction,
    notAList,
    notARecord,
    notATuple,
    nonExhaustiveMatchPatterns,
    tupleIndexOutOfBounds,
    unepxectedPatternForType,
    unexpectedFieldAccess,
    unexpectedInjection,
    unexpectedLambda,
    unexpectedList,
    unexpectedRecord,
    unexpectedRecordFields,
    unexpectedTuple,
    unexpectedTupleLength,
    unexpectedTypeForExpression,
    unexpectedTypeForParam,
    undefinedVariable,
    unexpectedVariant,
    unexpectedVariantLabel,
    duplicateVariantLabels,
  )
import TypeCheck.Expr (inferTypeExpression, checkTypeExpression)
import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

-- | Main entry point for type checking a program.
typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck (AbsSyntax.AProgram _ _ declarations) = do
  programContext <- collectDeclarations declarations
  case HM.lookup "main" programContext of
    Just _ -> checkDeclarations programContext declarations
    Nothing -> Left missingMain
