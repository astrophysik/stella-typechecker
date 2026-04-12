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
    dublicateVariantLabels,
    notAReference,
    unexpectedReferenceType,
    ambiguousReferenceType,
    unexpectedMemoryAddress,
    ambiguousPanicType,
    duplicateExceptionType,
    exceptionTypeNotDeclared,
    ambiguousThrowType,
    unexpectedSubType,

    -- * Context
    Context,
    emptyContext,
    lookupVar,
    insertVar,

    -- * Type inference and checking
    inferTypeExpression,
    checkTypeExpression,

    -- * Declarations
    checkDeclarations,
    collectDeclarations,
  )
where

import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.BidirectionalTyping (checkTypeExpression, inferTypeExpression)
import TypeCheck.Context
  ( Context,
    emptyContext,
    insertVar,
    lookupVar,
  )
import TypeCheck.Decl (checkDeclarations, collectDeclarations, processExtensions)
import TypeCheck.Errors
  ( ambiguousList,
    ambiguousPanicType,
    ambiguousReferenceType,
    ambiguousSumType,
    ambiguousThrowType,
    ambiguousVariantType,
    dublicateFunctionDeclaration,
    dublicateRecordFields,
    dublicateRecordTypeFields,
    dublicateVariantLabels,
    duplicateExceptionType,
    exceptionTypeNotDeclared,
    illegalEmptyMatching,
    missingMain,
    missingRecordFields,
    nonExhaustiveMatchPatterns,
    notAFunction,
    notAList,
    notARecord,
    notAReference,
    notATuple,
    tupleIndexOutOfBounds,
    undefinedVariable,
    unepxectedPatternForType,
    unexpectedFieldAccess,
    unexpectedInjection,
    unexpectedLambda,
    unexpectedList,
    unexpectedMemoryAddress,
    unexpectedRecord,
    unexpectedRecordFields,
    unexpectedReferenceType,
    unexpectedTuple,
    unexpectedTupleLength,
    unexpectedTypeForExpression,
    unexpectedTypeForParam,
    unexpectedVariant,
    unexpectedSubType,
    unexpectedVariantLabel,
  )

-- | Main entry point for type checking a program.
typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck (AbsSyntax.AProgram _ extensions declarations) = do
  programContext <- collectDeclarations declarations
  case lookupVar "main" programContext of
    Just _ -> checkDeclarations (processExtensions programContext extensions) declarations
    Nothing -> Left missingMain
