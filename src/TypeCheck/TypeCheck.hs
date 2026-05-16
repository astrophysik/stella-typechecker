{-# LANGUAGE LambdaCase #-}

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
    ambiguousType,
  )
where

import qualified Parsing.AbsSyntax as AbsSyntax
import qualified TypeCheck.Bidirectional.Decl (typeCheck)
import qualified TypeCheck.ConstraintBased.Decl (typeCheck)
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
    unexpectedSubType,
    unexpectedTuple,
    unexpectedTupleLength,
    unexpectedTypeForExpression,
    unexpectedTypeForParam,
    unexpectedVariant,
    unexpectedVariantLabel,
    ambiguousType,
  )

typeCheck :: AbsSyntax.Program -> Either String ()
typeCheck program@(AbsSyntax.AProgram _ extensions _) =
  let hasExtension name =
        any
          ( \case
              AbsSyntax.AnExtension nestedExtensions ->
                any
                  ( \case
                      (AbsSyntax.ExtensionName ext) -> ext == name
                      _ -> False
                  )
                  nestedExtensions
          )
          extensions
   in if hasExtension "#type-reconstruction"
        then TypeCheck.ConstraintBased.Decl.typeCheck program
        else TypeCheck.Bidirectional.Decl.typeCheck program
