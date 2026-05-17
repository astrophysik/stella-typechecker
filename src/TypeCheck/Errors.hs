module TypeCheck.Errors
  ( unexpectedTypeForExpression,
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
    formatUnexpectedTypeForExpressionMsg,
    formatUnexpectedSubTypeMsg,
    ambiguousType,
    accursCheckInfiniteType,
    notAGenericFunction, 
    incorrectNumberOfTypeArguments,
    undefinedTypeVariable,
    notAGenericFunction
  )
where

import qualified Parsing.AbsSyntax as AbsSyntax

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

ambiguousVariantType :: String
ambiguousVariantType = "ERROR_AMBIGUOUS_VARIANT_TYPE"

unexpectedVariant :: String
unexpectedVariant = "ERROR_UNEXPECTED_VARIANT"

unexpectedVariantLabel :: String
unexpectedVariantLabel = "ERROR_UNEXPECTED_VARIANT_LABEL"

dublicateVariantLabels :: String
dublicateVariantLabels = "ERROR_DUPLICATE_VARIANT_TYPE_FIELDS"

illegalEmptyMatching :: String
illegalEmptyMatching = "ERROR_ILLEGAL_EMPTY_MATCHING"

nonExhaustiveMatchPatterns :: String
nonExhaustiveMatchPatterns = "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS"

unepxectedPatternForType :: String
unepxectedPatternForType = "ERROR_UNEXPECTED_PATTERN_FOR_TYPE"

unexpectedInjection :: String
unexpectedInjection = "ERROR_UNEXPECTED_INJECTION"

ambiguousList :: String
ambiguousList = "ERROR_AMBIGUOUS_LIST"

notAList :: String
notAList = "ERROR_NOT_A_LIST"

unexpectedList :: String
unexpectedList = "ERROR_UNEXPECTED_LIST"

dublicateFunctionDeclaration :: String
dublicateFunctionDeclaration = "ERROR_DUPLICATE_FUNCTION_DECLARATION"

dublicateRecordFields :: String
dublicateRecordFields = "ERROR_DUPLICATE_RECORD_FIELDS"

dublicateRecordTypeFields :: String
dublicateRecordTypeFields = "ERROR_DUPLICATE_RECORD_TYPE_FIELDS"

ambiguousSumType :: String
ambiguousSumType = "ERROR_AMBIGUOUS_SUM_TYPE"

unexpectedTupleLength :: String
unexpectedTupleLength = "ERROR_UNEXPECTED_TUPLE_LENGTH"

notAReference :: String
notAReference = "ERROR_NOT_A_REFERENCE"

unexpectedReferenceType :: String
unexpectedReferenceType = "ERROR_UNEXPECTED_REFERENCE"

ambiguousReferenceType :: String
ambiguousReferenceType = "ERROR_AMBIGUOUS_REFERENCE_TYPE"

unexpectedMemoryAddress :: String
unexpectedMemoryAddress = "ERROR_UNEXPECTED_MEMORY_ADDRESS"

ambiguousPanicType :: String
ambiguousPanicType = "ERROR_AMBIGUOUS_PANIC_TYPE"

duplicateExceptionType :: String
duplicateExceptionType = "ERROR_DUPLICATE_EXCEPTION_TYPE"

exceptionTypeNotDeclared :: String
exceptionTypeNotDeclared = "ERROR_EXCEPTION_TYPE_NOT_DECLARED"

ambiguousThrowType :: String
ambiguousThrowType = "RROR_AMBIGUOUS_THROW_TYPE"

unexpectedSubType :: String
unexpectedSubType = "ERROR_UNEXPECTED_SUBTYPE"

ambiguousType :: String
ambiguousType = "ERROR_AMBIGUOUS_TYPE"

accursCheckInfiniteType :: String
accursCheckInfiniteType = "ERROR_OCCURS_CHECK_INFINITE_TYPE"

notAGenericFunction :: String
notAGenericFunction = "ERROR_NOT_A_GENERIC_FUNCTION"

incorrectNumberOfTypeArguments :: String 
incorrectNumberOfTypeArguments = "ERROR_INCORRECT_NUMBER_OF_TYPE_ARGUMENTS"

undefinedTypeVariable :: String 
undefinedTypeVariable = "ERROR_UNDEFINED_TYPE_VARIABLE"

formatUnexpectedSubTypeMsg :: AbsSyntax.Type -> AbsSyntax.Type -> AbsSyntax.Expr -> String
formatUnexpectedSubTypeMsg realType expectedType expression =
  unexpectedSubType
    ++ "\nexpected a subtype of\n\t"
    ++ show expectedType
    ++ "\nbut got\n\t"
    ++ show realType
    ++ "\nwhen typechecking the expression\n\t"
    ++ show expression

formatUnexpectedTypeForExpressionMsg :: AbsSyntax.Type -> AbsSyntax.Type -> AbsSyntax.Expr -> String
formatUnexpectedTypeForExpressionMsg realType expectedType expression =
  unexpectedTypeForExpression
    ++ "\nexpected type\n\t"
    ++ show expectedType
    ++ "\nbut got\n\t"
    ++ show realType
    ++ "\nwhen typechecking the expression\n\t"
    ++ show expression
