module TypeCheck.ConstraintBased.ConstraintSet (
    isFreeVariable,
    substituteVar
) where

import qualified Parsing.AbsSyntax as AbsSyntax

--   = TypeAuto
--   | TypeFun [Type] Type
--   | TypeForAll [StellaIdent] Type
--   | TypeRec StellaIdent Type
--   | TypeSum Type Type
--   | TypeTuple [Type]
--   | TypeRecord [RecordFieldType]
--   | TypeVariant [VariantFieldType]
--   | TypeList Type
--   | TypeBool
--   | TypeNat
--   | TypeUnit
--   | TypeTop
--   | TypeBottom
--   | TypeRef Type
--   | TypeVar StellaIdent

isFreeVariable :: AbsSyntax.StellaIdent -> AbsSyntax.Type -> Bool
isFreeVariable ident rhs =
  case rhs of
    AbsSyntax.TypeVar rhsIdent -> ident == rhsIdent
    AbsSyntax.TypeRef innerType -> isFreeVariable ident innerType
    AbsSyntax.TypeList innerType -> isFreeVariable ident innerType
    AbsSyntax.TypeFun [argType] returnType -> isFreeVariable ident argType || isFreeVariable ident returnType
    AbsSyntax.TypeSum lhsType rhsType -> isFreeVariable ident lhsType || isFreeVariable ident rhsType
    AbsSyntax.TypeNat -> False
    AbsSyntax.TypeBool -> False
    AbsSyntax.TypeUnit -> False
    _ -> error "critical error # 1"

substituteVarInType :: AbsSyntax.StellaIdent -> AbsSyntax.Type -> AbsSyntax.Type -> AbsSyntax.Type
substituteVarInType varName substitute typeForSubstitution =
  case typeForSubstitution of
    AbsSyntax.TypeVar ident ->
      if ident == varName
        then substitute
        else typeForSubstitution
    AbsSyntax.TypeRef innerType -> AbsSyntax.TypeRef $ substituteVarInType varName substitute innerType
    AbsSyntax.TypeNat -> AbsSyntax.TypeNat
    AbsSyntax.TypeBool -> AbsSyntax.TypeBool
    AbsSyntax.TypeUnit -> AbsSyntax.TypeUnit
    AbsSyntax.TypeList innerType -> AbsSyntax.TypeList $ substituteVarInType varName substitute innerType
    AbsSyntax.TypeSum lhsType rhsType -> AbsSyntax.TypeSum (substituteVarInType varName substitute lhsType) (substituteVarInType varName substitute rhsType)
    AbsSyntax.TypeFun [argType] returnType -> AbsSyntax.TypeFun [substituteVarInType varName substitute argType] (substituteVarInType varName substitute returnType)
    _ -> error $ "critical error # 2 " ++ show typeForSubstitution

substituteVar :: AbsSyntax.StellaIdent -> AbsSyntax.Type -> [(AbsSyntax.Type, AbsSyntax.Type)] -> [(AbsSyntax.Type, AbsSyntax.Type)]
substituteVar varName substitute [] = []
substituteVar varName substitute ((lhs, rhs) : xs) = (substituteVarInType varName substitute lhs, substituteVarInType varName substitute rhs) : substituteVar varName substitute xs
