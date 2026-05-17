module TypeCheck.ConstraintBased.ConstraintSet (
    isFreeVariable,
    substituteVar,
    checkTypeAmbiguous
) where

import qualified Parsing.AbsSyntax as AbsSyntax
import qualified TypeCheck.Errors as Errors
import qualified Data.Set as Set
import TypeCheck.ConstraintBased.Context

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

collectTypeVars :: [(AbsSyntax.Type, AbsSyntax.Type)] -> Set.Set String
collectTypeVars constraints = Set.unions $ map typeVarsInConstraint constraints
  where
    typeVarsInConstraint (t1, t2) = typeVarsInType t1 `Set.union` typeVarsInType t2
    typeVarsInType :: AbsSyntax.Type -> Set.Set String
    typeVarsInType (AbsSyntax.TypeVar (AbsSyntax.StellaIdent name)) = Set.singleton name
    typeVarsInType (AbsSyntax.TypeFun params ret) =
        Set.unions (map typeVarsInType params) `Set.union` typeVarsInType ret
    typeVarsInType (AbsSyntax.TypeList t) = typeVarsInType t
    typeVarsInType (AbsSyntax.TypeTuple ts) = Set.unions (map typeVarsInType ts)
    typeVarsInType (AbsSyntax.TypeSum l r) = typeVarsInType l `Set.union` typeVarsInType r
    typeVarsInType (AbsSyntax.TypeRef t) = typeVarsInType t
    typeVarsInType (AbsSyntax.TypeRecord fields) = Set.unions $ map fieldVars fields
      where fieldVars (AbsSyntax.ARecordFieldType _ t) = typeVarsInType t
    typeVarsInType (AbsSyntax.TypeVariant labels) = Set.unions $ map labelVars labels
      where
        labelVars (AbsSyntax.AVariantFieldType _ opt) = case opt of
          AbsSyntax.SomeTyping t -> typeVarsInType t
          AbsSyntax.NoTyping -> Set.empty
    typeVarsInType _ = Set.empty

checkTypeAmbiguous :: Context -> [(AbsSyntax.Type, AbsSyntax.Type)] -> Either String ()
checkTypeAmbiguous context constraints = do 
  let maxTypeVarId = typeVarId context
  let generatedVars = Set.fromList ["T" ++ show i | i <- [0..maxTypeVarId - 1]]
  let usedVars = collectTypeVars constraints

  let ambiguousVars = generatedVars `Set.difference` usedVars

  if Set.null ambiguousVars
    then Right ()
    else Left $ Errors.ambiguousType ++ "\nAmbiguous type variables: " ++ show (Set.toList ambiguousVars)