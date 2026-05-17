module TypeCheck.Bidirectional.UniversalTypes (substituteType, Substitution, validateTypeVars, validateTypeList) where

import qualified Data.Map.Strict as Map
import qualified Parsing.AbsSyntax as AbsSyntax
import TypeCheck.Bidirectional.Context
import TypeCheck.Errors
import TypeCheck.Common

type Substitution = Map.Map String AbsSyntax.Type

validateTypeList :: [AbsSyntax.StellaIdent] -> Either String ()
validateTypeList typeList = if hasDuplicateBy (==) typeList 
    then Left duplicateTypeParameter
    else Right ()

substituteType :: Substitution -> AbsSyntax.Type -> AbsSyntax.Type
substituteType sub t@(AbsSyntax.TypeVar (AbsSyntax.StellaIdent name)) =
  Map.findWithDefault t name sub
substituteType sub (AbsSyntax.TypeForAll typeVars bodyType) =
  AbsSyntax.TypeForAll typeVars (substituteType sub' bodyType)
  where
    boundNames = [name | AbsSyntax.StellaIdent name <- typeVars]
    sub' = foldr Map.delete sub boundNames
substituteType sub (AbsSyntax.TypeFun paramTypes returnType) =
  AbsSyntax.TypeFun (map (substituteType sub) paramTypes) (substituteType sub returnType)
substituteType sub (AbsSyntax.TypeList elemType) =
  AbsSyntax.TypeList (substituteType sub elemType)
substituteType sub (AbsSyntax.TypeTuple elemTypes) =
  AbsSyntax.TypeTuple (map (substituteType sub) elemTypes)
substituteType sub (AbsSyntax.TypeSum left right) =
  AbsSyntax.TypeSum (substituteType sub left) (substituteType sub right)
substituteType sub (AbsSyntax.TypeRef innerType) =
  AbsSyntax.TypeRef (substituteType sub innerType)
substituteType sub (AbsSyntax.TypeRecord fields) =
  AbsSyntax.TypeRecord (map subField fields)
  where
    subField (AbsSyntax.ARecordFieldType ident t) =
      AbsSyntax.ARecordFieldType ident (substituteType sub t)
substituteType sub (AbsSyntax.TypeVariant labels) =
  AbsSyntax.TypeVariant (map subLabel labels)
  where
    subLabel (AbsSyntax.AVariantFieldType ident opt) =
      AbsSyntax.AVariantFieldType ident (subOpt opt)
    subOpt (AbsSyntax.SomeTyping t) = AbsSyntax.SomeTyping (substituteType sub t)
    subOpt AbsSyntax.NoTyping = AbsSyntax.NoTyping
substituteType _ t = t

validateTypeVars :: Context -> AbsSyntax.Type -> Either String ()
validateTypeVars ctx = checkType
  where
    checkType (AbsSyntax.TypeVar (AbsSyntax.StellaIdent name))
      | lookupTypeVar name ctx = Right ()
      | otherwise = Left $ undefinedTypeVariable ++ "\nundefined type variable: " ++ name
    checkType (AbsSyntax.TypeFun paramTypes returnType) = do
      mapM_ checkType paramTypes
      checkType returnType
    checkType (AbsSyntax.TypeList elemType) = checkType elemType
    checkType (AbsSyntax.TypeTuple elemTypes) = mapM_ checkType elemTypes
    checkType (AbsSyntax.TypeSum left right) = checkType left >> checkType right
    checkType (AbsSyntax.TypeRef innerType) = checkType innerType
    checkType (AbsSyntax.TypeRecord fields) = mapM_ checkField fields
      where
        checkField (AbsSyntax.ARecordFieldType _ t) = checkType t
    checkType (AbsSyntax.TypeVariant labels) = mapM_ checkLabel labels
      where
        checkLabel (AbsSyntax.AVariantFieldType _ opt) = case opt of
          AbsSyntax.SomeTyping t -> checkType t
          AbsSyntax.NoTyping -> Right ()
    checkType (AbsSyntax.TypeForAll typeVars bodyType) = do
      validateTypeList typeVars  
      let boundNames = [name | AbsSyntax.StellaIdent name <- typeVars]
          ctx' = foldr insertTypeVar ctx boundNames
      validateTypeVars ctx' bodyType
    checkType _ = Right ()
