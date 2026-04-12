module TypeCheck.SubTyping
  ( (<:)
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Parsing.AbsSyntax as AbsSyntax

infix 4 <:

(<:) :: AbsSyntax.Type -> AbsSyntax.Type -> Bool
(<:) t1 t2 | t1 == t2 = True
(<:) (AbsSyntax.TypeRecord s) (AbsSyntax.TypeRecord t) =
  let extractBindigs bindingsType =
        M.fromList
          [ (ident, identType)
            | AbsSyntax.ARecordFieldType (AbsSyntax.StellaIdent ident) identType <- bindingsType
          ]

   in ( S.isSubsetOf
          (M.keysSet (extractBindigs t))
          (M.keysSet (extractBindigs s))
          && all
            ( \(ident, ti) ->
                case M.lookup ident (extractBindigs s) of
                  Nothing -> False
                  Just si -> si <: ti
            )
            (M.toList $ extractBindigs t)
      )
(<:) (AbsSyntax.TypeFun [s1] s2) (AbsSyntax.TypeFun [t1] t2) =
  t1 <: s1 && s2 <: t2
(<:) (AbsSyntax.TypeVariant s) (AbsSyntax.TypeVariant t) =
  let extractVariants variantType =
        M.fromList
          [ (ident, optTyping)
            | AbsSyntax.AVariantFieldType (AbsSyntax.StellaIdent ident) optTyping <- variantType
          ]
      extractType (AbsSyntax.SomeTyping ty) = Just ty
      extractType AbsSyntax.NoTyping = Nothing
      sMap = extractVariants s
      tMap = extractVariants t
   in
      all
        ( \(ident, si) ->
            case M.lookup ident tMap of
              Nothing -> False
              Just ti -> case (extractType si, extractType ti) of
                (Just siTy, Just tiTy) -> siTy <: tiTy
                (Nothing, Nothing) -> True
                _ -> False
        )
        (M.toList sMap)
(<:) (AbsSyntax.TypeList s) (AbsSyntax.TypeList t) = s <: t
(<:) (AbsSyntax.TypeRef s) (AbsSyntax.TypeRef t) = s <: t && t <: s
(<:) _ AbsSyntax.TypeTop = True
(<:) AbsSyntax.TypeBottom _ = True
(<:) _ _ = False
