module TypeCheck.Common
  ( nthElement,
    hasDuplicateBy,
    validateType,
  )
where

import Control.Monad (forM_, when)
import qualified Parsing.AbsSyntax as AbsSyntax
import qualified TypeCheck.Errors (dublicateRecordTypeFields, dublicateVariantLabels)

nthElement :: Integer -> [a] -> Maybe a
nthElement 1 (x : _) = Just x
nthElement n (_ : xs) | n > 1 = nthElement (n - 1) xs
nthElement _ _ = Nothing

hasDuplicateBy :: (a -> a -> Bool) -> [a] -> Bool
hasDuplicateBy eq xs =
  any (\(x, rest) -> any (eq x) rest) (zip xs (tail (tails xs)))
  where
    tails [] = []
    tails ys@(_ : ts) = ys : tails ts

validateType :: AbsSyntax.Type -> Either String ()
validateType recordType@(AbsSyntax.TypeRecord fields) = do
  when (hasDuplicateBy (\(AbsSyntax.ARecordFieldType n1 _) (AbsSyntax.ARecordFieldType n2 _) -> n1 == n2) fields) $
    Left $
      TypeCheck.Errors.dublicateRecordTypeFields ++ "\nduplicate record type fields\n\t" ++ show recordType
  mapM_ (\(AbsSyntax.ARecordFieldType _ fieldType) -> validateType fieldType) fields
validateType variantExpr@(AbsSyntax.TypeVariant labels) = do
  when (hasDuplicateBy (\(AbsSyntax.AVariantFieldType n1 _) (AbsSyntax.AVariantFieldType n2 _) -> n1 == n2) labels) $
    Left $
      TypeCheck.Errors.dublicateVariantLabels ++ "\nduplicate variant type fields\n\t" ++ show variantExpr
  mapM_
    ( \(AbsSyntax.AVariantFieldType _ optTyping) -> case optTyping of
        AbsSyntax.SomeTyping innerType -> validateType innerType
        AbsSyntax.NoTyping -> Right ()
    )
    labels
validateType (AbsSyntax.TypeFun argTypes retType) = do
  mapM_ validateType argTypes
  validateType retType
validateType (AbsSyntax.TypeList elemType) =
  validateType elemType
validateType (AbsSyntax.TypeTuple elemTypes) =
  mapM_ validateType elemTypes
validateType (AbsSyntax.TypeSum t1 t2) = do
  validateType t1
  validateType t2
validateType _ = Right ()
