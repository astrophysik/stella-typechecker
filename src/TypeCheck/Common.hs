module TypeCheck.Common
  ( Context,
    emptyContext,
    lookupVar,
    extendContext,
    nthElement,
    hasDuplicateBy,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

type Context = HM.HashMap String AbsSyntax.Type

emptyContext :: Context
emptyContext = HM.empty

lookupVar :: String -> Context -> Maybe AbsSyntax.Type
lookupVar = HM.lookup

extendContext :: String -> AbsSyntax.Type -> Context -> Context
extendContext = HM.insert

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
