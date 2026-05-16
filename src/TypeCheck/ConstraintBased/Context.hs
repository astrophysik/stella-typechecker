module TypeCheck.ConstraintBased.Context
  ( Context(..),
    emptyContext,
    lookupVar,
    insertVar,
    incTypeVarId,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

data Context = Context
  { variableContext :: HM.HashMap String AbsSyntax.Type,
    typeVarId :: Int
  }
  deriving (Show)

emptyContext :: Context
emptyContext = Context
  { variableContext = HM.empty,
    typeVarId = 0
  }

lookupVar :: String -> Context -> Maybe AbsSyntax.Type
lookupVar varName context = HM.lookup varName (variableContext context)

insertVar :: String -> AbsSyntax.Type -> Context -> Context
insertVar varName varType context =
  Context
    { variableContext = HM.insert varName varType (variableContext context),
      typeVarId = typeVarId context
    }

incTypeVarId :: Context -> Context
incTypeVarId context =
  Context
    { variableContext = variableContext context,
      typeVarId = typeVarId context + 1
    }
