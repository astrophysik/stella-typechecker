module TypeCheck.Bidirectional.Context
  ( Context,
    emptyContext,
    lookupVar,
    insertVar,
    lookupException,
    insertException,
    enableSubTyping,
    lookupSubTyping,
    lookupAmbiguousTypeAsBottom,
    enableAmbiguousTypeAsBottom,
    lookupUniversalTypes,
    enableUniversalTypes,
    insertTypeVar,
    lookupTypeVar
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Parsing.AbsSyntax as AbsSyntax

data Context = Context
  { variableContext :: HM.HashMap String AbsSyntax.Type,
    exceptionContext :: Maybe AbsSyntax.Type,
    typeVariableContext :: HS.HashSet String,
    useSubTyping :: Bool,
    useAmbiguousTypeAsBottom :: Bool,
    useUniversalTypes :: Bool
  }
  deriving (Show)

emptyContext :: Context
emptyContext =
  Context
    { variableContext = HM.empty,
      exceptionContext = Nothing,
      typeVariableContext = HS.empty,
      useSubTyping = False,
      useAmbiguousTypeAsBottom = False,
      useUniversalTypes = False
    }

lookupVar :: String -> Context -> Maybe AbsSyntax.Type
lookupVar varName context = HM.lookup varName (variableContext context)

insertVar :: String -> AbsSyntax.Type -> Context -> Context
insertVar varName varType context =
  Context
    { variableContext = HM.insert varName varType (variableContext context),
      exceptionContext = exceptionContext context,
      typeVariableContext = typeVariableContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context,
      useUniversalTypes = useUniversalTypes context
    }

lookupException :: Context -> Maybe AbsSyntax.Type
lookupException = exceptionContext

insertException :: AbsSyntax.Type -> Context -> Context
insertException exceptionType context =
  Context
    { variableContext = variableContext context,
      exceptionContext = Just exceptionType,
      typeVariableContext = typeVariableContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context,
      useUniversalTypes = useUniversalTypes context
    }

insertTypeVar :: String -> Context -> Context
insertTypeVar typeVar context = 
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      typeVariableContext = HS.insert typeVar (typeVariableContext context),
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context,
      useUniversalTypes = useUniversalTypes context
    }

lookupTypeVar :: String -> Context -> Bool 
lookupTypeVar typeVar context = HS.member typeVar (typeVariableContext context)

lookupUniversalTypes :: Context -> Bool
lookupUniversalTypes = useUniversalTypes

enableUniversalTypes :: Context -> Context
enableUniversalTypes context =
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      typeVariableContext = typeVariableContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context,
      useUniversalTypes = True
    }

lookupSubTyping :: Context -> Bool
lookupSubTyping = useSubTyping

enableSubTyping :: Context -> Context
enableSubTyping context =
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      typeVariableContext = typeVariableContext context,
      useSubTyping = True,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context,
      useUniversalTypes = useUniversalTypes context
    }

lookupAmbiguousTypeAsBottom :: Context -> Bool
lookupAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom

enableAmbiguousTypeAsBottom :: Context -> Context
enableAmbiguousTypeAsBottom context =
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      typeVariableContext = typeVariableContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = True,
      useUniversalTypes = useUniversalTypes context
    }
