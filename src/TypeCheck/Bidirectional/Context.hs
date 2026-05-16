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
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

data Context = Context
  { variableContext :: HM.HashMap String AbsSyntax.Type,
    exceptionContext :: Maybe AbsSyntax.Type,
    useSubTyping :: Bool,
    useAmbiguousTypeAsBottom :: Bool
  }
  deriving (Show)

emptyContext :: Context
emptyContext = Context {variableContext = HM.empty, exceptionContext = Nothing, useSubTyping = False, useAmbiguousTypeAsBottom = False}

lookupVar :: String -> Context -> Maybe AbsSyntax.Type
lookupVar varName context = HM.lookup varName (variableContext context)

insertVar :: String -> AbsSyntax.Type -> Context -> Context
insertVar varName varType context =
  Context
    { variableContext = HM.insert varName varType (variableContext context),
      exceptionContext = exceptionContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context
    }

lookupException :: Context -> Maybe AbsSyntax.Type
lookupException = exceptionContext

insertException :: AbsSyntax.Type -> Context -> Context
insertException exceptionType context =
  Context
    { variableContext = variableContext context,
      exceptionContext = Just exceptionType,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context
    }

lookupSubTyping :: Context -> Bool
lookupSubTyping = useSubTyping

enableSubTyping :: Context -> Context
enableSubTyping context =
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      useSubTyping = True,
      useAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom context
    }

lookupAmbiguousTypeAsBottom :: Context -> Bool
lookupAmbiguousTypeAsBottom = useAmbiguousTypeAsBottom

enableAmbiguousTypeAsBottom :: Context -> Context
enableAmbiguousTypeAsBottom context = 
    Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      useSubTyping = useSubTyping context,
      useAmbiguousTypeAsBottom = True
    }
