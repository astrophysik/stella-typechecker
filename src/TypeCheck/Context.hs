module TypeCheck.Context
  ( Context,
    emptyContext,
    lookupVar,
    insertVar,
    lookupException,
    insertException,
    enableSubTyping,
    isSubTypingEnabled,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Parsing.AbsSyntax as AbsSyntax

data Context = Context
  { variableContext :: HM.HashMap String AbsSyntax.Type,
    exceptionContext :: Maybe AbsSyntax.Type,
    useSubTyping :: Bool
  }
  deriving (Show)

emptyContext :: Context
emptyContext = Context {variableContext = HM.empty, exceptionContext = Nothing, useSubTyping = False}

lookupVar :: String -> Context -> Maybe AbsSyntax.Type
lookupVar varName context = HM.lookup varName (variableContext context)

insertVar :: String -> AbsSyntax.Type -> Context -> Context
insertVar varName varType context =
  Context
    { variableContext = HM.insert varName varType (variableContext context),
      exceptionContext = exceptionContext context,
      useSubTyping = useSubTyping context
    }

lookupException :: Context -> Maybe AbsSyntax.Type
lookupException = exceptionContext

insertException :: AbsSyntax.Type -> Context -> Context
insertException exceptionType context =
  Context
    { variableContext = variableContext context,
      exceptionContext = Just exceptionType,
      useSubTyping = useSubTyping context
    }

isSubTypingEnabled :: Context -> Bool
isSubTypingEnabled = useSubTyping

enableSubTyping :: Context -> Context
enableSubTyping context =
  Context
    { variableContext = variableContext context,
      exceptionContext = exceptionContext context,
      useSubTyping = True
    }
