module Pinafore.Language.Library
    ( DefDoc(..)
    , LibraryModule
    , FetchModule
    , directoryFetchModule
    , textFetchModule
    , libraryFetchModule
    , LibraryContext(..)
    , mkLibraryContext
    , nameIsInfix
    , allOperatorNames
    ) where

import Pinafore.Context
import Pinafore.Language.DefDoc
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Eval
import Pinafore.Language.Library.FetchModule
import Pinafore.Language.Library.Std
import Pinafore.Language.Library.Storage
import Pinafore.Language.Library.Stream
import Pinafore.Language.Library.Task
import Pinafore.Language.Library.Undo
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule InvocationInfo]
library =
    pure $
    MkLibraryModule builtInModuleName $
    headingBDT
        "Built-In"
        ""
        [generalStuff, taskStuff, streamStuff, storageStuff, undoStuff, envStuff, evalStuff, debugStuff]

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: forall context. BindDoc context -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = Just (BindScopeEntry (UnqualifiedFullNameRef name) _), bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in mapMaybe getDocName $ mconcat $ fmap libraryModuleEntries library

data LibraryContext = MkLibraryContext
    { lcLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    }

mkLibraryContext :: InvocationInfo -> FetchModule InvocationInfo -> LibraryContext
mkLibraryContext context fetchModule =
    MkLibraryContext $ runFetchModule (libraryFetchModule library <> fetchModule) context
