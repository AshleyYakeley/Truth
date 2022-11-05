module Pinafore.Language.Library
    ( DefDoc(..)
    , DocTree
    , runDocTree
    , libraryDoc
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
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Env
import Pinafore.Language.Library.Eval
import Pinafore.Language.Library.FetchModule
import Pinafore.Language.Library.Std
import Pinafore.Language.Library.Stream
import Pinafore.Language.Library.Task
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule]
library =
    pure $
    MkLibraryModule builtInModuleName $
    MkDocTree "Built-In" "" [generalStuff, taskStuff, streamStuff, envStuff, evalStuff, debugStuff]

libraryDoc :: [LibraryModule] -> [(ModuleName, DocTree DefDoc)]
libraryDoc extralib = fmap (\MkLibraryModule {..} -> (lmName, fmap bdDoc lmContents)) $ library <> extralib

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: BindDoc -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = BindScopeEntry (RootFullName name) _, bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in mapMaybe getDocName $ mconcat $ fmap (toList . lmContents) library

data LibraryContext = MkLibraryContext
    { lcLoadModule :: ModuleName -> QInterpreter (Maybe QModule)
    }

mkLibraryContext :: (?qcontext :: QContext) => FetchModule -> LibraryContext
mkLibraryContext fetchModule = MkLibraryContext $ runFetchModule $ libraryFetchModule library <> fetchModule
