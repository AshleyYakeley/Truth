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
    , allOperatorNames
    ) where

import Pinafore.Context
import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.ExprShow
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.FetchModule
import Pinafore.Language.Library.Std
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule]
library = [stdLibraryModule, debugLibraryModule]

libraryDoc :: [LibraryModule] -> [DocTree DefDoc]
libraryDoc extralib = fmap (fmap bdDoc) $ library <> extralib

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: BindDoc -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = BindScopeEntry name _, bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in mapMaybe getDocName $ mconcat $ fmap toList library

getImplictScope :: (?pinafore :: PinaforeContext) => IO PinaforeScope
getImplictScope = fmap moduleScope $ getLibraryModuleModule stdLibraryModule

data LibraryContext = MkLibraryContext
    { lcImplictScope :: PinaforeScope
    , lcLoadModule :: ModuleName -> PinaforeInterpreter (Maybe PinaforeModule)
    }

mkLibraryContext :: (?pinafore :: PinaforeContext) => FetchModule -> IO LibraryContext
mkLibraryContext fetchModule = do
    lcImplictScope <- getImplictScope
    let lcLoadModule = runFetchModule (libraryFetchModule library <> fetchModule) lcImplictScope
    return MkLibraryContext {..}
