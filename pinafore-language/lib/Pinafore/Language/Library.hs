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
import Pinafore.Language.DocTree
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

nameIsInfix :: Name -> Bool
nameIsInfix n =
    case unpack n of
        (c:_)
            | isAlpha c -> False
        "[]" -> False
        _ -> True

allOperatorNames :: [Name]
allOperatorNames = let
    getDocName :: BindDoc -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = BindScopeEntry name _}
        | nameIsInfix name = Just name
    getDocName _ = Nothing
    in catMaybes $ fmap getDocName $ mconcat $ fmap toList library

getImplictScope :: (?pinafore :: PinaforeContext) => IO PinaforeScope
getImplictScope = getModuleScope stdLibraryModule

data LibraryContext = MkLibraryContext
    { lcImplictScope :: PinaforeScope
    , lcLoadModule :: ModuleName -> PinaforeInterpreter (Maybe PinaforeScope)
    }

mkLibraryContext :: (?pinafore :: PinaforeContext) => FetchModule -> IO LibraryContext
mkLibraryContext fetchModule = do
    lcImplictScope <- getImplictScope
    let lcLoadModule = runFetchModule (libraryFetchModule library <> fetchModule) lcImplictScope
    return MkLibraryContext {..}
