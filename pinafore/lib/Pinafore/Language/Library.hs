module Pinafore.Language.Library
    ( implicitScope
    , getLibraryScope
    , DefDoc(..)
    , DocTree
    , runDocTree
    , libraryDoc
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule]
library = [stdLibraryModule, debugLibraryModule]

docTreeScope :: (?pinafore :: PinaforeContext) => DocTree BindDoc -> PinaforeScope
docTreeScope dt = let
    bindDocBinding :: BindDoc -> Maybe (Name, PinaforeBinding)
    bindDocBinding doc = do
        (name, mb) <- bdBind doc
        b <- mb
        return (name, b ?pinafore)
    in bindingsScope $ mapFromList $ mapMaybe bindDocBinding $ toList dt

libraryDoc :: DocTree DefDoc
libraryDoc = MkDocTree "Library" "" $ fmap (TreeDocTreeEntry . fmap bdDoc) library

implicitScope :: (?pinafore :: PinaforeContext) => PinaforeScope
implicitScope = docTreeScope stdLibraryModule

libraryModuleMap :: (?pinafore :: PinaforeContext) => Map Text PinaforeScope
libraryModuleMap = mapFromList $ fmap (\lmod -> (docTreeName lmod, docTreeScope lmod)) library

getLibraryScope :: (?pinafore :: PinaforeContext) => ModuleName -> Maybe PinaforeScope
getLibraryScope mname = lookup (moduleNameText mname) libraryModuleMap
