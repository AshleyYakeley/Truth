module Pinafore.Library
    ( implicitScope
    , stdLibraryScope
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Interpret
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Library.Debug
import Pinafore.Library.Defs
import Pinafore.Library.Std
import Shapes

libraryModules :: [LibraryModule]
libraryModules = [stdLibraryModule, debugLibraryModule]

docTreeScope :: (?pinafore :: PinaforeContext) => DocTree BindDoc -> PinaforeScope
docTreeScope dt = let
    bindDocBinding :: BindDoc -> Maybe (Name, PinaforeBinding)
    bindDocBinding doc = do
        (name, mb) <- bdBind doc
        b <- mb
        return (name, b ?pinafore)
    in bindingsScope $ mapFromList $ mapMaybe bindDocBinding $ toList dt

predefinedDoc :: DocTree DefDoc
predefinedDoc = MkDocTree "Built-In Modules" "" $ fmap (TreeDocTreeEntry . fmap bdDoc) libraryModules

implicitScope :: (?pinafore :: PinaforeContext) => PinaforeScope
implicitScope = docTreeScope stdLibraryModule

libraryModulesMap :: (?pinafore :: PinaforeContext) => Map Text PinaforeScope
libraryModulesMap = mapFromList $ fmap (\lmod -> (docTreeName lmod, docTreeScope lmod)) libraryModules

stdLibraryScope :: (?pinafore :: PinaforeContext) => ModuleName -> Maybe PinaforeScope
stdLibraryScope mname = lookup (moduleNameText mname) libraryModulesMap
