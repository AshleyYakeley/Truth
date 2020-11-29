module Pinafore.Language.Library
    ( predefinedScope
    , stdLibraryScope
    , DefDoc(..)
    , DocTree
    , runDocTree
    , predefinedDoc
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Interpret
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Predefined
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

docTreeScope :: (?pinafore :: PinaforeContext) => DocTree BindDoc -> PinaforeScope
docTreeScope dt = let
    bindDocBinding :: BindDoc -> Maybe (Name, PinaforeBinding)
    bindDocBinding doc = do
        (name, mb) <- bdBind doc
        b <- mb
        return (name, b ?pinafore)
    in bindingsScope $ mapFromList $ mapMaybe bindDocBinding $ toList dt

predefinedDoc :: DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions

predefinedScope :: (?pinafore :: PinaforeContext) => PinaforeScope
predefinedScope = docTreeScope predefinitions

stdLibraryScope :: (?pinafore :: PinaforeContext) => ModuleName -> Maybe PinaforeScope
stdLibraryScope (MkModuleName ("Debug" :| [])) = Just $ docTreeScope debugDocModule
stdLibraryScope _ = Nothing
