module Pinafore.Language.Predefined
    ( PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedBindings
    , predefinedPatternConstructors
    , predefinedDoc
    , outputLn
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Predefined.Base
import Pinafore.Language.Predefined.Defs
import Pinafore.Language.Predefined.File
import Pinafore.Language.Predefined.SpecialForms
import Pinafore.Language.Predefined.UI
import Pinafore.Language.Type
import Shapes

predefinitions :: DocTree BindDoc
predefinitions =
    MkDocTree "Predefined Bindings" "Entries in italics are supertypes of existing types, for convenience." $
    special_forms <> base_predefinitions <> ui_predefinitions <> file_predefinitions

predefinedDoc :: DocTree DefDoc
predefinedDoc = fmap bdDoc $ predefinitions

predefinedBindings :: (?pinafore :: PinaforeContext) => Map Name QValue
predefinedBindings =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             db <- bdBind doc
             val <- dbValue db
             return (dbName db, val ?pinafore)) $
    predefinitions

predefinedPatternConstructors :: Map Name PinaforePatternConstructor
predefinedPatternConstructors =
    mapFromList $
    catMaybes $
    toList $
    fmap
        (\doc -> do
             db <- bdBind doc
             pat <- dbPattern db
             return (dbName db, pat)) $
    predefinitions
