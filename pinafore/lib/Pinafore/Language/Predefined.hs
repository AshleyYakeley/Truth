module Pinafore.Language.Predefined
    ( PinaforeContext
    , DefDoc(..)
    , DocTree(..)
    , runDocTree
    , predefinedScope
    , predefinedDoc
    , outputLn
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
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

predefinedScope :: (?pinafore :: PinaforeContext) => PinaforeScope
predefinedScope = docTreeScope predefinitions
