module Pinafore.Language.Library.Predefined
    ( predefinitions
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Predefined.Base
import Pinafore.Language.Library.Predefined.File
import Pinafore.Language.Library.Predefined.SpecialForms
import Pinafore.Language.Library.Predefined.UI
import Shapes

predefinitions :: DocTree BindDoc
predefinitions =
    MkDocTree "Predefined Bindings" "Entries in italics are supertypes of existing types, for convenience." $
    special_forms <> base_predefinitions <> ui_predefinitions <> file_predefinitions
