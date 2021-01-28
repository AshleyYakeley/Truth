module Pinafore.Library.Std
    ( stdLibraryModule
    ) where

import Pinafore.Language.DocTree
import Pinafore.Library.Defs
import Pinafore.Library.Std.Base
import Pinafore.Library.Std.File
import Pinafore.Library.Std.SpecialForms
import Pinafore.Library.Std.UI
import Shapes

stdLibraryModule :: LibraryModule
stdLibraryModule =
    MkDocTree
        "Std"
        "The standard library, implicitly impoted. Entries in italics are supertypes of existing types, for convenience." $
    special_forms <> base_stdLibraryModule <> ui_stdLibraryModule <> file_stdLibraryModule
