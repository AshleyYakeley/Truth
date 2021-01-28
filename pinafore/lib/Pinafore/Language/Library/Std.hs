module Pinafore.Language.Library.Std
    ( stdLibraryModule
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Base
import Pinafore.Language.Library.Std.File
import Pinafore.Language.Library.Std.UI
import Shapes

stdLibraryModule :: LibraryModule
stdLibraryModule =
    MkDocTree
        "Std"
        "The standard library, implicitly impoted. Entries in italics are supertypes of existing types, for convenience." $
    base_stdLibraryModule <> ui_stdLibraryModule <> file_stdLibraryModule
