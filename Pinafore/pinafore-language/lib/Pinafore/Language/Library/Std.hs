module Pinafore.Language.Library.Std
    ( stdLibraryModule
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Actions
import Pinafore.Language.Library.Std.Base
import Pinafore.Language.Library.Std.Lifecycle
import Pinafore.Language.Library.Std.Model
import Shapes

stdLibraryModule :: LibraryModule context
stdLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Std"
        "The standard library, implicitly imported. Entries in italics are supertypes of existing types, for convenience." $
    mconcat [baseLibEntries, actionsLibEntries, lifecycleLibEntries, modelLibEntries]
