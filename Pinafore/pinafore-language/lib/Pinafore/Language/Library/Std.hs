module Pinafore.Language.Library.Std
    ( stdLibraryModule
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Actions
import Pinafore.Language.Library.Std.Base
import Pinafore.Language.Library.Std.File
import Pinafore.Language.Library.Std.Interpreter
import Pinafore.Language.Library.Std.Lifecycle
import Pinafore.Language.Library.Std.Model
import Pinafore.Language.Library.Std.Undo
import Shapes

stdLibraryModule :: LibraryModule
stdLibraryModule =
    MkDocTree
        "Std"
        "The standard library, implicitly imported. Entries in italics are supertypes of existing types, for convenience." $
    mconcat
        [ baseLibEntries
        , actionsLibEntries
        , lifecycleLibEntries
        , interpreterLibEntries
        , undoLibEntries
        , modelLibEntries
        , fileLibEntries
        ]
