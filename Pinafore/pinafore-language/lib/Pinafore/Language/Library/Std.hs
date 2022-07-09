module Pinafore.Language.Library.Std
    ( semigroupSubtypeRelationEntry
    , monoidSubtypeRelationEntry
    , stdLibraryModule
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Actions
import Pinafore.Language.Library.Std.Base
import Pinafore.Language.Library.Std.File
import Pinafore.Language.Library.Std.Interpreter
import Pinafore.Language.Library.Std.Invocation
import Pinafore.Language.Library.Std.LifeCycle
import Pinafore.Language.Library.Std.Monoid
import Pinafore.Language.Library.Std.Reference
import Pinafore.Language.Library.Std.Tasks
import Pinafore.Language.Library.Std.Undo
import Shapes

stdLibraryModule :: LibraryModule
stdLibraryModule =
    MkDocTree
        "Std"
        "The standard library, implicitly imported. Entries in italics are supertypes of existing types, for convenience." $
    mconcat
        [ baseLibEntries
        , monoidLibEntries
        , actionsLibEntries
        , lifeCycleLibEntries
        , tasksLibEntries
        , interpreterLibEntries
        , invocationLibEntries
        , undoLibEntries
        , refLibEntries
        , fileLibEntries
        ]
