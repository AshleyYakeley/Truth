module Pinafore.Language.Library.Std.Undo
    ( undoLibEntries
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Shapes

undoLibEntries :: [DocTreeEntry BindDoc]
undoLibEntries =
    [ docTreeEntry
          "Undo"
          "Undo and redo changes."
          [ mkValEntry "queueUndo" "Undo an action." $ do
                uh <- actionUndoHandler
                rc <- actionResourceContext
                liftIO $ undoHandlerUndo uh rc noEditSource
          , mkValEntry "queueRedo" "Redo an action." $ do
                uh <- actionUndoHandler
                rc <- actionResourceContext
                liftIO $ undoHandlerRedo uh rc noEditSource
          ]
    ]
