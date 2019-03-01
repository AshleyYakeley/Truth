module Pinafore.Language.Window where

import Truth.Core

data PinaforeWindow = MkPinaforeWindow
    { pwWindow :: UIWindow
    , pwUndoActions :: UndoActions
    }
