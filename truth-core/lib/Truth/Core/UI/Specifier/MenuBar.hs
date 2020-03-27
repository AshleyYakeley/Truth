module Truth.Core.UI.Specifier.MenuBar where

import Truth.Core.Import
import Truth.Core.Reference
import Truth.Core.Types
import Truth.Core.UI.View.View

data KeyboardModifier
    = KMShift
    | KMCtrl
    | KMAlt

type KeyboardKey = Char

data MenuAccelerator =
    MkMenuAccelerator [KeyboardModifier]
                      KeyboardKey

data MenuEntry
    = SeparatorMenuEntry
    | ActionMenuEntry Text
                      (Maybe MenuAccelerator)
                      (Model (ROWUpdate (Maybe (View ()))))
    | SubMenuEntry Text
                   [MenuEntry]

type MenuBar = [MenuEntry]

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> View () -> MenuEntry
simpleActionMenuItem label maccel action = ActionMenuEntry label maccel $ constantModel $ Just action
