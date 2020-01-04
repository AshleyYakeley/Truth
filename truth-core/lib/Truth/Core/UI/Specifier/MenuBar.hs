module Truth.Core.UI.Specifier.MenuBar where

import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Resource
import Truth.Core.Types

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
                      (ReadOnlyOpenSubscriber (WholeUpdate (Maybe (IO ()))))
    | SubMenuEntry Text
                   [MenuEntry]

type MenuBar = [MenuEntry]

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> IO () -> MenuEntry
simpleActionMenuItem label maccel action =
    ActionMenuEntry label maccel $ openResource $ constantSubscriber $ Just action
