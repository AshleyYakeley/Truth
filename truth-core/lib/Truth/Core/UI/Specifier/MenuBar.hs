module Truth.Core.UI.Specifier.MenuBar where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types

data KeyboardModifier
    = KMShift
    | KMCtrl
    | KMAlt

type KeyboardKey = Char

data MenuAccelerator =
    MkMenuAccelerator [KeyboardModifier]
                      KeyboardKey

data MenuEntry update
    = SeparatorMenuEntry
    | ActionMenuEntry Text
                      (Maybe MenuAccelerator)
                      (UpdateFunction update (WholeUpdate (Maybe (IO ()))))
    | SubMenuEntry Text
                   [MenuEntry update]

type MenuBar update = [MenuEntry update]

mapMenuEntry :: UpdateFunction updateb updatea -> MenuEntry updatea -> MenuEntry updateb
mapMenuEntry _ SeparatorMenuEntry = SeparatorMenuEntry
mapMenuEntry ef (ActionMenuEntry label maccel efaction) = ActionMenuEntry label maccel (efaction . ef)
mapMenuEntry ef (SubMenuEntry name entries) = SubMenuEntry name $ fmap (mapMenuEntry ef) entries

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> IO () -> MenuEntry update
simpleActionMenuItem label maccel action = ActionMenuEntry label maccel $ constUpdateFunction $ Just action
