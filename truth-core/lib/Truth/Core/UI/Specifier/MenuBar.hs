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

data MenuEntry edit
    = SeparatorMenuEntry
    | ActionMenuEntry Text
                      (Maybe MenuAccelerator)
                      (EditFunction edit (WholeEdit (Maybe (IO ()))))
    | SubMenuEntry Text
                   [MenuEntry edit]

type MenuBar edit = [MenuEntry edit]

mapMenuEntry :: EditFunction editb edita -> MenuEntry edita -> MenuEntry editb
mapMenuEntry _ SeparatorMenuEntry = SeparatorMenuEntry
mapMenuEntry ef (ActionMenuEntry label maccel efaction) = ActionMenuEntry label maccel (efaction . ef)
mapMenuEntry ef (SubMenuEntry name entries) = SubMenuEntry name $ fmap (mapMenuEntry ef) entries

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> IO () -> MenuEntry edit
simpleActionMenuItem label maccel action = ActionMenuEntry label maccel $ constEditFunction $ Just action
