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
    | ActionMenuEntry (EditFunction edit (WholeEdit (Text, Maybe MenuAccelerator)))
                      (EditFunction edit (WholeEdit (Maybe (IO ()))))
    | SubMenuEntry Text
                   [MenuEntry edit]

type MenuBar edit = [MenuEntry edit]

mapMenuEntry :: EditFunction editb edita -> MenuEntry edita -> MenuEntry editb
mapMenuEntry _ SeparatorMenuEntry = SeparatorMenuEntry
mapMenuEntry ef (ActionMenuEntry eflabel efaction) = ActionMenuEntry (eflabel . ef) (efaction . ef)
mapMenuEntry ef (SubMenuEntry name entries) = SubMenuEntry name $ fmap (mapMenuEntry ef) entries

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> IO () -> MenuEntry edit
simpleActionMenuItem t ma action = ActionMenuEntry (constEditFunction (t, ma)) $ constEditFunction $ Just action
