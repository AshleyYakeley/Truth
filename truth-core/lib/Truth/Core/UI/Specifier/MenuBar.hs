module Truth.Core.UI.Specifier.MenuBar where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Specifier

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

simpleActionMenuItem :: Text -> Maybe MenuAccelerator -> IO () -> MenuEntry edit
simpleActionMenuItem t ma action = ActionMenuEntry (constEditFunction (t, ma)) $ constEditFunction $ Just action

data MenuBarUISpec sel edit where
    MkMenuBarUISpec :: [MenuEntry edit] -> MenuBarUISpec sel edit

instance Show (MenuBarUISpec sel edit) where
    show _ = "menu-bar"

instance UIType MenuBarUISpec where
    uiWitness = $(iowitness [t|MenuBarUISpec|])

menuBarUISpec :: [MenuEntry edit] -> UISpec sel edit
menuBarUISpec menu = MkUISpec $ MkMenuBarUISpec menu
