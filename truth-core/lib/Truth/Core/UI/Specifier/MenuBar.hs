module Truth.Core.UI.Specifier.MenuBar where

import Truth.Core.Import
import Truth.Core.UI.Specifier.Specifier

data MenuEntry
    = SeparatorMenuEntry
    | ActionMenuEntry Text
                      (Maybe Char)
                      (IO ())
    | SubMenuEntry Text
                   [MenuEntry]

data MenuBarUISpec sel edit where
    MkMenuBarUISpec :: [MenuEntry] -> MenuBarUISpec sel tedit

instance Show (MenuBarUISpec sel edit) where
    show _ = "menu-bar"

instance UIType MenuBarUISpec where
    uiWitness = $(iowitness [t|MenuBarUISpec|])

menuBarUISpec :: [MenuEntry] -> UISpec sel tedit
menuBarUISpec menu = MkUISpec $ MkMenuBarUISpec menu
