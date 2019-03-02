module Truth.UI.GTK.MenuBar
    ( menuBarGetView
    ) where

import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.Debug.Object

attachMenuEntry :: IsMenuShell menushell => menushell -> MenuEntry -> IO ()
attachMenuEntry ms (ActionMenuEntry name mchar action) = do
    item <- menuItemNewWithLabel name
    menuShellAppend ms item
    _ <- on item #activate $ traceBracket ("GTK.MenuBar:activate " <> unpack name) action
    case mchar of
        Nothing -> return ()
        Just _ -> fail "GTK menu accel not supported"
    return ()
attachMenuEntry ms (SubMenuEntry name entries) = do
    item <- menuItemNewWithLabel name
    menuShellAppend ms item
    menu <- menuNew
    menuItemSetSubmenu item $ Just menu
    attachMenuEntries menu entries
attachMenuEntry ms SeparatorMenuEntry = do
    item <- new SeparatorMenuItem []
    menuShellAppend ms item

attachMenuEntries :: IsMenuShell menushell => menushell -> [MenuEntry] -> IO ()
attachMenuEntries menu mm = for_ mm $ attachMenuEntry menu

createWidget :: [MenuEntry] -> CreateView sel edit Widget
createWidget menu = do
    widget <- menuBarNew
    liftIO $ attachMenuEntries widget menu
    toWidget widget

menuBarGetView :: GetGView
menuBarGetView =
    MkGetView $ \_ uispec -> do
        MkMenuBarUISpec menu <- isUISpec uispec
        return $ createWidget menu
