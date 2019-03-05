module Truth.UI.GTK.MenuBar
    ( menuBarGetView
    ) where

import Data.IORef
import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

attachMenuEntry :: IsMenuShell menushell => menushell -> MenuEntry edit -> CreateView sel edit ()
attachMenuEntry ms (ActionMenuEntry rlabel raction) = do
    aref <- liftIO $ newIORef Nothing
    item <- menuItemNew
    menuShellAppend ms item
    cvBindEditFunction rlabel $ \(label, _maccel) -> set item [#label := label]
    cvBindEditFunction raction $ \maction ->
        liftIO $ do
            writeIORef aref maction
            set item [#sensitive := isJust maction]
    _ <-
        on item #activate $ do
            maction <- readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
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

attachMenuEntries :: IsMenuShell menushell => menushell -> [MenuEntry edit] -> CreateView sel edit ()
attachMenuEntries menu mm = for_ mm $ attachMenuEntry menu

createWidget :: [MenuEntry edit] -> CreateView sel edit Widget
createWidget menu = do
    widget <- menuBarNew
    attachMenuEntries widget menu
    toWidget widget

menuBarGetView :: GetGView
menuBarGetView =
    MkGetView $ \_ uispec -> do
        MkMenuBarUISpec menu <- isUISpec uispec
        return $ createWidget menu
