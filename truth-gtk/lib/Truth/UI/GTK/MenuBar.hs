module Truth.UI.GTK.MenuBar
    ( menuBarGetView
    ) where

import Data.IORef
import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.Debug.Object

toModifierType :: KeyboardModifier -> ModifierType
toModifierType KMShift = ModifierTypeShiftMask
toModifierType KMCtrl = ModifierTypeControlMask
toModifierType KMAlt = ModifierTypeMod1Mask

attachMenuEntry :: IsMenuShell menushell => menushell -> MenuEntry edit -> CreateView sel edit ()
attachMenuEntry ms (ActionMenuEntry rlabel raction) = do
    aref <- liftIO $ newIORef Nothing
    item <- menuItemNew
    menuShellAppend ms item
    cvBindEditFunction rlabel $ \(label, maccel) ->
        traceBracket "GTK.MenuBar:label" $
        liftIO $ do
            set item [#label := label] -- creates child if not present
            mc <- binGetChild item
            for_ mc $ \c -> do
                ml <- castTo AccelLabel c
                for_ ml $ \l -> do
                    case maccel of
                        Nothing -> traceBracket "GTK.MenuBar:accel:empty" $ accelLabelSetAccel l 0 []
                        Just (MkMenuAccelerator mods key) ->
                            traceBracket "GTK.MenuBar:accel:key" $ accelLabelSetAccel l (fromIntegral $ ord key) $ fmap toModifierType mods
    cvBindEditFunction raction $ \maction ->
        liftIO $ do
            writeIORef aref maction
            set item [#sensitive := isJust maction]
    _ <-
        on item #activate $ traceBracket "GTK.MenuBar:activate" $ do
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
