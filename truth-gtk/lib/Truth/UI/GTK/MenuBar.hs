module Truth.UI.GTK.MenuBar
    ( createMenuBar
    ) where

import Data.IORef
import GI.Gdk
import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.Useful
import Truth.Debug.Reference

toModifierType :: KeyboardModifier -> ModifierType
toModifierType KMShift = ModifierTypeShiftMask
toModifierType KMCtrl = ModifierTypeControlMask
toModifierType KMAlt = ModifierTypeMod1Mask

accelGroupConnection :: IsAccelGroup ag => ag -> Word32 -> [ModifierType] -> [AccelFlags] -> IO () -> LifeCycleIO ()
accelGroupConnection ag key mods flags action = do
    closure <-
        genClosure_AccelGroupActivate $ \_ _ _ _ -> do
            action
            return True
    accelGroupConnect ag key mods flags closure
    lifeCycleClose $ do
        _ <- accelGroupDisconnect ag $ Just closure
        return ()

attachMenuEntry :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> MenuEntry -> CreateView ()
attachMenuEntry ag ms (ActionMenuEntry label maccel raction) = do
    aref <- liftIO $ newIORef Nothing
    item <- menuItemNew
    menuShellAppend ms item
    let
        meaction :: View ()
        meaction = do
            maction <- liftIO $ readIORef aref
            case maction of
                Nothing -> return ()
                Just action -> action
    set item [#label := label] -- creates child if not present
    mc <- binGetChild item
    unliftView <- cvLiftView askUnliftIO
    for_ mc $ \c -> do
        ml <- liftIO $ castTo AccelLabel c
        for_ ml $ \l -> do
            case maccel of
                Nothing -> traceBracket "GTK.MenuBar:accel:empty" $ accelLabelSetAccel l 0 []
                Just (MkMenuAccelerator mods key) -> traceBracket "GTK.MenuBar:accel:key" $ do
                    let
                        keyw :: Word32
                        keyw = fromIntegral $ ord key
                        gmods :: [ModifierType]
                        gmods = fmap toModifierType mods
                    accelLabelSetAccel l keyw gmods
                    liftLifeCycleIO $
                        accelGroupConnection ag keyw gmods [AccelFlagsVisible] $ traceBracketIO "THREAD: accel" $ runWMFunction unliftView meaction
    cvBindReadOnlyWholeModel raction $ \maction ->
        liftIO $ do
            writeIORef aref maction
            set item [#sensitive := isJust maction]
    _ <- cvOn item #activate $ traceBracket "GTK.MenuBar:activate" meaction
    return ()
attachMenuEntry ag ms (SubMenuEntry name entries) = do
    item <- menuItemNewWithLabel name
    menuShellAppend ms item
    menu <- menuNew
    menuItemSetSubmenu item $ Just menu
    attachMenuEntries ag menu entries
attachMenuEntry _ ms SeparatorMenuEntry = do
    item <- new SeparatorMenuItem []
    menuShellAppend ms item

attachMenuEntries :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> [MenuEntry] -> CreateView ()
attachMenuEntries ag menu mm = for_ mm $ attachMenuEntry ag menu

createMenuBar :: IsAccelGroup ag => ag -> Truth.Core.MenuBar -> CreateView Gtk.MenuBar
createMenuBar ag menu = do
    mbar <- menuBarNew
    attachMenuEntries ag mbar menu
    return mbar
