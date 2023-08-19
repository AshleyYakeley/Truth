module Changes.World.GNOME.GTK.Element.MenuBar
    ( KeyboardModifier(..)
    , KeyboardKey
    , MenuAccelerator(..)
    , MenuEntry(..)
    , simpleActionMenuEntry
    , MenuBar
    , createMenuBar
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import Data.IORef
import GI.Gdk
import GI.Gtk hiding (MenuBar)
import Shapes

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
                      (Model (ROWUpdate (Maybe (GView 'Locked ()))))
    | SubMenuEntry Text
                   [MenuEntry]

type MenuBar = [MenuEntry]

simpleActionMenuEntry :: Text -> Maybe MenuAccelerator -> GView 'Locked () -> MenuEntry
simpleActionMenuEntry label maccel action = ActionMenuEntry label maccel $ constantModel $ Just action

toModifierType :: KeyboardModifier -> ModifierType
toModifierType KMShift = ModifierTypeShiftMask
toModifierType KMCtrl = ModifierTypeControlMask
toModifierType KMAlt = ModifierTypeMod1Mask

accelGroupConnection ::
       IsAccelGroup ag => ag -> Word32 -> [ModifierType] -> [AccelFlags] -> GView 'Locked () -> GView 'Locked ()
accelGroupConnection ag key mods flags action = do
    closure <-
        liftIOWithUnlift $ \unlift ->
            genClosure_AccelGroupActivate $ \_ _ _ _ -> do
                unlift action
                return True
    accelGroupConnect ag key mods flags closure
    gvOnClose $ do
        _ <- gvLiftIO $ accelGroupDisconnect ag $ Just closure
        return ()

attachMenuEntry :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> MenuEntry -> GView 'Unlocked ()
attachMenuEntry ag ms (ActionMenuEntry label maccel raction) = do
    aref <- gvLiftIONoUI $ newIORef Nothing
    item <-
        gvRunLocked $ do
            item <- menuItemNew
            menuShellAppend ms item
            let
                meaction :: GView 'Locked ()
                meaction = do
                    maction <- liftIO $ readIORef aref
                    case maction of
                        Nothing -> return ()
                        Just action -> action
            set item [#label := label] -- creates child if not present
            mc <- binGetChild item
            for_ mc $ \c -> do
                ml <- liftIO $ castTo AccelLabel c
                for_ ml $ \l -> do
                    case maccel of
                        Nothing -> liftIO $ accelLabelSetAccel l 0 []
                        Just (MkMenuAccelerator mods key) -> do
                            let
                                keyw :: Word32
                                keyw = fromIntegral $ ord key
                                gmods :: [ModifierType]
                                gmods = fmap toModifierType mods
                            liftIO $ accelLabelSetAccel l keyw gmods
                            accelGroupConnection ag keyw gmods [AccelFlagsVisible] meaction
            _ <- gvOnSignal item #activate meaction
            return item
    gvBindReadOnlyWholeModel raction $ \maction -> do
        gvLiftIONoUI $ writeIORef aref maction
        gvRunLocked $ set item [#sensitive := isJust maction]
attachMenuEntry ag ms (SubMenuEntry name entries) = do
    menu <-
        gvRunLocked $ do
            item <- menuItemNewWithLabel name
            menuShellAppend ms item
            menu <- menuNew
            menuItemSetSubmenu item $ Just menu
            return menu
    attachMenuEntries ag menu entries
attachMenuEntry _ ms SeparatorMenuEntry =
    gvRunLocked $ do
        item <- gvNew SeparatorMenuItem []
        menuShellAppend ms item

attachMenuEntries :: (IsMenuShell menushell, IsAccelGroup ag) => ag -> menushell -> [MenuEntry] -> GView 'Unlocked ()
attachMenuEntries ag menu mm = for_ mm $ attachMenuEntry ag menu

createMenuBar :: IsAccelGroup ag => ag -> MenuBar -> GView 'Unlocked Widget
createMenuBar ag menu = do
    (mbar, widget) <-
        gvRunLocked $ do
            mbar <- menuBarNew
            widget <- toWidget mbar
            return (mbar, widget)
    attachMenuEntries ag mbar menu
    return widget
